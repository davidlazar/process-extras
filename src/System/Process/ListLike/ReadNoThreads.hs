{-# LANGUAGE MultiParamTypeClasses, PackageImports, ScopedTypeVariables #-}
{-# OPTIONS -Wwarn -Wall -fno-warn-name-shadowing -fno-warn-missing-signatures #-}

-- | This is an alternative version of the module
--   "System.Process.ListLike.Read" that doesn't use forkIO.  I don't
--   know of any advantages, I'm just including it because it is derived
--   from old code I had developed, it is kinda badass, and maybe
--   someone has a use for it.
--
--   Function to run a process and return a lazy list of chunks from
--   standard output, standard error, and at the end of the list an
--   object indicating the process result code.  If neither output
--   handle is ready for reading the process sleeps and tries again,
--   with the sleep intervals increasing from 8 microseconds to a
--   maximum of 0.1 seconds.
module System.Process.ListLike.ReadNoThreads
    ( ListLikeIOPlus(..)
    , readCreateProcess
    , readCreateProcessWithExitCode
    , readProcessWithExitCode
    ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Concurrent (threadDelay)
import Control.Exception (catch, mask, onException, try)
import Data.ListLike (ListLike(length, null), ListLikeIO(hGetNonBlocking))
import Data.Maybe (mapMaybe)
import Data.Monoid (Monoid(mempty), (<>), mconcat)
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified GHC.IO.Exception as E
import Prelude hiding (length, null)
import System.Exit (ExitCode)
import System.Process (ProcessHandle, CreateProcess(..), waitForProcess, proc, createProcess, StdStream(CreatePipe), terminateProcess)
import System.IO (Handle, hReady, hClose)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Process.ListLike.Classes (ProcessOutput(..), ListLikeLazyIO(..))
import System.Process.ListLike.Instances ()

-- For the ListLikeIOPlus instance
import qualified Data.ByteString.Lazy as L
import Data.Word (Word8)

-- | For the unthreaded implementation we need a more powerful
-- ListLikeLazyIO class.
class ListLikeLazyIO a c => ListLikeIOPlus a c where
    hPutNonBlocking :: Handle -> a -> IO a
    chunks :: a -> [a]

instance ListLikeIOPlus L.ByteString Word8 where
    hPutNonBlocking = L.hPutNonBlocking
    chunks = Prelude.map (L.fromChunks . (: [])) . L.toChunks

instance ListLikeIOPlus LT.Text Char where
    hPutNonBlocking h text = L.hPutNonBlocking h (encodeUtf8 text) >> return text
    chunks = map (LT.fromChunks . (: [])) . LT.toChunks

bufSize = 65536		-- maximum chunk size
uSecs = 8		-- minimum wait time, doubles each time nothing is ready
maxUSecs = 100000	-- maximum wait time (microseconds)

readCreateProcess :: forall a b c. (ListLikeIOPlus a c, ProcessOutput a b) => CreateProcess -> a -> IO b
readCreateProcess p input = mask $ \ restore -> do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (p {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe})

    -- setModes input hs

    -- The uses of unsafeInterleaveIO here and below are required to
    -- keep the output from blocking waiting for process exit.
    onException
      (restore $ (<>) <$> pure (pidf pid)
                      <*> (unsafeInterleaveIO $ elements pid (chunks input, Just inh, [(outf, outh), (errf, errh)], Nothing)))
      (do terminateProcess pid; hClose inh; hClose outh; hClose errh;
          waitForProcess pid)
    where
      elements :: ProcessHandle -> ([a], Maybe Handle, [(a -> b, Handle)], Maybe b) -> IO b
      -- EOF on both output descriptors, get exit code.  It can be
      -- argued that the list will always contain exactly one exit
      -- code if traversed to its end, because the only case of
      -- elements that does not recurse is the one that adds a Result,
      -- and nowhere else is a Result added.  However, the process
      -- doing the traversing might die before that end is reached.
      elements pid tuple@(_, _, [], elems) =
          do result <- try (waitForProcess pid)
             case result of
               Left exn -> (<>) <$> pure (maybe mempty id elems <> intf exn) <*> elements pid tuple
               Right code -> pure (maybe mempty id elems <> codef code)
      -- The available output has been processed, send input and read
      -- from the ready handles
      elements pid tuple@(_, _, _, Nothing) =
          do result <- try (ready uSecs tuple)
             case result of
               Left exn -> (<>) <$> pure (intf exn) <*> elements pid tuple
               Right tuple' -> elements pid tuple'
      -- Add some output to the result value
      elements pid (input, inh, pairs, Just elems) =
          (<>) <$> pure elems
               <*> (unsafeInterleaveIO $ elements pid (input, inh, pairs, Nothing))

-- A quick fix for the issue where hWaitForInput has actually started
-- raising the isEOFError exception in ghc 6.10.
data Readyness = Ready | Unready | EndOfFile deriving Eq

hReady' :: Handle -> IO Readyness
hReady' h = (hReady h >>= (\ flag -> return (if flag then Ready else Unready))) `catch` (\ (e :: IOError) ->
                                                                                             case E.ioe_type e of
                                                                                               E.EOF -> return EndOfFile
                                                                                               _ -> error (show e))

-- | Wait until at least one handle is ready and then write input or
-- read output.  Note that there is no way to check whether the input
-- handle is ready except to try to write to it and see if any bytes
-- are accepted.  If no input is accepted, or the input handle is
-- already closed, and none of the output descriptors are ready for
-- reading the function sleeps and tries again.
ready :: (ListLikeIOPlus a c, ProcessOutput a b) =>
         Int -> ([a], Maybe Handle, [(a -> b, Handle)], Maybe b)
      -> IO ([a], Maybe Handle, [(a -> b, Handle)], Maybe b)
ready waitUSecs (input, inh, pairs, elems) =
    do
      anyReady <- mapM (hReady' . snd) pairs
      case (input, inh, anyReady) of
        -- Input exhausted, close the input handle.
        ([], Just handle, _) | all (== Unready) anyReady ->
            do hClose handle
               ready  waitUSecs ([], Nothing, pairs, elems)
        -- Input handle closed and there are no ready output handles,
        -- wait a bit
        ([], Nothing, _) | all (== Unready) anyReady ->
            do threadDelay waitUSecs
               --ePut0 ("Slept " ++ show uSecs ++ " microseconds\n")
               ready (min maxUSecs (2 * waitUSecs)) (input, inh, pairs, elems)
        -- Input is available and there are no ready output handles
        (input : etc, Just handle, _)
            -- Discard a zero byte input
            | all (== Unready) anyReady && null input -> ready waitUSecs (etc, inh, pairs, elems)
            -- Send some input to the process
            | all (== Unready) anyReady ->
                do input' <- hPutNonBlocking handle input
                   case null input' of
                     -- Input buffer is full too, sleep.
                     True -> do threadDelay uSecs
                                ready (min maxUSecs (2 * waitUSecs)) (input : etc, inh, pairs, elems)
                     -- We wrote some input, discard it and continue
                     False -> return (input' : etc, Just handle, pairs, elems)
        -- One or both output handles are ready, try to read from them
        _ ->
            do allOutputs <- mapM (\ ((f, h), r) -> nextOut h r f) (zip pairs anyReady)
               let newPairs = mapMaybe (\ ((_, mh), (f, _)) -> maybe Nothing (\ h -> Just (f, h)) mh) (zip allOutputs pairs)
               return (input, inh, newPairs, elems <> mconcat (map fst allOutputs))

-- | Return the next output element and the updated handle
-- from a handle which is assumed ready.
nextOut :: (ListLikeIO a c, ProcessOutput a b) => Handle -> Readyness -> (a -> b) -> IO (Maybe b, Maybe Handle)
nextOut _ EndOfFile _ = return (Nothing, Nothing)	-- Handle is closed
nextOut handle Unready _ = return (Nothing, Just handle)	-- Handle is not ready
nextOut handle Ready constructor =	-- Perform a read
    do
      a <- hGetNonBlocking handle bufSize
      case length a of
        -- A zero length read, unlike a zero length write, always
        -- means EOF.
        0 -> do hClose handle
                return (Nothing, Nothing)
        -- Got some input
        _n -> return (Just (constructor a), Just handle)

readCreateProcessWithExitCode :: (ListLikeIOPlus a c) => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode p input = readCreateProcess p input

readProcessWithExitCode :: (ListLikeIOPlus a c) => FilePath -> [String] -> a -> IO (ExitCode, a, a)
readProcessWithExitCode cmd args input = readCreateProcessWithExitCode (proc cmd args) input
