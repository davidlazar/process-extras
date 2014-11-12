-- | Generalized versions of the functions
-- 'System.Process.readProcess', and
-- 'System.Process.readProcessWithExitCode'.
{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables, TupleSections, TypeFamilies, UndecidableInstances #-}
module System.Process.ListLike.Read
    ( readCreateProcess,
      readCreateProcess',
      readInterleaved,
      readCreateProcessWithExitCode,
      readProcessWithExitCode,
      StdoutWrapper(..),
      readProcess
    ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Concurrent
import Control.Exception as E (SomeException, onException, catch, mask, throw)
import Control.Monad
import Data.ListLike (ListLike(..), ListLikeIO(..))
import Data.ListLike.Text.Text ()
import Data.ListLike.Text.TextLazy ()
import Data.Maybe (maybeToList)
import Data.Monoid (Monoid(mempty, mappend), (<>))
import GHC.IO.Exception (IOErrorType(OtherError, ResourceVanished), IOException(ioe_type))
import Prelude hiding (null, length, rem)
import System.Exit (ExitCode(ExitSuccess))
import System.IO hiding (hPutStr, hGetContents)
import qualified System.IO.Error as IO
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Process (CreateProcess(..), StdStream(CreatePipe, Inherit), proc,
                       createProcess, waitForProcess, terminateProcess)
import System.Process.ListLike.Classes (ListLikeLazyIO(..), ProcessOutput(..))
import System.Process.ListLike.Instances ()
import Utils (forkWait)

-- | Read the output of a process and use the argument functions to
-- convert it into a Monoid, preserving the order of appearance of the
-- different chunks of output from standard output and standard error.
readCreateProcess :: (ListLikeLazyIO a c, ProcessOutput a b) => CreateProcess -> a -> IO b
readCreateProcess  p input =
    readCreateProcess' (p {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }) input

readCreateProcess' :: (ListLikeLazyIO a c, ProcessOutput a b) => CreateProcess -> a -> IO b
readCreateProcess'  p input = mask $ \ restore -> do
  (Just inh, maybe_outh, maybe_errh, pid) <- createProcess p

  onException
    (restore $
     do -- Without unsafeIntereleaveIO the pid messsage gets stuck
        -- until some additional output arrives from the process.
        waitOut <- forkWait $ (<>) <$> pure (pidf pid)
                                   <*> unsafeInterleaveIO (readInterleaved (maybeToList (fmap (outf,) maybe_outh) <> maybeToList (fmap (errf,) maybe_errh))
                                                                           (codef <$> waitForProcess pid))
        writeInput inh input
        waitOut)
    (do terminateProcess pid
        hClose inh
        maybe (return ()) hClose maybe_outh
        maybe (return ()) hClose maybe_errh
        waitForProcess pid)

-- | Simultaneously read the output from several file handles, using
-- the associated functions to add them to a Monoid b in the order
-- they appear.  This closes each handle on EOF, because AFAIK it is
-- the only useful thing to do with a file handle that has reached
-- EOF.
readInterleaved :: forall a b c. (ListLikeLazyIO a c, ProcessOutput a b) =>
                   [(a -> b, Handle)] -> IO b -> IO b
readInterleaved pairs finish = newEmptyMVar >>= readInterleaved' pairs finish

readInterleaved' :: forall a b c. (ListLikeLazyIO a c, ProcessOutput a b) =>
                    [(a -> b, Handle)] -> IO b -> MVar (Either Handle b) -> IO b
readInterleaved' pairs finish res = do
  mapM_ (forkIO . uncurry readHandle) pairs
  takeChunks (length pairs)
    where
      -- Forked thread to read the input and send it to takeChunks via
      -- the MVar.
      readHandle :: (a -> b) -> Handle -> IO ()
      readHandle f h = do
        cs <- readChunks h
        -- If the type returned as stdout and stderr is lazy we need
        -- to force it here in the producer thread - I'm not exactly
        -- sure why.  And why is String lazy?
        -- when (lazy (undefined :: a)) (void cs)
        mapM_ (\ c -> putMVar res (Right (f c))) cs
        hClose h
        putMVar res (Left h)
      takeChunks :: Int -> IO b
      takeChunks 0 = finish
      takeChunks openCount = takeChunk >>= takeMore openCount
      takeMore :: Int -> Either Handle b -> IO b
      takeMore openCount (Left h) = hClose h >> takeChunks (openCount - 1)
      takeMore openCount (Right x) =
          do xs <- unsafeInterleaveIO $ takeChunks openCount
             return (x <> xs)
      takeChunk = takeMVar res `catch` (\ (e :: SomeException) -> return $ Right $ intf e)

-- | An implementation of 'System.Process.readProcessWithExitCode'
-- with a two generalizations: (1) The input and outputs can be any
-- instance of 'ListLikeLazyIO', and (2) The CreateProcess is passes an
-- argument, so you can use either 'System.Process.proc' or
-- 'System.Process.rawSystem' and you can modify its fields such as
-- 'System.Process.cwd' before the process starts
readCreateProcessWithExitCode :: ListLikeLazyIO a c =>
                                 CreateProcess       -- ^ process to run
                              -> a                   -- ^ standard input
                              -> IO (ExitCode, a, a) -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCode p input = readCreateProcess p input

-- | A version of 'System.Process.readProcessWithExitCode' that uses
-- any instance of 'ListLikeLazyIO' instead of 'String', implemented
-- using 'readCreateProcessWithExitCode'.
readProcessWithExitCode :: ListLikeLazyIO a c =>
                           FilePath            -- ^ command to run
                        -> [String]            -- ^ any arguments
                        -> a                   -- ^ standard input
                        -> IO (ExitCode, a, a) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args input = readCreateProcessWithExitCode (proc cmd args) input

-- | Implementation of 'System.Process.readProcess' that uses any
-- instance of 'ListLikeLazyIO' instead of 'String', implemented using
-- 'readCreateProcess'.  As with 'System.Process.readProcess', Stderr
-- goes directly to the console, only stdout is returned.  Also like
-- 'System.Process.readProcess', an IO error of type OtherError is
-- thrown when the result code is not ExitSuccess.
readProcess :: ListLikeLazyIO a c =>
               FilePath        -- ^ command to run
            -> [String]        -- ^ any arguments
            -> a               -- ^ standard input
            -> IO a            -- ^ stdout
readProcess cmd args input =
    unStdoutWrapper <$> readCreateProcess' ((proc cmd args) {std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit}) input

-- | For the 'readProcess' function, we need to wrap a newtype around
-- the output type so we can build a ProcessOutput instance for it.
-- Otherwise it would overlap everything.
newtype StdoutWrapper a = StdoutWrapper {unStdoutWrapper :: a}

instance Monoid a => Monoid (StdoutWrapper a) where
    mempty = StdoutWrapper mempty
    mappend (StdoutWrapper a) (StdoutWrapper b) = StdoutWrapper (a <> b)

instance (ListLikeLazyIO a c, Monoid a) => ProcessOutput a (StdoutWrapper a) where
    pidf _ = mempty
    codef ExitSuccess = mempty
    codef failure = throw $ IO.mkIOError OtherError ("Process exited with " ++ show failure) Nothing Nothing
    outf x = StdoutWrapper x
    errf _ = mempty
    intf e = throw e

-- | Write and flush process input, closing the handle when done.
-- Catch and ignore Resource Vanished exceptions, they just mean the
-- process exited before all of its output was read.
writeInput :: ListLikeLazyIO a c => Handle -> a -> IO ()
writeInput inh input = do
  (do unless (null input) (hPutStr inh input >> hFlush inh)
      hClose inh) `E.catch` resourceVanished (\ _ -> return ())

-- | Wrapper for a process that provides a handler for the
-- ResourceVanished exception.  This is frequently an exception we
-- wish to ignore, because many processes will deliberately exit
-- before they have read all of their input.
resourceVanished :: (IOError -> IO a) -> IOError -> IO a
resourceVanished epipe e = if ioe_type e == ResourceVanished then epipe e else ioError e
