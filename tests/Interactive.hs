{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving #-}
-- | Tests that require a user to send a keyboard interrupt.
module Main where

import Control.Exception (SomeException)
import qualified Data.Text.Lazy
import qualified Data.ByteString.Lazy
import Data.Monoid
import System.Process
import System.Process.Chunks (Chunk(..))
import System.Process.ListLike.Instances ()
import qualified System.Process.ListLike.ReadNoThreads as NoThreads
import qualified System.Process.ListLike.Read as Threads
import System.Environment
import System.IO
import Text.Read (readMaybe)

instance Show ProcessHandle where
    show _ = "<processhandle>"

instance Eq ProcessHandle where
    _ == _ = False

instance Eq SomeException where
    _ == _ = False

deriving instance Show a => Show (Chunk a)

deriving instance Eq a => Eq (Chunk a)

main :: IO ()
main = do
  args <- getArgs
  case readArgs args of
    Nothing ->
        hPutStrLn stderr $
          unlines ([ "usage: interactive-tests test# runner#"
                   , "tests: " ] ++
                   map (\ (n, test) -> " " ++ show n ++ ". " ++ test) (zip ([1..] :: [Int]) tests) ++
                   [ "runners: " ] ++
                   map (\ (n, (runner, _)) -> " " ++ show n ++ ". " ++ runner) (zip ([1..] :: [Int]) runners))
    Just (test, (s, runner)) ->
        do hPutStrLn stderr (test ++ " -> " ++ s)
           runner (shell test)
    where
      readArgs :: [String] -> Maybe (String, (String, CreateProcess -> IO ()))
      readArgs [t, r] =
          case (readMaybe t, readMaybe r) of
            (Just t', Just r')
                | t' >= 1 && t' <= length tests && r' >= 1 && r' <= length runners ->
                    Just (tests !! (t' - 1), runners !! (r' - 1))
            _ -> Nothing
      readArgs _ = Nothing

tests :: [String]
tests = [ "ls -l /tmp"
        , "yes | cat -n | while read i; do echo $i; sleep 1; done"
        , "oneko"
        , "yes | cat -n" ]

runners  :: [(String, CreateProcess -> IO ())]
runners = [ ("NoThreads.readCreateProcess Lazy.Text",
             \ p -> NoThreads.readCreateProcess p mempty >>= \ (b :: [Chunk Data.Text.Lazy.Text]) ->
                    mapM_ (hPutStrLn stderr . show) b)
          , ("NoThreads.readCreateProcess Lazy.ByteString",
             \ p -> NoThreads.readCreateProcess p mempty >>= \ (b :: [Chunk Data.ByteString.Lazy.ByteString]) ->
                    mapM_ (hPutStrLn stderr . show) b)
          , ("Threads.readCreateProcess Lazy.Text",
             \ p -> Threads.readCreateProcess p mempty >>= \ (b :: [Chunk Data.Text.Lazy.Text]) ->
                    mapM_ (hPutStrLn stderr . show) b)
          , ("Threads.readCreateProcess Lazy.ByteString",
             \ p -> Threads.readCreateProcess p mempty >>= \ (b :: [Chunk Data.ByteString.Lazy.ByteString]) ->
                    mapM_ (hPutStrLn stderr . show) b)
          ]
