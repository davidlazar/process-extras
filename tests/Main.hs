{-# LANGUAGE CPP, FlexibleInstances, OverloadedStrings, RankNTypes, ScopedTypeVariables, StandaloneDeriving #-}
module Main where

import Codec.Binary.UTF8.String (encode)
import Control.Applicative ((<$>))
import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, try)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Char (chr, ord)
import Data.List (isSuffixOf)
import Data.ListLike as ListLike (ListLike(length, concat, null, groupBy, head, toList, dropWhile, takeWhile, drop, length), ListLikeIO(readFile))
import Data.Maybe (mapMaybe)
import Data.Monoid (Monoid(..), (<>))
import Data.String (IsString(fromString))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Prelude hiding (length, readFile, head)
import GHC.IO.Exception
import System.Exit
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (getFileStatus, fileMode, setFileMode, unionFileModes, ownerExecuteMode, groupExecuteMode, otherExecuteMode)
import System.Process (CreateProcess, proc, ProcessHandle, shell, readProcessWithExitCode)
import System.Process.ByteString as B
import System.Process.ByteString.Lazy as LB
import System.Process.String as S
import System.Process.Text as T
import System.Process.Text.Lazy as LT
import System.Process.ListLike.Classes (ListLikeLazyIO, ProcessOutput)
import System.Process.ListLike.Instances
import System.Process.ListLike.StrictString () -- the lazy one is the same as lazy text, the strict one is more interesting to test
import System.Process.ListLike.Read as LL
import System.Timeout
import Test.HUnit hiding (path)
import Text.Regex.Posix ((=~))
import Text.Printf

instance Show ProcessHandle where
    show _ = "<processhandle>"

instance Eq ProcessHandle where
    _ == _ = False

instance Eq SomeException where
    _ == _ = False

instance Monoid Test where
    mempty = TestList []
    mappend (TestList a) (TestList b) = TestList (a <> b)
    mappend (TestList a) b = TestList (a <> [b])
    mappend a (TestList b) = TestList ([a] <> b)
    mappend a b = TestList [a, b]

type MkTest = forall a c. (Show a, ListLikeLazyIO a c, IsString a, Eq a, Enum c) => String -> a -> Test

testInstances :: MkTest -> Test
testInstances mkTest = mappend (testCharInstances mkTest) (testWord8Instances mkTest)

testStrictInstances :: MkTest -> Test
testStrictInstances mkTest = mappend (testStrictCharInstances mkTest) (testStrictWord8Instances mkTest)

testLazyInstances :: MkTest -> Test
testLazyInstances mkTest = mappend (testLazyCharInstances mkTest) (testLazyWord8Instances mkTest)

testCharInstances :: MkTest -> Test
testCharInstances mkTest = mappend (testLazyCharInstances mkTest) (testStrictCharInstances mkTest)

testLazyCharInstances :: MkTest -> Test
testLazyCharInstances mkTest = mkTest "Lazy Text" LT.empty

testStrictCharInstances :: MkTest -> Test
testStrictCharInstances mkTest = mappend (mkTest "Strict Text" T.empty) (mkTest "String" ("" :: String))

testWord8Instances :: MkTest -> Test
testWord8Instances mkTest = mappend (testLazyWord8Instances mkTest) (testStrictWord8Instances mkTest)

testLazyWord8Instances :: MkTest -> Test
testLazyWord8Instances mkTest = mkTest "Lazy ByteString" LB.empty

testStrictWord8Instances :: MkTest -> Test
testStrictWord8Instances mkTest = mkTest "Strict ByteString" B.empty

main :: IO ()
main =
    do chmod "tests/script1.hs"
       chmod "tests/script4.hs"
       (c,st) <- runTestText putTextToShowS test1 -- (TestList (versionTests ++ sourcesListTests ++ dependencyTests ++ changesTests))
       putStrLn (st "")
       case (failures c) + (errors c) of
         0 -> return ()
         _ -> exitFailure

chmod :: FilePath -> IO ()
chmod path =
    getFileStatus path >>= \ status ->
    setFileMode path (foldr unionFileModes (fileMode status) [ownerExecuteMode, groupExecuteMode, otherExecuteMode])

cps :: [CreateProcess]
cps = [ proc "true" []
      , proc "false" []
      , shell "foo"
      , proc "foo" []
      , shell "yes | cat -n | head 100"
      , shell "yes | cat -n"
      , proc "cat" ["tests/text"]
      , proc "cat" ["tests/houseisclean.jpg"]
      , proc "tests/script1.hs" []
      ]

test1 :: Test
test1 =
    TestLabel "test1"
      (TestList
       [ TestLabel "[Output]" $
         TestList
         [ testInstances
           (\ s i -> TestLabel s $ TestCase $ do -- We can use a string for the input because the process I/O types
                       -- are instances of IsString.
                       result <- LL.readCreateProcessWithExitCode (proc "tests/script4.hs" []) ("a" <> i)
                       assertEqual "file closed 1" (ExitSuccess, "a", "Read one character: 'a'\n") result)
         , testInstances
           (\ s i -> TestLabel s $ TestCase $ do
                       result <- LL.readCreateProcessWithExitCode (proc "tests/script4.hs" []) i
                       assertEqual "file closed 2" (ExitFailure 1, "", "script4.hs: <stdin>: hGetChar: end of file\n") result)
         , testInstances
           (\ s i -> TestLabel s $ TestCase $ do
                       result <- LL.readCreateProcessWithExitCode (proc "tests/script4.hs" []) ("abcde" <> i)
                       assertEqual "file closed 3" (ExitSuccess, "a", "Read one character: 'a'\n") result)
         , testCharInstances
           (\ s i -> TestLabel s $ TestCase $ do
                       b <- LL.readCreateProcessWithExitCode (proc "cat" ["tests/text"]) i
                       assertEqual
                         "Unicode"
                         (ExitSuccess,
                          -- For Text, assuming your locale is set to utf8, the result is unicode.
                          "Signed: Baishi laoren \30333\30707\32769\20154, painted in the artist\8217s 87th year.\n",
                          "")
                         b)
         , testWord8Instances
           (\ s i -> TestLabel s $ TestCase $ do
                       b <- LL.readCreateProcessWithExitCode (proc "cat" ["tests/text"]) i
                       assertEqual
                         "UTF8"
                         (ExitSuccess,
                          -- For ByteString we get utf8 encoded text
                          "Signed: Baishi laoren \231\153\189\231\159\179\232\128\129\228\186\186, painted in the artist\226\128\153s 87th year.\n",
                          "")
                         b)
         , testInstances
           (\ s i -> TestLabel s $ TestCase $ do
                       b <- LL.readCreateProcessWithExitCode (proc "tests/script1.hs" []) i
                       assertEqual
                         "ExitFailure"
                         (ExitFailure 123,
                          "",
                          "This is an error message.\n")
                         b)
         , testWord8Instances
           (\ s i -> TestLabel s $ TestCase $ do
                       (ex, out, err) <- LL.readCreateProcessWithExitCode (proc "cat" ["tests/houseisclean.jpg"]) i
                       assertEqual "Length" (ExitSuccess, 68668, 0) (ex, length out, length err))
         , testInstances
           (\ s i -> TestLabel s $ TestCase $ do
                       l <- LL.readCreateProcessWithExitCode (proc "tests/script1.hs" []) i
                       assertEqual "Error 123" (ExitFailure 123, "", "This is an error message.\n") l)
         , testWord8Instances
           (\ s i -> TestLabel s $ TestCase $ do
                       jpg <- readFile "tests/penguin.jpg"
                       (code1, pnm, err1) <- LL.readCreateProcessWithExitCode (proc "djpeg" []) (i <> jpg) -- force the return type of readFile to match i
                       (code2, out2, err2) <- LL.readCreateProcessWithExitCode (proc "pnmfile" []) pnm
                       assertEqual "pnmfile1"
                                   (ExitSuccess, mempty, 2192, 27661, "stdin:\tPPM raw, 96 by 96  maxval 255\n")
                                   (code1, err1, length jpg, length pnm, out2))
         , testCharInstances
           (\ s i -> TestLabel s $ TestCase $ do
                     Left e <- try $ do jpg <- readFile "tests/penguin.jpg"
                                        (code1, pnm, err1) <- LL.readCreateProcessWithExitCode (proc "djpeg" []) (i <> jpg)
                                        LL.readCreateProcessWithExitCode (proc "pnmfile" []) pnm
                     assertEqual "pnmfile2" (IOError {ioe_handle = ioe_handle e,
                                                      ioe_type = InvalidArgument,
                                                      ioe_location = "hGetContents",
                                                      ioe_description = "invalid byte sequence",
                                                      ioe_errno = Nothing,
                                                      ioe_filename = Just "tests/penguin.jpg"})
                                      (e :: IOError))
         , testInstances
           (\ s i -> TestLabel s $ TestCase $ do
                       let n = case isSuffixOf "ByteString" s of
                                 True -> (50000000 :: Int)
                                 False -> (5000000 :: Int)
                           l :: Int
                           l = case isSuffixOf "ByteString" s of
                                 True -> (539000002 :: Int)
                                 False -> (49000001 :: Int)
                       (ex, out, err) <- LL.readCreateProcessWithExitCode (proc "time" ["bash", "-c", "yes | cat -n | head -n " <> show n]) i
                       let t = parseTimeOutput err
                           v = fromInteger (toEnum l) / t
                       hPutStrLn stderr (printf "%7.2f million chars/second for " (v / 1000000.00) <> s <> " (" <> percentMessage v (expected s) <> ")")
                       let r = v `about` expected s
                       assertEqual "5M lines" (ExitSuccess, l, "Within 30% of usual speed") (ex, length out, r))
         , testInstances
           (\ s i -> TestLabel s $ TestCase $ do
                       hPutStrLn stderr $ "deadlock test for " ++ s
                       result <- timeout 1000000 $ print =<< LL.readProcessWithExitCode "sleep" ["2h"] i
                       assertEqual "takano-akio deadlock test" Nothing result)
         ]
       ])

expected :: String -> Double
expected "String" =             23000000.0
expected "Strict ByteString" = 760000000.0
expected "Lazy ByteString" =   760000000.0
expected "Strict Text" =        44000000.0
expected "Lazy Text" =          47000000.0
expected s = error $ "Unexpected instance: " <> show s

percentMessage v expected =
    case v / expected of
      r | r < 1.0 -> printf "%01.2f pct slower than usual" (100.0 - 100.0 * r)
      r -> printf "%01.2f pct faster than usual" (100.0 * r - 100.0)

about :: Double -> Double -> String
about v expected =
    case v / expected of
      r | r >= 0.7 && r <= 1.3 -> "Within 30% of usual speed"
      _ -> percentMessage v expected

parseTimeOutput :: forall a c. (ListLike a c, Enum c) => a -> Double
parseTimeOutput a =
    case s =~ ("^(.*)system ([0-9]*):([0-9]*).([0-9]*)elapsed(.*)$" :: String) :: (String, String, String, [String]) of
      (_, _, _, [_, m, s, h, _]) -> (read m :: Double) * 60.0 + (read s :: Double) + (read ("0." <> h) :: Double)
    where
      s = map (chr . fromEnum) $ toList a
