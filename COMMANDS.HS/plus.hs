#!/usr/bin/env runghc
import System.Environment
import System.Exit
import System.IO
import Data.Fixed

showUsage :: IO ()
showUsage = do
    System.IO.hPutStr stderr "Usage    : plus <n1> <n2> ... \n"
    System.IO.hPutStr stderr "Version  : Sun Apr 26 11:08:56 JST 2015\n"
    System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD+Mac), Haskell ver.\n"
    exitWith (ExitFailure 1)

die str = hPutStr stderr ( "Error[map] : " ++ str ++ "\n")
          >> exitWith (ExitFailure 1)

main = do args <- getArgs
          case args of
              ["-h"]     -> showUsage
              []         -> showUsage
              nums       -> main' nums

data E100 = E100
instance HasResolution E100 where resolution _ = 10 ^ 100

main' :: [String] -> IO ()
main' strs = putStrLn . shaveZero . show . sum $ map (\n -> read n :: (Fixed E100)) strs

shaveZero :: String -> String
shaveZero str = reverse $ dropWhile (== '.') $ dropWhile (== '0') $ reverse str

