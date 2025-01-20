#!/usr/bin/env runghc
import System.Environment
import System.Exit
import System.IO
import Data.List as DL

{--
ysum（Open usp Tukubai）

designed by Nobuaki Tounaka
written  by Hinata Yanagi

The MIT License

Copyright (C) 2025 Universal Shell Programming Laboratory

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
--}


command_name = "ysum"
version = "Sun Mar 19 18:24:55 JST 2023"

showUsage :: IO ()
showUsage = do
  System.IO.hPutStr stderr $ "Usage    : " ++ command_name ++ " [num=<n>] [-<m>] [<file>]\n"
  System.IO.hPutStr stderr $ "Version  : Mon Jan 20 17:18:16 JST 2025\n"
  System.IO.hPutStr stderr $ "           Open usp Tukubai (LINUX+FREEBSD)\n"
  exitFailure

main :: IO ()
main = do args <- getArgs
          case args of
            ["-h"]        -> showUsage
            ["--help"]    -> showUsage
            ["--version"] -> showUsage
            ["+h", prefixed_num] -> ysum' True prefixed_num stdin
            ["+h", prefixed_num, file_name] -> openFile file_name ReadMode >>= ysum' True prefixed_num
            [prefixed_num] -> ysum' False prefixed_num stdin
            [prefixed_num, file_name] -> openFile file_name ReadMode >>= ysum' False prefixed_num
            _ -> showUsage

ysum' skip_header prefixed_num source =
  case DL.stripPrefix "num=" prefixed_num of
    Nothing -> showUsage
    Just num' ->
      ysum skip_header (quantify num') source

quantify :: [Char] -> Int

quantify string =
  case reads string of
   [(number, "")] -> number
   _ -> error "数値に変換できません"

ysum skip_header num source = do
  if not skip_header then return () else
    hGetLine source >>= putStrLn
 
  hIsEOF source >>= \eof ->
    case eof of
     True  -> hClose source
     False -> do
      content <- words <$> hGetLine source
      System.IO.putStrLn $ unwords $ content ++ [show $ sum $ fmap quantify $ drop num content]
      ysum False num source
