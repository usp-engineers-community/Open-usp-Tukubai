#!/usr/bin/env runghc --
import Control.Monad (foldM, join)
import GHC.Utils.Monad (anyM)
import System.Environment
import System.Exit
import System.IO
import Data.Maybe (fromJust)
import Data.Ix (range)
import Data.List (drop, elemIndex, singleton, take)
import Data.Time.Calendar
import Data.Text (pack, strip, unpack)

{--
ysum（Open usp Tukubai）

designed by Nobuaki Tounaka
written  by Hinata Yanagi

The MIT License

Copyright (C) 2023 Universal Shell Programming Laboratory

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

command_name = "ycat"
version = "Mon Mar 20 12:50:58 JST 2023"

showUsage :: IO ()
showUsage = do
  System.IO.hPutStr stderr $ "Usage    : " ++ command_name ++ " [-<n>] file1 file2 ...\n"
  System.IO.hPutStr stderr $ "Version  : " ++ version ++ "\n"
  System.IO.hPutStr stderr $ "           Open usp Tukubai (LINUX+FREEBSD)\n"
  exitFailure

data Language = Index | English | Japanese deriving Show

main :: IO ()
main = do args <- getArgs
          case args of
            []            -> showUsage
            ["-h"]        -> showUsage
            ["--help"]    -> showUsage
            ["--version"] -> showUsage
            padding_or_file_path:file_paths ->
              if padding_or_file_path !! 0 == '-' then
                case (reads $ tail padding_or_file_path) :: [(Int, String)] of
                 [(padding, "")] ->
                   ycat padding file_paths
                 _ ->
                   ycat 1 $ padding_or_file_path:file_paths
              else
                ycat 1 $ padding_or_file_path:file_paths

ycat padding file_paths = do
  handles <- mapM (\file_path -> openFile file_path ReadMode) file_paths
  ycat' padding handles
  _ <- mapM hClose handles
  return ()

ycat' padding handles = do
  terminate <- anyM hIsEOF handles

  if terminate then return ()
  else do
    foldM (\concatenated handle -> do
      line <- hGetLine handle
      return $ (
        if concatenated == "" then "" else
          concatenated ++ (take padding $ repeat ' ')) ++ line) "" handles
      >>= System.IO.putStrLn
    ycat' padding handles
