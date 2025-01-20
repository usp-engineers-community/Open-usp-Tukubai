#!/usr/bin/env runghc
import Control.Monad (forM_)
import Data.Ix (range)
import Data.List (stripPrefix)
import Prelude hiding (error)
import System.Environment
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO

{--
tcat（Open usp Tukubai）

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

showUsage :: IO ()
showUsage = do
    System.IO.hPutStr stderr "Usage   : tcat [-<n>] <file1> <file2> ...\n"
    System.IO.hPutStr stderr "Version : Mon Jan 20 17:18:16 JST 2025\n"
    System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD)\n"
    exitFailure

error message =
  System.IO.hPutStrLn stderr $ "Error[tcat] : " ++ message --ファイル -2a をオープンできません。"

main :: IO ()
main = do args <- getArgs
          case args of
                []            -> showUsage
                ["-h"]        -> showUsage
                ["--help"]    -> showUsage
                ["--version"] -> showUsage
                options       -> tcat options

tcat (option:options) =
  case stripPrefix "-" option of
    Nothing ->
      tcat' 0 (option:options)
    Just probable_padding ->
     case reads probable_padding :: [(Int, String)] of
       [(padding, "")] ->
         tcat' padding options
       _ ->
         tcat' 0 (option:options)

tcat' padding [] = showUsage

tcat' padding (file_name:file_names) = do
  exists <- doesFileExist file_name
  
  if exists || file_name == "-" then do
    file <-
      if file_name == "-" then
        return stdin
      else
        openFile file_name ReadMode

    hGetContents file >>= System.IO.putStr
    hClose file
 
    if file_names == [] then
      return ()
    else do
      System.IO.putStr $ take padding $ repeat '\n'
      tcat' padding file_names
  else
    error $ "ファイル " ++  file_name ++ " をオープンできません。"
