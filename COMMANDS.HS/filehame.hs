#!/usr/bin/env runghc --
import System.Environment
import System.IO
import Data.List
import Data.ByteString.Lazy.Char8 as BS hiding (length,drop)

{--
filehame（Open usp Tukubai）

designed by Nobuaki Tounaka
written  by Hinata Yanagi

The MIT License

Copyright (C) 2022 Universal Shell Programming Laboratory

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
     System.IO.hPutStr stderr "Usage    : filehame <-lSTRING> <file1> <file2>\n"
     System.IO.hPutStr stderr "Version  : Tue Apr 19 15:03:21 JST 2022\n"
     System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD)\n"

main :: IO ()
main = do args <- getArgs
          case args of
                (label:f1:f2:[]) -> do cs <- readF f1
                                       ins <- readF f2
                                       mainProc (drop 2 label) (BS.lines cs) ins
                _                -> showUsage

mainProc :: String -> [BS.ByteString] -> BS.ByteString -> IO ()
mainProc "" _ _             = error "no label"
mainProc label [] ins  = do return ()
mainProc label (ln:lns) ins = f >> mainProc label lns ins
                            where f = if label `isInfixOf` (BS.unpack ln)
                                      then BS.putStr ins
                                      else BS.putStrLn ln 

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f
