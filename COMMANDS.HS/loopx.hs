#!/usr/bin/env runghc --
import System.Environment
import System.IO
import Data.List.Split hiding (oneOf)
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.ByteString.Lazy.Char8 as BS hiding (filter,last,zip,head,drop,reverse,concat)

{--
loopx（Open usp Tukubai）

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
showUsage = do System.IO.hPutStr stderr ("Usage    : loopx <file1> <file2> ... \n" ++ 
                "Version  : Sat Oct  1 21:43:34 JST 2022\n" ++
                "Open usp Tukubai (LINUX+FREEBSD)\n")


main :: IO ()
main = do args <- getArgs
          case args of
              []            -> showUsage
              ["-h"]        -> showUsage
              ["--help"]    -> showUsage
              ["--version"] -> showUsage
              _             -> mainProc args []

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f = BS.readFile f

type Record = BS.ByteString

mainProc :: [String] -> [Record] -> IO ()
mainProc []     out = BS.putStr $ BS.unlines out 
mainProc (a:as) out = readF a >>= (mainProc' as out) . BS.lines

mainProc' :: [String] -> [Record] -> [Record] -> IO ()
mainProc' files [] right = mainProc files right
mainProc' files left right = mainProc files $ joint left right

joint :: [Record] -> [Record] -> [Record]
joint ls rs = concat [ joint' ln rs | ln <- ls ]

joint' :: Record -> [Record] -> [Record]
joint' left rs = [ BS.unwords [left,r] | r <- rs ]
          
 
