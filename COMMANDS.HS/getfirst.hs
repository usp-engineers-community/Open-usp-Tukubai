#!/usr/bin/env runghc --
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.ByteString.Lazy.Char8 as BS hiding (length,take,drop,filter,head,concat)
import Control.Applicative hiding ((<|>), many)

{--
getfirst（Open usp Tukubai）

designed by Nobuaki Tounaka
written  by Ryuichi Ueda

The MIT License

Copyright (C) 2012 Universal Shell Programming Laboratory

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
    System.IO.hPutStr stderr "Usage    : getfirst <f1> <f2> <file>\n"
    System.IO.hPutStr stderr "Version  : Tue Aug  6 10:09:16 JST 2013\n"
    System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n"

main :: IO ()
main = do args <- getArgs
          case args of
                []         -> showUsage
                ["-h"]     -> showUsage
                ["--help"] -> showUsage
                [one,two]      -> readF "-"  >>= mainProc (read one::Int) (read two::Int)
                [one,two,file] -> readF file >>= mainProc (read one::Int) (read two::Int)


mainProc :: Int -> Int -> BS.ByteString -> IO ()
mainProc f t str = BS.putStr $ BS.unlines $ getFirst ks (BS.pack "")
               where lns = BS.lines str
                     ks = [ splitLine (myWords ln) f t | ln <- lns ]

data Record = Record BS.ByteString BS.ByteString BS.ByteString

splitLine :: [BS.ByteString] -> Int -> Int -> Record
splitLine ws f t = Record pre key post
                    where pre = BS.unwords $ take (f-1) ws
                          post = BS.unwords $ drop t ws
                          key = BS.unwords $ drop (f-1) $ take t ws

getFirst :: [Record] -> BS.ByteString -> [BS.ByteString]
getFirst [] key                    = []
getFirst ((Record pr k po):rs) key = if k == key
                                     then getFirst rs key
                                     else (decode pr k po) : getFirst rs k

decode :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
decode pr k po = BS.unwords $ filter (/= (BS.pack "")) [pr,k,po]
                      
readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f

myWords :: BS.ByteString -> [BS.ByteString]
myWords line = filter (/= BS.pack "") $ BS.split ' ' line
