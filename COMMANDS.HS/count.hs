import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.ByteString.Lazy.Char8 as BS hiding (length,take,drop,filter,head)
import Control.Applicative hiding ((<|>), many)

{--
count（Open usp Tukubai）

designed by Nobuaki Tounaka
written by Ryuichi Ueda

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
showUsage = do System.IO.hPutStr stderr ("Usage    : count [+ng] <key=n> <master> <tran>\n" ++ 
                "Thu Jul 25 22:54:27 JST 2013\n" ++
                "Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

main :: IO ()
main = do args <- getArgs
          case args of
                ["-h"]     -> showUsage
                ["--help"] -> showUsage
                [one,two]      -> readF "-"  >>= mainProc (read one::Int) (read two::Int)
                [one,two,file] -> readF file >>= mainProc (read one::Int) (read two::Int)


mainProc :: Int -> Int -> BS.ByteString -> IO ()
mainProc f t str = BS.putStr $ BS.unlines $ doCount ks (BS.pack "") 0
               where lns = BS.lines str
                     ks = [ BS.unwords $ take (t-f+1) $ drop (f-1) $ myWords ln | ln <- lns ]

doCount :: [BS.ByteString] -> BS.ByteString -> Int -> [BS.ByteString]
doCount []     prev carry = [BS.unwords [prev,BS.pack (show carry)]]
doCount (k:ks) _    0     = doCount ks k 1
doCount (k:ks) prev carry = if k == prev
                            then doCount ks prev (carry+1)
                            else (BS.unwords [prev,BS.pack (show carry)]) : (doCount (k:ks) prev 0)
           
                      
readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f

myWords :: BS.ByteString -> [BS.ByteString]
myWords line = filter (/= BS.pack "") $ BS.split ' ' line
