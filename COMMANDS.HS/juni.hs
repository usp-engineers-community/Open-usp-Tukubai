import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.ByteString.Lazy.Char8 as BS hiding (length,take,drop,filter,head,concat)
import Control.Applicative hiding ((<|>), many)
import Data.List as DL

{--
juni（Open usp Tukubai）

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
showUsage = do System.IO.hPutStr stderr ("Usage    : juni <f1> <f2> <file>\n" ++ 
                "Tue Aug  6 10:40:57 JST 2013\n" ++
                "Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

main :: IO ()
main = do args <- getArgs
          case args of
                ["-h"]     -> showUsage
                ["--help"] -> showUsage
                []         -> readF "-" >>= simpleJuni
                [a]        -> readF a   >>= simpleJuni
                [a,b]      -> readF "-" >>= mainProc (read a) (read b)
                [a,b,c]    -> readF c   >>= mainProc (read a) (read b)

simpleJuni :: BS.ByteString -> IO ()
simpleJuni cs = simpleJuni' (BS.lines cs) 1

simpleJuni' :: [BS.ByteString] -> Int -> IO ()
simpleJuni' [] n = do return ()
simpleJuni' (ln:lns) n = (BS.putStrLn $ BS.unwords [BS.pack (show n),ln]) >> simpleJuni' lns (n+1)


mainProc :: Int -> Int -> BS.ByteString -> IO ()
mainProc f t cs = BS.putStr $ BS.unlines $ mainProc' f t (BS.lines cs) 1 (BS.pack "")

mainProc' :: Int -> Int -> [BS.ByteString] -> Int -> BS.ByteString -> [BS.ByteString]
mainProc' f t [] count k = []
mainProc' f t (ln:lns) count k = if k == key
                                 then BS.unwords [BS.pack $ (show count),ln] : mainProc' f t lns (count+1) k
                                 else BS.unwords [BS.pack "1",ln] : mainProc' f t lns 2 key
                         where wds = myWords ln
                               pre = BS.unwords $ DL.take (f-1) wds
                               tmp = DL.drop (f-1) wds
                               key = BS.unwords $ DL.take (t-f+1) tmp
                               pst = BS.unwords $ DL.drop (t-f+1) tmp


readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f

myWords :: BS.ByteString -> [BS.ByteString]
myWords line = filter (/= BS.pack "") $ BS.split ' ' line
