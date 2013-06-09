import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.ByteString.Lazy.Char8 as BS
import Data.List.Split

{--
cjoin0（Open usp Tukubai）

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
showUsage = do System.IO.hPutStr stderr ("Usage    : cjoin0 [+ng] <key=n> <master> <tran>\n" ++ 
                "Sun Jun  9 17:48:12 JST 2013\n" ++
                "Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

type Lines = [BS.ByteString]
type TrnLine = BS.ByteString
type Master = [[BS.ByteString]]
type TranWords = [BS.ByteString]
type Keys = [Int]

main :: IO ()
main = do args <- getArgs
          case args of
                ["-h"]     -> showUsage
                ["--help"] -> showUsage
                ("+ng":as) -> mainProc True as
                _          -> mainProc False args


mainProc :: Bool -> [String] -> IO ()
mainProc flg as = do mst <- readF (as !! 1)
                     trn <- readF tran
                     printRes flg $ mainProc' keys (BS.lines mst) (BS.lines trn)
                     where tran = if (Prelude.length as) == 3 then (as !! 2) else "-"
                           keys = getKeys (as !! 0)

mainProc' :: Keys -> Lines -> Lines -> [(Int,TrnLine)]
mainProc' ks ms ts = [ mainProc'' ks master t | t <- ts ]
                         where master = [ Prelude.take keynum (BS.words ln) | ln <- ms ]
                               keynum = Prelude.length ks

mainProc'' :: Keys -> Master -> TrnLine -> (Int,TrnLine)
mainProc'' ks ms t = if keys `Prelude.elem` ms then (0,t) else (1,t)
                     where keys = takeKeys ks (BS.words t)

printRes :: Bool -> [(Int,TrnLine)] -> IO ()
printRes False cs = BS.putStr . BS.unlines $ ok cs
printRes True cs = do BS.putStr . BS.unlines $ ok cs 
                      BS.hPutStr stderr . BS.unlines $ ng True cs

ok :: [(Int,TrnLine)] -> [TrnLine]
ok ((0,ln):lns) = ln : ok lns
ok ((1,ln):lns) = ok lns
ok [] = [] 

ng :: Bool -> [(Int,TrnLine)] -> [TrnLine]
ng False lns = [] 
ng True ((1,ln):lns) = ln : ng True lns
ng True ((0,ln):lns) = ng True lns
ng True [] = [] 

takeKeys :: Keys -> TranWords -> [BS.ByteString]
takeKeys (n:ns) ts = (ts !! n) : takeKeys ns ts
takeKeys [] ts = []

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f = BS.readFile f

getKeys :: String -> Keys
getKeys str = [f..s]
              where ks = splitOn "/" (Prelude.drop 4 str)
                    f = read (ks !! 0) - 1
                    s = if Prelude.length ks == 1 then f else read (ks !! 1) - 1
