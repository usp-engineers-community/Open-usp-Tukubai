import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.ByteString.Lazy.Char8 as BS hiding (take,drop,filter,head,last,map,zip,repeat)
import Data.ByteString.Lazy.UTF8 as US hiding (take,drop,filter,head,last,map,zip,repeat)
import Control.Applicative hiding ((<|>), many)
import Data.Char

{--
cjoin1（Open usp Tukubai）

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
showUsage = do System.IO.hPutStr stderr ("Usage    : cjoin1 [+ng] <key=n> <master> <tran>\n" ++ 
                "Tue Jul 23 11:10:37 JST 2013\n" ++
                "Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

main :: IO ()
main = do args <- getArgs
          case args of
                ["-h"]                  -> showUsage
                ["--help"]              -> showUsage
                [delim,key,master,tran] -> mainProc delim key master tran
                [a,b,c]                 -> if (a !! 0) == 'k' 
                                           then mainProc "" a b c
                                           else mainProc a b c "-"
                [key,master]            -> mainProc "" key master "-"


mainProc :: String -> String -> String -> String -> IO ()
mainProc delim key master tran = do ms <- readF master
                                    ts <- readF tran
                                    mainProc' (parseKey key) ms ts (readDelim delim)

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f

readDelim :: String -> String
readDelim ""            = ""
readDelim ('+':str)     = str
readDelim ('-':'d':str) = str

parseKey :: String -> Keys
parseKey str = case parse keys "" str of
                    Right opt -> opt
                    Left  err -> Error (show err)

mainProc' :: Keys -> BS.ByteString -> BS.ByteString -> String -> IO ()
mainProc' (Keys ks) ms ts delim = out [ join1 mlines t | t <- tlines ]
                               where mlines = parseMaster ks (BS.lines ms) delim
                                     tlines = parseTran ks (BS.lines ts)

out :: [OutTran] -> IO ()
out []               = do return ()
out (ln:lns) = (BS.putStrLn ln) >> (out lns)

join1 :: [Master] -> Tran -> OutTran
join1 ms (Tran p k a) = makeLine (pickMaster ms k) (Tran p k a)

makeLine :: Master -> Tran -> OutTran
makeLine (Master [] v) (Tran p k a) = BS.unwords $ p ++ k ++ v ++ a
makeLine (Master k v) (Tran p _ a) = BS.unwords $ p ++ k ++ v ++ a

myWords :: BS.ByteString -> [BS.ByteString]
myWords line = BS.split ' ' line

pickMaster :: [Master] -> [BS.ByteString] -> Master
pickMaster ms k = if Prelude.length matched > 0 then head matched else dummy
            where matched = filter ( matchMaster k ) ms
                  a = last ms
                  dummy = f a
                  f (Master _ v) = Master [] v

matchMaster k (Master a b) = k == a
              
parseMaster :: [Int] -> [BS.ByteString] -> String -> [Master]
parseMaster ks lines delim = ms ++ [makeDummy ms delim]
                       where ms = [ f (Prelude.length ks) (myWords ln) | ln <- lines ]
                             f n ws = Master (take n ws) (drop n ws)

makeDummy :: [Master] -> String -> Master
makeDummy ms "" = Master k [ BS.pack $ take y ( repeat '*' ) | y <- x ]
               where x = maxLengths [ getValueLength m | m <- ms ]
                     h = head ms
                     k = f h
                     f (Master a _) = a
makeDummy ms str = Master k [ BS.pack str | y <- (g h) ]
               where h = head ms
                     k = f h
                     f (Master a _) = a
                     g (Master _ b) = b

maxLengths :: [[Int]] -> [Int]
maxLengths (vsr:vsl:[]) = [ if (fst z) > (snd z) then fst z else snd z | z <- (zip vsr vsl)] 
maxLengths (vsr:vsl:vss) = maxLengths (a:vss)
                            where zs = zip vsr vsl
                                  a  = [ if (fst z) > (snd z) then fst z else snd z | z <- zs] 

getValueLength :: Master -> [Int]
getValueLength (Master k v) = map ( wc . BS.unpack ) v

parseTran :: [Int] -> [BS.ByteString] -> [Tran]
parseTran ks lines = [ parseTran' ks (myWords ln) | ln <- lines ]

parseTran' :: [Int] -> [BS.ByteString] -> Tran
parseTran' ks ws = Tran (take pre ws) (take (Prelude.length ks) rem) (drop (Prelude.length ks) rem)
             where pre = (ks !! 0) - 1
                   rem = drop pre ws

wc :: String -> Int
wc []           = 0
wc (c:[])       = 1
wc (c:a:[])     = 2
wc (c:a:b:cs) = (wc' (ord c))
                 where wc' n = if n < 128 
                       then (1 + wc (a:b:cs))
                       else (hanzen (n*256*256+(ord a)*256+(ord c))) + wc cs
                       hanzen m = if m >= 0xFF61 && m <= 0xFF9F then 1 else 2

data Keys   = Keys [Int] | Error String
data Master = Master [BS.ByteString] [BS.ByteString] deriving Show -- keys and values
data Tran   = Tran [BS.ByteString] [BS.ByteString] [BS.ByteString] deriving Show -- values and keys and values
type OutTran = BS.ByteString

keys = string "key=" >> (try(rangekey) <|> try(singlekey) )

singlekey = do n <- many1 digit
               return $ Keys [read n::Int]

rangekey = do n <- many1 digit
              char '/'
              m <- many1 digit
              return $ Keys [(read n::Int)..(read m::Int)]
