import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.ByteString.Lazy.Char8 as BS hiding (length,take,drop,filter,head)
import Data.List.Split
import Control.Applicative hiding ((<|>), many)

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
                "Sun Jul 21 15:37:16 JST 2013\n" ++
                "Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

main :: IO ()
main = do args <- getArgs
          case args of
                ["-h"]     -> showUsage
                ["--help"] -> showUsage
                ["+ng",key,master,tran] -> mainProc True  key master tran
                ["+ng",key,master]      -> mainProc True  key master "-"
                [key,master,tran]       -> mainProc False key master tran
                [key,master]            -> mainProc False key master "-"


mainProc :: Bool -> String -> String -> String -> IO ()
mainProc ng key master tran = do ms <- readF master
                                 ts <- readF tran
                                 mainProc' ng (parseKey key) ms ts

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f

parseKey :: String -> Keys
parseKey str = case parse keys "" str of
                    Right opt -> opt
                    Left  err -> Error (show err)

mainProc' :: Bool -> Keys -> BS.ByteString -> BS.ByteString -> IO ()
mainProc' ng (Keys ks) ms ts = out ng [ join1 mlines t | t <- tlines ]
                               where mlines = parseMaster ks (BS.lines ms)
                                     tlines = parseTran ks (BS.lines ts)
                                     ans = [ join1 mlines t | t <- tlines ]

out :: Bool -> [OutTran] -> IO ()
out _  []                  = do return ()
out False ((OkTran ln):as) = (BS.putStrLn ln) >> (out False as)
out True  ((OkTran ln):as) = (BS.putStrLn ln) >> (out True  as)
out False ((NgTran ln):as) = out False as
out True  ((NgTran ln):as) = (BS.hPutStrLn stderr ln) >> (out True as)

join1 :: [Master] -> Tran -> OutTran
join1 ms (Tran p k a) = makeLine (pickMaster ms k) (Tran p k a)

makeLine :: Maybe Master -> Tran -> OutTran
makeLine (Just (Master k v)) (Tran p _ a) = OkTran (BS.unwords $ p ++ k ++ v ++ a)
makeLine Nothing             (Tran p k a) = NgTran (BS.unwords $ p ++ k ++ a)

myWords :: BS.ByteString -> [BS.ByteString]
myWords line = BS.split ' ' line

pickMaster :: [Master] -> [BS.ByteString] -> Maybe Master
pickMaster ms k = if length matched > 0 then Just (head matched) else Nothing
            where matched = filter ( matchMaster k ) ms

matchMaster k (Master a b) = k == a
              
parseMaster :: [Int] -> [BS.ByteString] -> [Master]
parseMaster ks lines = [ f (length ks) (myWords ln) | ln <- lines ]
                       where f n ws = Master (take n ws) (drop n ws)

parseTran :: [Int] -> [BS.ByteString] -> [Tran]
parseTran ks lines = [ parseTran' ks (myWords ln) | ln <- lines ]

parseTran' :: [Int] -> [BS.ByteString] -> Tran
parseTran' ks ws = Tran (take pre ws) (take (length ks) rem) (drop (length ks) rem)
             where pre = (ks !! 0) - 1
                   rem = drop pre ws

data Keys   = Keys [Int] | Error String
data Master = Master [BS.ByteString] [BS.ByteString] deriving Show -- keys and values
data Tran   = Tran [BS.ByteString] [BS.ByteString] [BS.ByteString] deriving Show -- values and keys and values
data OutTran = OkTran BS.ByteString | NgTran BS.ByteString

keys = string "key=" >> (try(rangekey) <|> try(singlekey) )

singlekey = do n <- many1 digit
               return $ Keys [read n::Int]

rangekey = do n <- many1 digit
              char '/'
              m <- many1 digit
              return $ Keys [(read n::Int)..(read m::Int)]
