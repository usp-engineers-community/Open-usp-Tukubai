#!/usr/bin/env runghc
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.ByteString.Lazy.Char8 as BS hiding (length,take,drop,filter,head)
import Control.Applicative hiding ((<|>), many)

{--
join1（Open usp Tukubai）

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
    System.IO.hPutStr stderr "Usage    : join1 [+ng] <key=n> <master> <tran>\n"
    System.IO.hPutStr stderr "Version  : Sun Jul 28 15:40:30 JST 2013\n"
    System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n"

main :: IO ()
main = do args <- getArgs
          case args of
                []         -> showUsage
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
mainProc' ng (Keys ks) ms ts = out ng (join1 ng (head mlines) (drop 1 mlines) tlines) 
                               where mlines = parseMaster ks (BS.lines ms)
                                     tlines = parseTran ks (BS.lines ts)

out :: Bool -> [OutTran] -> IO ()
out _  []                  = do return ()
out False ((OkTran ln):as) = (BS.putStrLn ln) >> (out False as)
out True  ((OkTran ln):as) = (BS.putStrLn ln) >> (out True  as)
out False ((NgTran ln):as) = out False as
out True  ((NgTran ln):as) = (BS.hPutStrLn stderr ln) >> (out True as)

join1 :: Bool -> Master -> [Master] -> [Tran] -> [OutTran]
join1 _ _ _ []    = []
join1 ng (Master mk v) [] ((Tran p tk a):ts) 
  | mk == tk      = OkTran (toStr2 v (Tran p tk a)) : join1 ng (Master mk v) [] ts
  | ng == True    = NgTran (toStr (Tran p tk a)) : join1 ng (Master mk v) [] ts
  | otherwise     = join1 ng (Master mk v) [] ts
join1 ng (Master mk v) (m:ms) ((Tran p tk a):ts) 
  | mk == tk      = OkTran (toStr2 v (Tran p tk a)) : join1 ng (Master mk v) (m:ms) ts 
  | mk <  tk      = join1 ng m ms ((Tran p tk a):ts) 
  | ng == True    = NgTran (toStr (Tran p tk a)) : join1 ng (Master mk v) (m:ms) ts
  | otherwise     = join1 ng (Master mk v) (m:ms) ts

toStr (Tran p k a) = BS.unwords $ filter (/= (BS.pack "")) [p,k,a]
toStr2 v (Tran p k a) = BS.unwords $ filter (/= (BS.pack "")) [p,k,v,a]

makeLine :: Maybe Master -> Tran -> OutTran
makeLine (Just (Master k v)) (Tran p _ a) = OkTran (BS.unwords [p,k,a])
makeLine Nothing             (Tran p k a) = NgTran (BS.unwords [p,k,a])

myWords :: BS.ByteString -> [BS.ByteString]
myWords line = BS.split ' ' line

matchMaster k (Master a b) = k == a
              
parseMaster :: [Int] -> [BS.ByteString] -> [Master]
parseMaster ks lines = [ f (length ks) (myWords ln) | ln <- lines ]
                       where f n ws = Master (BS.unwords $ take n ws) (BS.unwords $ drop n ws)

parseTran :: [Int] -> [BS.ByteString] -> [Tran]
parseTran ks lines = [ parseTran' ks (myWords ln) | ln <- lines ]

parseTran' :: [Int] -> [BS.ByteString] -> Tran
parseTran' ks ws = Tran a k p
             where pre = (ks !! 0) - 1
                   rem = drop pre ws
                   a = BS.unwords $ take pre ws
                   k = BS.unwords $ take (length ks) rem
                   p = BS.unwords $ drop (length ks) rem

data Keys   = Keys [Int] | Error String
data Master = Master BS.ByteString BS.ByteString deriving Show -- keys and values
data Tran   = Tran BS.ByteString BS.ByteString BS.ByteString deriving Show -- values and keys and values
data OutTran = OkTran BS.ByteString | NgTran BS.ByteString deriving Show

keys = string "key=" >> (try(rangekey) <|> try(singlekey) )

singlekey = do n <- many1 digit
               return $ Keys [read n::Int]

rangekey = do n <- many1 digit
              char '/'
              m <- many1 digit
              return $ Keys [(read n::Int)..(read m::Int)]
