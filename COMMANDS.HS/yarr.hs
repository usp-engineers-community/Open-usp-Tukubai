#!/usr/bin/env runghc --
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.ByteString.Lazy.Char8 as BS hiding (filter,drop,head,length,take)
import Data.List as DL

{--
yarr（Open usp Tukubai）

designed by Nobuaki Tounaka
written  by Hinata Yanagi

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
     System.IO.hPutStr stderr "Usage    : yarr [num=<n>] [-<m>] [<file>]\n"
     System.IO.hPutStr stderr "Version  : Sat Oct  1 21:43:34 JST 2022\n"
     System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD)\n"

main :: IO ()
main = do args <- getArgs
          case args of
              ["-h"]        -> showUsage
              ["--help"]    -> showUsage
              ["--version"] -> showUsage
              []         -> do mainProc (Option 0 0 "-")
              _          -> do mainProc (setOpts args)

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f = BS.readFile f

data Option = Option Int Int String | Error String deriving Show

data Record = Record (Maybe BS.ByteString) [BS.ByteString]

mainProc :: Option -> IO ()
mainProc (Option num m file) = readF file >>= mainProc' num m

mainProc' :: Int -> Int -> BS.ByteString -> IO ()
mainProc' num m str = BS.putStr $ BS.unlines $ DL.map toStr $ yarr m rs
                      where rs = [ makeRecord num ln | ln <- BS.lines str ]

yarr :: Int -> [Record] -> [Record]
yarr m []       = []
yarr m (a:[])   = [a]
yarr 0 (a:b:rs) = if compKey a b then yarr 0 ((mergeKey a b) : rs) else (a : yarr 0 (b:rs))
yarr m (a:b:rs) = if compKey a b then yarr' m (x:rs) else (a : yarr m (b:rs))
                  where x = mergeKey a b

yarr' :: Int -> [Record] -> [Record]
yarr' m ((Record k vs):rs) = if (length vs) > m 
                             then Record k (take m vs) : yarr m (Record k (drop m vs): rs) 
                             else  (Record k vs) : yarr m rs

toStr :: Record -> BS.ByteString
toStr (Record k vs) = if k == Nothing then BS.unwords vs else BS.unwords ((f k):vs)
                      where f (Just x) = x

compKey (Record k vs) (Record k2 vs2 )  = k == k2
mergeKey (Record k vs) (Record k2 vs2 ) = Record k (vs ++ vs2)  

makeRecord :: Int -> BS.ByteString -> Record
makeRecord 0   str = Record Nothing (myWords str)
makeRecord num str = Record k (drop num vs)
                     where vs = myWords str
                           k  = Just (BS.unwords $ take num vs)

myWords :: BS.ByteString -> [BS.ByteString]
myWords ws = filter (/= BS.pack "") $ split ' ' ws

setOpts :: [String] -> Option
setOpts as = Option num m f
             where ns = filter ( DL.isPrefixOf "num=" ) as
                   num = if ns == [] then 0 else read $ drop 4 $ head ns
                   ms = filter ( DL.isPrefixOf "-" ) as
                   m = if ms == [] then 0 else read $ drop 1 $ head ms
                   str = DL.last as
                   f = if "num=" `DL.isPrefixOf` str 
                       then "-" else ( if "-" `DL.isPrefixOf` str then "-" else str )
