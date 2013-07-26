import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.ByteString.Lazy.Char8 as BS hiding (length,take,drop,filter,head,zip)
import Control.Applicative hiding ((<|>), many)
import Text.Printf

{--
sm2（Open usp Tukubai）

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
showUsage = do System.IO.hPutStr stderr ("Usage    : sm2 [+ng] <key=n> <master> <tran>\n" ++ 
                "Fri Jul 26 11:50:49 JST 2013\n" ++
                "Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

main :: IO ()
main = do args <- getArgs
          case args of
                ["-h"]     -> showUsage
                ["--help"] -> showUsage
                ["+count",a,b,c,d] -> readF "-"   >>= mainProc True (read a::Int) (read b::Int) (read c::Int) (read d::Int)
                ["+count",a,b,c,d,file] -> readF file  >>= mainProc True (read a::Int) (read b::Int) (read c::Int) (read d::Int)
                [a,b,c,d]       -> readF "-"   >>= mainProc False (read a::Int) (read b::Int) (read c::Int) (read d::Int)
                [a,b,c,d,file]  -> readF file  >>= mainProc False (read a::Int) (read b::Int) (read c::Int) (read d::Int)
                _               -> showUsage


mainProc :: Bool -> Int -> Int -> Int -> Int -> BS.ByteString -> IO ()
mainProc count k1 k2 v1 v2 str = BS.putStr $ BS.unlines $ decode count $ calc rs
                                 where lns = BS.lines str
                                       rs = [ makeRecord (myWords ln) k1 k2 v1 v2 | ln <- lns ]

data Record = Record BS.ByteString Int [Double]

makeRecord :: [BS.ByteString] -> Int -> Int -> Int -> Int -> Record
makeRecord ws  0  0 v1 v2 = Record (BS.pack "") 1 values
                            where vws = take (v2-v1+1) $ drop (v1-1) ws
                                  values = [ read (BS.unpack v)::Double | v <- vws ]
makeRecord ws k1 k2 v1 v2 = Record key 1 values
                            where key = BS.unwords $ take (k2-k1+1) $ drop (k1-1) ws
                                  vws = take (v2-v1+1) $ drop (v1-1) ws
                                  values = [ read (BS.unpack v)::Double | v <- vws ]

decode :: Bool -> [Record] -> [BS.ByteString]
decode count rs = [decode' count r | r <- rs]

decode' :: Bool -> Record -> BS.ByteString
decode' False (Record k n vs) = BS.unwords [k,showValues vs]
decode' True  (Record k n vs) = BS.unwords [k,BS.pack $ show n,showValues vs]

showValues :: [Double] -> BS.ByteString
showValues vs = BS.pack $ Prelude.unwords [ printf "%f" v | v <- vs ]

calc :: [Record] -> [Record]
calc (r:[])                                 = [r]
calc ((Record k n vs):(Record kn _ vns):rs) = if k == kn 
                                              then calc ((Record k (n+1) (pls vs vns)):rs) 
                                              else (Record k n vs) : calc ((Record kn 1 vns):rs)

pls :: [Double] -> [Double] -> [Double]
pls a b = [ (fst p) + (snd p) | p <- (zip a b) ]
                      
readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f

myWords :: BS.ByteString -> [BS.ByteString]
myWords line = filter (/= BS.pack "") $ BS.split ' ' line
