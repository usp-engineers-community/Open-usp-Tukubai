import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.ByteString.Lazy.Char8 as BS hiding (filter,drop,head,length,take)
import Data.List as DL

{--
yarr（Open usp Tukubai）

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
showUsage = do System.IO.hPutStr stderr
		("Usage    : yarr [num=<n>] [-<m>] <file>\n" ++ 
		"Fri Jul 26 20:16:22 JST 2013\n" ++
		"Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

main :: IO ()
main = do args <- getArgs
          case args of
              ["-h"]     -> showUsage
              ["--help"] -> showUsage
              _          -> do mainProc (setOpts args)
{--
              _          -> do readF (getFileName os) >>= mainProc fs
                                     where fs = getFields os
                                           os = setOpts args
--}

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
yarr' m ((Record k vs):rs) = if (length vs) > m then Record k (take m vs) : yarr m (Record k (drop m vs): rs) 
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
setOpts as = Option y1 y2 f
             where x1 = filter ( > 0) [ getNum a | a <- as ]
                   y1 = if x1 == [] then 0 else head x1
                   x2 = filter ( > 0) [ getM a | a <- as ]
                   y2 = if x2 == [] then 0 else head x2
                   fs = filter (/= "") [ getFileName a | a <- as ]
                   f = if fs == [] then "-" else head fs

getNum :: String -> Int
getNum str = if "num=" `DL.isPrefixOf` str
             then read $ drop 4 str
             else 0
        
getM :: String -> Int
getM str = if "-" `DL.isPrefixOf` str
             then read $ drop 1 str
             else 0

getFileName :: String -> String
getFileName str = if "-" `DL.isPrefixOf` str then "" else y
             where y = if "num=" `DL.isPrefixOf` str then "" else str
                                   
{--
------------
-- output --
------------

--mainProc fs cs = BS.putStr $ BS.unlines [ lineProc fs c | c <- BS.lines cs ]

lineProc :: [(Int,Int)] -> BS.ByteString -> BS.ByteString
lineProc fs ln = BS.unwords $ deleteFields nfs zwds
                 where zwds = Prelude.zip [1..] (myWords ln)
                       nfs = fieldNormalize fs (Prelude.length zwds)

fieldNormalize :: [(Int,Int)] -> Int -> [Int]
fieldNormalize ((a,b):fs) fnum = [c..d] ++ (fieldNormalize fs fnum)
                 where c = if a > 0 then a else fnum + a
                       d = if b > 0 then b else fnum + b
fieldNormalize [] fnum = []

deleteFields :: [Int] -> [(Int,BS.ByteString)] -> [BS.ByteString]
deleteFields fs ((n,w):nws) = if n `Prelude.elem` fs
                              then deleteFields fs nws
                              else w : deleteFields fs nws 
deleteFields fs [] = []

------------------------
-- parsing of options --
------------------------

data Field = Field Int

getFields :: [Option] -> [(Int,Int)]
getFields ((FRange a b):opts) = (f a,f b) : getFields opts
                                where f (Field c) = c
getFields (opt:opts)       = getFields opts
getFields []               = []

getFileName :: [Option] -> String
getFileName ((FileName s):opts) = s
getFileName (opt:opts)          = getFileName opts
getFileName []                  = "-"

parseMonoRange :: Parser Option
parseMonoRange = do {f <- parseField ; return (FRange f f) }

parseField :: Parser Field
parseField = try(parseNormalField) <|> try(parseNFMinusField) <|> try(parseNFField)

parseCompRange :: Parser Option
parseCompRange = do first <- parseField
                    char '/'
                    second <- parseField
                    return ( FRange first second )

parseNormalField :: Parser Field
parseNormalField = liftM (Field . read) $ many1 digit

parseNFMinusField :: Parser Field
parseNFMinusField =  do string "NF-"
                        num <- many1 digit
                        return $ Field ( -1 * (read num) )

parseNFField :: Parser Field
parseNFField =  string "NF" >> (return $ Field 0)

parseFileName :: Parser Option
parseFileName =  many1 ( letter <|> digit <|> symbol ) >>= return . FileName

symbol :: Parser Char
symbol = oneOf "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
--}
