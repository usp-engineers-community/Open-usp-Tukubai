#!/usr/bin/env runghc --
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.ByteString.Lazy.Char8 as BS hiding (filter)

{--
delf（Open usp Tukubai）

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
    System.IO.hPutStr stderr "Usage    : delf <f1> <f2> ... [<file>]\n"
    System.IO.hPutStr stderr "Version  : Sat Oct  1 21:43:34 JST 2022\n"
    System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD)\n"

main :: IO ()
main = do args <- getArgs
          case args of
              []            -> showUsage
              ["-h"]        -> showUsage
              ["--help"]    -> showUsage
              ["--version"] -> showUsage
              _             -> do readF (getFileName os) >>= mainProc fs
                                     where fs = getFields os
                                           os = setOpts args

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f = BS.readFile f
                                   
------------
-- output --
------------

mainProc :: [(Int,Int)] -> BS.ByteString -> IO ()
mainProc fs cs = BS.putStr $ BS.unlines [ lineProc fs c | c <- BS.lines cs ]

lineProc :: [(Int,Int)] -> BS.ByteString -> BS.ByteString
lineProc fs ln = BS.unwords $ deleteFields nfs zwds
                 where zwds = Prelude.zip [1..] (myWords ln)
                       nfs = fieldNormalize fs (Prelude.length zwds)

myWords :: BS.ByteString -> [BS.ByteString]
myWords ws = filter (/= BS.pack "") $ split ' ' ws

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
data Option = FRange Field Field | FileName String | Error String

getFields :: [Option] -> [(Int,Int)]
getFields ((FRange a b):opts) = (f a,f b) : getFields opts
                                where f (Field c) = c
getFields (opt:opts)       = getFields opts
getFields []               = []

getFileName :: [Option] -> String
getFileName ((FileName s):opts) = s
getFileName (opt:opts)          = getFileName opts
getFileName []                  = "-"

setOpts :: [String] -> [Option]
setOpts as = [ fnc a | a <- as ]
             where fnc str = case parse parseOption "" str of
                                  Right opt -> opt
                                  Left err -> Error ( show err ) 

parseMonoRange :: Parser Option
parseMonoRange = do {f <- parseField ; return (FRange f f) }

parseField :: Parser Field
parseField = try(parseNormalField) <|> try(parseNFMinusField) <|> try(parseNFField)

parseCompRange :: Parser Option
parseCompRange = do first <- parseField
                    char '/'
                    second <- parseField
                    return ( FRange first second )

parseOption :: Parser Option
parseOption = try(parseCompRange) <|> try(parseMonoRange) <|> try(parseFileName)

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
