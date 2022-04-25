#!/usr/bin/env runghc --
import System.Environment
import System.IO
import Data.List.Split hiding (oneOf)
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.ByteString.Lazy.Char8 as BS hiding (filter,last,zip,head,drop,reverse,concat)
import Control.Applicative hiding ((<|>), many)

{--
comma（Open usp Tukubai）

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
    System.IO.hPutStr stderr "Usage    : comma <f1> <f2> ... <file>\n"
    System.IO.hPutStr stderr "Version  : Mon Apr 9 14:53:20 JST 2022\n"
    System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n"

main :: IO ()
main = do args <- getArgs
          case args of
              []         -> showUsage
              ["-h"]     -> showUsage
              ["--help"] -> showUsage
              _          -> mainProc (setOpts args) 

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f = BS.readFile f

mainProc :: Opts -> IO ()
mainProc (Opts fs file) = readF file >>= mainProc' fs

mainProc' :: [Int] -> BS.ByteString -> IO ()
mainProc' fs str = BS.putStr $ BS.unlines [ mainProc'' fs ln | ln <- BS.lines str]

mainProc'' :: [Int] -> BS.ByteString -> BS.ByteString
mainProc'' fs ln = BS.unwords [ fnc fs w | w <- zip [1..] (myWords ln)]
                   where fnc fs (n,w) = if filter ( == n) fs == [] 
                                        then w
                                        else BS.pack $ addComma $ BS.unpack w

addComma :: String -> String
addComma ('-':cs) = '-': addComma cs
addComma cs = (addCommaInt (head x)) ++ fmtFrac (drop 1 x)
              where x = splitOn "." cs

addCommaInt :: String -> String
addCommaInt cs = reverse $ f $ reverse cs
                 where f :: String -> String
                       f [] = []
                       f (a:[]) = (a:[])
                       f (a:b:[]) = (a:b:[])
                       f (a:b:c:[]) = (a:b:c:[])
                       f (a:b:c:cs) = (a:b:c:[]) ++ "," ++ f cs

fmtFrac :: [String] -> String
fmtFrac [] = []
fmtFrac (c:cs) = '.' : c

myWords :: BS.ByteString -> [BS.ByteString]
myWords line = filter (/= (BS.pack "")) $ BS.split ' ' line

data Opts   = Opts [Int] String | Error String deriving Show

setOpts :: [String] -> Opts
setOpts as = case parse args "" ((Prelude.unwords as) ++ " ") of
                  Right opt -> opt
                  Left err -> Error ( show err ) 

args = do Opts <$> (many1 $ try field) <*> (try(filename) <|> return "-")

field = do a <- many1 digit
           char ' '
           return (read a::Int)

filename = many1 ( try(letter) <|> try(digit) <|> symbol ) >>= return 

symbol = oneOf "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
