#!/usr/bin/env runghc
import System.Environment
import System.IO
import Data.List.Split hiding (oneOf)
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.ByteString.Lazy.Char8 as BS hiding (filter,last,zip,head,drop,reverse,concat)
import Control.Applicative hiding ((<|>), many)
import Text.Printf

{--
divsen（Open usp Tukubai）

designed by Nobuaki Tounaka
written  by Hinata Yanagi

The MIT License

Copyright (C) 2025 Universal Shell Programming Laboratory

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
    System.IO.hPutStr stderr "Usage    : divsen <f1> <f2> ... [<file>]\n"
    System.IO.hPutStr stderr "Version  : Mon Jan 20 17:18:16 JST 2025\n"
    System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD)\n"

main :: IO ()
main = do args <- getArgs
          case args of
              []            -> showUsage
              ["-h"]        -> showUsage
              ["--help"]    -> showUsage
              ["--version"] -> showUsage
              _             -> mainProc (setOpts args) 

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f = BS.readFile f

mainProc :: Opts -> IO ()
mainProc (Opts s fs file) = readF file >>= mainProc' s fs

mainProc' :: Char -> [Int] -> BS.ByteString -> IO ()
mainProc' s fs str = BS.putStr $ BS.unlines [ mainProc'' s fs ln | ln <- BS.lines str]

mainProc'' :: Char -> [Int] -> BS.ByteString -> BS.ByteString
mainProc'' s fs ln = BS.unwords [ fnc fs w | w <- zip [1..] (myWords ln)]
                   where fnc fs (n,w) = if filter ( == n) fs == [] 
                                        then w
                                        else BS.pack $ divsen s $ BS.unpack w

divsen :: Char -> String -> String
divsen 's' cs = printf "%f" $ (read cs::Double) / 1000
divsen s ('-':cs) = '-' : divsen s cs
divsen '0' cs = show m'
            where n = read (head $ splitOn "." cs)::Int
                  m = n `div` 1000
                  m' = if n - (m*1000) < 500 then m else m + 1


myWords :: BS.ByteString -> [BS.ByteString]
myWords line = filter (/= (BS.pack "")) $ BS.split ' ' line

data Opts = Opts Char [Int] String | Error String deriving Show

setOpts :: [String] -> Opts
setOpts as = case parse args "" ((Prelude.unwords as) ++ " ") of
                  Right opt -> opt
                  Left err -> Error ( show err ) 

args = do Opts <$> sflg <*> (many1 $ try field) <*> (try(filename) <|> return "-")

sflg = try(string "-s " >> return 's') <|> return '0'

field = do a <- many1 digit
           char ' '
           return (read a::Int)

filename = many1 ( try(letter) <|> try(digit) <|> symbol ) >>= return 

symbol = oneOf "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
