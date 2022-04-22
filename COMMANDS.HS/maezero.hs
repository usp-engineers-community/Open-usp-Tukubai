#!/usr/bin/env runghc --
import System.Environment
import System.IO
import Data.List.Split hiding (oneOf)
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.ByteString.Lazy.Char8 as BS hiding (filter,last,zip,drop,reverse,concat,length,take,head,splitAt,tail,repeat)
import Control.Applicative hiding ((<|>), many)
import Text.Printf

{--
maezero（Open usp Tukubai）

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
showUsage = do System.IO.hPutStr stderr ("Usage    : maezero <f1.k1> <f2.k2> ... <file>\n" ++ 
                "Version  : Tue Jul 30 15:06:18 JST 2013\n" ++
                "Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

type OneLine = BS.ByteString
type AllLines = BS.ByteString
type UWord = BS.ByteString

main :: IO ()
main = do args <- getArgs
          case args of
              []         -> showUsage
              ["-h"]     -> showUsage
              ["--help"] -> showUsage
              _          -> mainProc (setOpts args) 

mainProc :: Opts -> IO ()
mainProc (Opts fs file) = readF file >>= mainProc' fs

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f = BS.readFile f

mainProc' :: [Field] -> AllLines -> IO ()
mainProc' fs str = BS.putStr $ BS.unlines [ maezero fs ln | ln <- BS.lines str]

maezero :: [Field] -> OneLine -> OneLine
maezero fs ln = BS.unwords [ maezero' fs w | w <- ws ]
                   where ws = zip [1..] $ myUWords ln

maezero' :: [Field] -> (Int,UWord) -> UWord
maezero' fs (pos,word) = maezero'' f word
                    where f = pickOp fs pos

maezero'' :: Maybe Field -> UWord -> UWord
maezero'' Nothing            w = w
maezero'' (Just (Field _ n)) w = BS.pack $ (take (n - (length ww)) (repeat '0')) ++ ww
                                 where ww = BS.unpack w

pickOp :: [Field] -> Int -> Maybe Field
pickOp fs pos = if length x == 0 then Nothing else Just $ Prelude.head x
               where f pos (Field p _) = p == pos
                     x = filter (f pos) fs

myUWords :: BS.ByteString -> [BS.ByteString]
myUWords line = filter (/= (BS.pack "")) $ BS.split ' ' line

data Opts = Opts [Field] String | Error String deriving Show
data Field = Field Int Int deriving Show

setOpts :: [String] -> Opts
setOpts as = case parse args "" ((Prelude.unwords as) ++ " ") of
                  Right opt -> opt
                  Left err -> Error ( show err ) 

args = do Opts <$> (many1 $ try field) <*> (try(filename) <|> return "-")

field = do a <- many1 digit
           char '.'
           b <- many1 digit
           char ' '
           return $ Field (read a::Int) (f b) 
           where f "0"      = 0
                 f ('0':cs) = (read cs::Int) * (-1)
                 f cs       = read cs::Int

filename = many1 ( try(letter) <|> try(digit) <|> symbol ) >>= return 

symbol = oneOf "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
