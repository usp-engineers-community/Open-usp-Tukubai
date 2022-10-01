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
marume（Open usp Tukubai）

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
    System.IO.hPutStr stderr "Usage    : marume [+age|-sage] <f1.k1> <f2.k2> ... [<file>]\n"
    System.IO.hPutStr stderr "Version  : Sat Oct  1 21:43:34 JST 2022\n"
    System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD)\n"

type OneLine = BS.ByteString
type AllLines = BS.ByteString
type UWord = BS.ByteString

main :: IO ()
main = do args <- getArgs
          case args of
              []            -> showUsage
              ["-h"]        -> showUsage
              ["--help"]    -> showUsage
              ["--version"] -> showUsage
              _             -> mainProc (setOpts args)

mainProc :: Opts -> IO ()
mainProc (Opts flg fs file) = readF file >>= mainProc' flg fs
mainProc _  = showUsage

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f = BS.readFile f

mainProc' :: Char -> [Field] -> AllLines -> IO ()
mainProc' flg fs str = BS.putStr $ BS.unlines [ marume flg fs ln | ln <- BS.lines str]

marume :: Char -> [Field] -> OneLine -> OneLine
marume flg fs ln = BS.unwords [ marume' flg fs w | w <- ws ]
                   where ws = zip [1..] $ myUWords ln

marume' :: Char -> [Field] -> (Int,UWord) -> UWord
marume' flg fs (pos,word) = marume'' flg f word
                    where f = pickOp fs pos

marume'' :: Char -> Maybe Field -> UWord -> UWord
marume'' _   Nothing            w = w
marume'' flg (Just (Field _ n)) w = BS.pack $ signCheck $ sign ++ (zeroCheck $ revPoint n sftn)
                        where sft = shiftPoint n (splitOn "." $ dropSign ww)
                              sftn = show $ cutFrac flg sft
                              sign = takeSign str
                              ww = if sciForm str then str else printf "%f" (read str::Double)
                              str = BS.unpack w

sciForm str = filter (== 'e') str == [] && filter (== 'E') str == []

revPoint :: Int -> String -> String
revPoint n str 
 | n == 0 = str
 | n > 0  = if m > 0 then (take m str) ++ "." ++ (drop m str) else "0." ++ (take ((-1)*m) $ repeat '0') ++ str
 | n < 0  = str ++ (take ((-1)*n) $ repeat '0')
            where m = (length str) - n

cutFrac :: Char -> (String,String) -> Int
cutFrac c   ([],a)   = cutFrac c ("0",a)
cutFrac _   (n,[])    = read n
cutFrac '0' (n,(a:b)) = if read [a] < 5 then read n else  1 + read n
cutFrac '-' (n,_)     = read n
cutFrac '+' (n,f)     = if read f > 0 then 1 + read n else read n

takeSign :: String -> String
takeSign ('-':w) = "-"
takeSign ('+':w) = "+"
takeSign w = ""

dropSign :: String -> String
dropSign ('-':w) = w
dropSign ('+':w) = w
dropSign w = w

shiftPoint :: Int -> [String] -> (String,String)
shiftPoint n (left:[]) = shiftPoint' n left []
shiftPoint n (left:right:[]) = shiftPoint' n left right

shiftPoint' :: Int -> String -> String -> (String,String)
shiftPoint' n left right 
 | n == 0 = (left, right)
 | n > 0  = (left ++ (takez n right) , (drop n right) )
 | n < 0  = (take ((length left) + n) left, (dropz ((length left) + n) left) ++ right)
               
takez :: Int -> String -> String
takez n str = take n (str ++ (repeat '0'))

dropz :: Int -> String -> String
dropz n str = if n < 0 then take ((-1)*n) (repeat '0') ++ str else drop n str

signCheck :: String -> String
signCheck ('-':cs) = if x == 0.0 then cs else ('-':cs)
                     where x = read cs::Double
signCheck str      = str

zeroCheck :: String -> String
zeroCheck str = if length str == length (filter (== '0') str) then "0" else str

pickOp :: [Field] -> Int -> Maybe Field
pickOp fs pos = if length x == 0 then Nothing else Just $ Prelude.head x
               where f pos (Field p _) = p == pos
                     x = filter (f pos) fs

myUWords :: BS.ByteString -> [BS.ByteString]
myUWords line = filter (/= (BS.pack "")) $ BS.split ' ' line

data Opts = Opts Char [Field] String | Error String deriving Show
data Field = Field Int Int deriving Show

setOpts :: [String] -> Opts
setOpts as = case parse args "" ((Prelude.unwords as) ++ " ") of
                  Right opt -> opt
                  Left err -> Error ( show err ) 

args = do Opts <$> agesage <*> (many1 $ try field) <*> (try(filename) <|> return "-")

agesage = try(string "+age " >> return '+') <|> 
          try(string "-sage " >> return '-') <|>
          return '0'

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
