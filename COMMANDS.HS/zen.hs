#!/usr/bin/env runghc
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.ByteString.Lazy.Char8 as BS hiding (length,take,drop,filter,head,concat)
import Control.Applicative hiding ((<|>), many)
import Data.List as DL
import Data.Char
import Text.Printf

{--
zen（Open usp Tukubai）

designed by Nobuaki Tounaka
written  by Ryuichi Ueda

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
    System.IO.hPutStr stderr "Usage    : zen <f1> <f2> ... <file>\n"
    System.IO.hPutStr stderr "Version  : Thu Aug 15 23:31:59 JST 2013\n"
    System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n"

main :: IO ()
main = do args <- getArgs
          case args of
                ["-h"]     -> showUsage
                ["--help"] -> showUsage
                _         -> mainProc (setOpts args)

mainProc :: Opts -> IO ()
mainProc (Opts fs file) = readF file >>= mainProc' fs

mainProc' :: [Int] -> BS.ByteString -> IO ()
mainProc' fs cs = mainProc'' fs (BS.lines cs)

mainProc'' :: [Int] -> [BS.ByteString] -> IO ()
mainProc'' fs [] = do return ()
mainProc'' fs (ln:lns) = zen fs (myUWords ln) >> mainProc'' fs lns

type UWords = [BS.ByteString]
type UWord = BS.ByteString

zen :: [Int] -> UWords -> IO ()
zen [] wds = BS.putStrLn $ BS.unwords $ DL.map zen' wds 
zen fs wds = BS.putStrLn $ BS.unwords $ zenFs fs (DL.zip [1..] wds)

zenFs :: [Int] -> [(Int,UWord)] -> UWords
zenFs fs [] = []
zenFs fs ((n,w):ws) = (if x == [] then w else zen' w) : zenFs fs ws
        where x = filter (== n) fs

zen' :: UWord -> UWord
zen' w = BS.pack $ zen'' $ BS.unpack w

zen'' :: String -> String
zen'' [] = []
zen'' cs = cng ++ (if add == 0 then zen'' str else zen'' str2)
           where x = takeChar cs
                 c = fst x
                 str = snd x
                 next = toUnicode $ fst $ takeChar str
                 str2 = snd $ takeChar str
                 add = if next == 0xFF9E then 0x100000 else (if next == 0xFF9F then 0x200000 else 0)
                 cng = if length c == 1 || length c == 3
                       then (fromUnicode $ toZencode (add + (toUnicode c)))
                       else c

toUnicode :: String -> Int
toUnicode []         = 0
toUnicode (c:[])     = ord c
toUnicode (a:b:c:[]) = upper*256 + under
                    where upper = (oa `mod` 16)*16 + ((ob `div` 4) `mod` 16)
                          under = (ob `mod` 4)*64 + (oc `mod` 64)
                          oa = ord a
                          ob = ord b
                          oc = ord c
{--
toUnicode (a:b:[]) = upper*256 + under
                    where upper = (oa `div` 4) `mod` 8
                          under = (oa `mod` 4)*64 + (ob `mod` 64)
                          oa = ord a
                          ob = ord b
--}


fromUnicode :: Int -> String
fromUnicode n 
  | n < 128 = [chr n]
  | n < 0x800 = from2Byte n
  | n > 0x200000 = (fromUnicode (n - 0x200000)) ++ fromUnicode 0x309A
  | n > 0x100000 = (fromUnicode (n - 0x100000)) ++ fromUnicode 0x3099
  | otherwise = from3Byte n

from2Byte :: Int -> String
from2Byte n = DL.map chr [a,b]
                 where upper = n `div` 256
                       under = n `mod` 256
                       a = 0xC0 + (upper `mod` 8)*4 + (under `div` 64)
                       b = 128 + (under `mod` 64)

from3Byte :: Int -> String
from3Byte n = DL.map chr [a,b,c]
                 where upper = n `div` 256
                       under = n `mod` 256
                       a = 0xE0 + (upper `div` 16)
                       b = 128 + (upper `mod` 16)*4 + (under `div` 64)
                       c = 128 + (under `mod` 64)

punclist = [(0x3001,0xFF64),(0x3002,0xFF61),(0x300C,0xFF62),(0x300D,0xFF63)]
panalist = [(0x30D1,0xFF8A), (0x30D4,0xFF8B),(0x30D7,0xFF8C), (0x30DA,0xFF8D), (0x30DD,0xFF8E)]
ganalist = [(0x30AC,0xFF76), (0x30AE,0xFF77), (0x30B0,0xFF78),
           (0x30B2,0xFF79), (0x30B4,0xFF7A), (0x30B6,0xFF7B), (0x30B8,0xFF7C), (0x30BA,0xFF7D),
           (0x30BC,0xFF7E), (0x30BE,0xFF7F), (0x30C0,0xFF80), (0x30C2,0xFF81), (0x30C4,0xFF6F),
           (0x30C5,0xFF82), (0x30C7,0xFF83), (0x30C9,0xFF84), (0x30D0,0xFF8A), (0x30D3,0xFF8B),
           (0x30D6,0xFF8C), (0x30D9,0xFF8D), (0x30DC,0xFF8E), (0x30F4,0xFF73), (0x30F7,0xFF9C)]
kanalist = [(0x3099,0xFF9E), (0x309A,0xFF9F), (0x30A1,0xFF67), (0x30A2,0xFF71), (0x30A3,0xFF68),
           (0x30A4,0xFF72), (0x30A5,0xFF69), (0x30A6,0xFF73), (0x30A7,0xFF6A), (0x30A8,0xFF74),
           (0x30A9,0xFF6B), (0x30AA,0xFF75), (0x30AB,0xFF76), (0x30AD,0xFF77), (0x30AF,0xFF78),
           (0x30B1,0xFF79), (0x30B3,0xFF7A), (0x30B5,0xFF7B), (0x30B7,0xFF7C), (0x30B9,0xFF7D),
           (0x30BB,0xFF7E), (0x30BD,0xFF7F), (0x30BF,0xFF80), (0x30C1,0xFF81), (0x30C3,0xFF6F),
           (0x30C4,0xFF82), (0x30C6,0xFF83), (0x30C8,0xFF84), (0x30CA,0xFF85), (0x30CB,0xFF86),
           (0x30CC,0xFF87), (0x30CD,0xFF88), (0x30CE,0xFF89), (0x30CF,0xFF8A), (0x30D2,0xFF8B),
           (0x30D5,0xFF8C), (0x30D8,0xFF8D), (0x30DB,0xFF8E), (0x30DE,0xFF8F), (0x30DF,0xFF90),
           (0x30E0,0xFF91), (0x30E1,0xFF92), (0x30E2,0xFF93), (0x30E3,0xFF6C), (0x30E4,0xFF94),
           (0x30E5,0xFF6D), (0x30E6,0xFF95), (0x30E7,0xFF6E), (0x30E8,0xFF96), (0x30E9,0xFF97),
           (0x30EA,0xFF98), (0x30EB,0xFF99), (0x30EC,0xFF9A), (0x30ED,0xFF9B), (0x30EF,0xFF9C),
           (0x30F2,0xFF66), (0x30F3,0xFF9D), (0x30FB,0xFF65), (0x30FC,0xFF70)]
symblist = [(0xFFE0,0x00A2), (0xFFE1,0x00A3), (0xFFE2,0x00AC), (0xFFE3,0x00AF), (0xFFE4,0x00A6),
           (0xFFE5,0x00A5), (0xFFE6,0x20A9)]

toZencode :: Int -> Int
toZencode c
  | c >= 0x0020 && c <= 0x007E = c - 0x21 + 0xFF01
  | c == 0x2985                = 0xFF5F --Fullwidth brackets
  | c == 0x2986                = 0xFF60 --Fullwidth brackets
  | c >= 0xFF61 && c <= 0xFF64 = getList c punclist
  | c >= 0xFF65 && c <= 0xFF9F = getList c kanalist
  | c >= 0x200000 = getList (c - 0x200000) panalist
  | c >= 0x100000 = getList (c - 0x100000) ganalist
  | otherwise = c

getList :: Int -> [(Int,Int)] -> Int
getList n list = if ans == [] then n else fst $ head $ ans
    where f v (a,b) = v == b
          ans = filter (f n) list

takeChar :: String -> (String,String)
takeChar [] = ([],[])
takeChar (c:[]) = ([c],[])
takeChar (c:cs) 
  | ord c < 128 = ([c],cs) 
  | (ord c) `div` 32 ==  6 = take2Char (c:cs)
  | (ord c) `div` 16 == 14 = take3Char (c:cs)
  | (ord c) `div`  8 == 30 = take4Char (c:cs)

take2Char (a:b:c) = ((a:b:[]),c)
take3Char (a:b:c:d) = ((a:b:c:[]),d)
take4Char (a:b:c:d:e) = ((a:b:c:d:[]),e)

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f

myUWords :: BS.ByteString -> [BS.ByteString]
myUWords line = filter (/= BS.pack "") $ BS.split ' ' line

data Opts = Opts [Int] String | Error String deriving Show

setOpts :: [String] -> Opts
setOpts as = case parse args "" ((Prelude.unwords as) ++ " ") of
                  Right opt -> opt
                  Left err -> Error ( show err )

args = Opts <$> many num <*> (try(filename) <|> return "-")

num = do a <- many1 digit
         char ' '
         return (read a)

filename = many1 ( try(letter) <|> try(digit) <|> symbol ) >>= return

symbol = oneOf "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
