#!/usr/bin/env runghc --
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Char
import qualified Data.List as DL (isInfixOf,isPrefixOf)
import Control.Applicative hiding ((<|>),many) 
import Data.ByteString.Lazy.Char8 as BS hiding (filter,head,last,map,zip,repeat,init,take,drop,length,elem)

{--
self（Open usp Tukubai）

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
    System.IO.hPutStr stderr "Usage   : dayslash [-r] <format> <field> <file>\n"
    System.IO.hPutStr stderr "Version : Tue Apr 19 14:44:33 JST 2022\n"
    System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n"

main :: IO ()
main = do args <- getArgs
          case args of
                []         -> showUsage
                ["-h"]     -> showUsage
                ["--help"] -> showUsage
                ["-r",fmt,fld,files] -> readF files >>= main' True fmt ((read fld) -1) . BS.lines
                ["-r",fmt,fld]       -> readF "-" >>= main' True fmt ((read fld) -1)  . BS.lines
                [fmt,fld,files]      -> readF files >>= main' False fmt ((read fld) -1) . BS.lines
                [fmt,fld]            -> readF "-" >>= main' False fmt ((read fld) -1) . BS.lines
                _                    -> showUsage

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f

main' :: Bool -> String -> Int -> [BS.ByteString] -> IO ()
main' _ _ _ [] = return ()
main' rev fmt fld (ln:lns) = main'' rev fmt fld ln >> main' rev fmt fld lns

main'' :: Bool -> String -> Int -> BS.ByteString -> IO ()
main'' rev fmt fld ln 
  | num == 1     = Prelude.putStrLn neww
  | fld == 0     = Prelude.putStr (neww ++ " ") >> BS.putStrLn (BS.unwords $ drop 1 ws)
  | fld == num-1 = BS.putStr (BS.unwords $ init ws) >> Prelude.putStrLn (' ' : neww)
  | otherwise    = BS.putStr (BS.unwords $ take fld ws) >> 
                   Prelude.putStr (" " ++ neww ++ " ") >>
                   BS.putStrLn (BS.unwords $ drop (fld+1) ws)
    where ws = myWords ln
          w = BS.unpack $ ws !! fld
          neww = f $ if rev then unfmt pfmt w else format pfmt dt
          num = length ws
          pfmt = snd $ parseFormat (fmt,[])
          dt = mkDayTime w 
          f "" = w
          f s = s

parseFormat :: (String,Format) -> (String,Format)
parseFormat ([],fmt) = ([],fmt)
parseFormat (str,fmt)
  | "yyyy" `DL.isPrefixOf` str = parseFormat (drop 4 str,fmt++["yyyy"])
  | "yy"   `DL.isPrefixOf` str = parseFormat (drop 2 str,fmt++["yy"])
  | "mm"   `DL.isPrefixOf` str = parseFormat (drop 2 str,fmt++["mm"])
  | "m"    `DL.isPrefixOf` str = parseFormat (drop 1 str,fmt++["m"])
  | "dd"   `DL.isPrefixOf` str = parseFormat (drop 2 str,fmt++["dd"])
  | "d"    `DL.isPrefixOf` str = parseFormat (drop 1 str,fmt++["d"])
  | "HH"   `DL.isPrefixOf` str = parseFormat (drop 2 str,fmt++["HH"])
  | "H"    `DL.isPrefixOf` str = parseFormat (drop 1 str,fmt++["H"])
  | "MM"   `DL.isPrefixOf` str = parseFormat (drop 2 str,fmt++["MM"])
  | "M"    `DL.isPrefixOf` str = parseFormat (drop 1 str,fmt++["M"])
  | "SS"   `DL.isPrefixOf` str = parseFormat (drop 2 str,fmt++["SS"])
  | "S"    `DL.isPrefixOf` str = parseFormat (drop 1 str,fmt++["S"])
  | otherwise                  = parseFormat (drop 1 str,fmt++[[head str]])


unfmt :: Format -> String -> String
unfmt fmt str = buildStr fmt (splitDayStr str)

splitDayStr :: String -> [String]
splitDayStr [] = [[]]
splitDayStr str = fst x : splitDayStr (snd x)
    where x = splitNumNonNum ([],str)

splitNumNonNum :: (String,String) -> (String,String)
splitNumNonNum (a,[]) = (a,[])
splitNumNonNum ([],(ch:str)) = splitNumNonNum ([ch],str)
splitNumNonNum (a,(ch:str)) 
  | ch >= '0' && ch <= '9' && ha >= '0' && ha <= '9' = splitNumNonNum (a ++ [ch],str)
  | (ch < '0' || ch > '9') && (ha < '0' || ha > '9') = splitNumNonNum (a ++ [ch],str)
  | otherwise = (a,ch:str)
    where ha = head a

buildStr :: Format -> [String] -> String
buildStr fmt ws = printDay simplefmt $ buildStr' simplefmt ws (DayTime "0000" "00" "00" "00" "00" "00")
    where f "yyyy" = "y"
          f "mm" = "m"
          f "dd" = "d"
          f "HH" = "H"
          f "MM" = "M"
          f "SS" = "S"
          f s = s
          simplefmt = map f fmt

printDay :: Format -> DayTime -> String
printDay fmt (DayTime y m d h min s) 
  | (not $ elem "H" fmt) = y ++ m ++ d
  | (not $ elem "y" fmt) = h ++ min ++ s
  | (not $ elem "S" fmt) = y ++ m ++ d ++ h ++ min
  | otherwise = y ++ m ++ d ++ h ++ min ++ s


buildStr' :: Format -> [String] -> DayTime -> DayTime
buildStr' _ [] dt = dt
buildStr' [] _ dt = dt
buildStr' (f:fs) (w:ws) (DayTime y m d h min s)
  | f == "y" = buildStr' fs ws (DayTime w m d h min s)
  | f == "m" = buildStr' fs ws (DayTime y (g w) d h min s)
  | f == "d" = buildStr' fs ws (DayTime y m (g w) h min s)
  | f == "H" = buildStr' fs ws (DayTime y m d (g w) min s)
  | f == "M" = buildStr' fs ws (DayTime y m d h (g w) s)
  | f == "S" = buildStr' fs ws (DayTime y m d h min (g w))
  | otherwise = buildStr' fs ws (DayTime y m d h min s)
    where g x = if length x == 1 then '0' : x else x

isNumPart :: String -> Bool
isNumPart (ch:str) = ch == 'y' || ch == 'm' || ch == 'd' || ch == 'H' || ch == 'M' || ch == 'S'

cutNumPart :: (String,String) -> (String,String)
cutNumPart (a,(ch:str)) = if ch >= '0' && ch <= '9' then cutNumPart (a++[ch],str) else (a,(ch:str))

data DayTime = DayTime String String String String String String | NotDate String

type Format = [String]

format :: Format -> DayTime -> String
format _ (NotDate s) = s
format fs dt = if elem Nothing a then "" else  f a
    where a = map (replaceFmt dt) fs
          f [] = []
          f (Just s:as) = s ++ f as

replaceFmt :: DayTime -> String -> Maybe String
replaceFmt (DayTime "" _ _ _ _ _) ('y':str) = Nothing
replaceFmt (DayTime y _ _ _ _ _)  "yyyy"    = Just y
replaceFmt (DayTime y _ _ _ _ _)  "yy"      = Just $ drop 2 y
replaceFmt (DayTime _ "" _ _ _ _) ('m':str) = Nothing
replaceFmt (DayTime _ m _ _ _ _)  "mm"      = Just m
replaceFmt (DayTime _ m _ _ _ _)  "m"      = if head m == '0' then Just (drop 1 m) else Just m
replaceFmt (DayTime _ _ "" _ _ _) ('s':str) = Nothing
replaceFmt (DayTime _ _ d _ _ _)  "dd"      = Just d
replaceFmt (DayTime _ _ d _ _ _)  "d"      = if head d == '0' then Just (drop 1 d) else Just d
replaceFmt (DayTime _ _ _ "" _ _) ('H':str) = Nothing
replaceFmt (DayTime _ _ _ h _ _)  "HH"      = Just h
replaceFmt (DayTime _ _ _ h _ _)  "H"      = if head h == '0' then Just (drop 1 h) else Just h
replaceFmt (DayTime _ _ _ _ "" _) ('M':str) = Nothing
replaceFmt (DayTime _ _ _ _ m _)  "MM"      = Just m
replaceFmt (DayTime _ _ _ _ m _)  "M"      = if head m == '0' then Just (drop 1 m) else Just m
replaceFmt (DayTime _ _ _ _ _ "") ('S':str) = Nothing
replaceFmt (DayTime _ _ _ _ _ s)  "SS"      = Just s
replaceFmt (DayTime _ _ _ _ _ s)  "S"      = if head s == '0' then Just (drop 1 s) else Just s
replaceFmt _ fmt = Just fmt

mkDayTime :: String -> DayTime
mkDayTime w 
  | lw == 8 = DayTime (take 4 w) (f 2 4 w) (drop 6 w) "" "" ""
  | lw == 6 = DayTime "" "" "" (take 2 w) (f 2 2 w) (drop 4 w)
  | lw == 14 = DayTime (take 4 w) (f 2 4 w) (f 2 6 w) (f 2 8 w) (f 2 10 w) (f 2 12 w)
  | otherwise = NotDate w
    where lw = length w
          f t d w = take t $ drop d w

myWords :: BS.ByteString -> [BS.ByteString]
myWords line = filter (/= x) $ BS.split ' ' line
               where x = BS.pack ""
