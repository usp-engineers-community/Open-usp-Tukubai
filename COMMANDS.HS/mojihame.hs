#!/usr/bin/env runghc
import System.IO
import System.Environment
import System.Exit
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List
import Data.List.Split

{--
loopx（Open usp Tukubai）

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
    System.IO.hPutStr stderr "Usage    : mojihame [-lLABEL] <template> [<data>]\n"
    System.IO.hPutStr stderr "Version  : Mon Jan 20 17:18:16 JST 2025\n"
    System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD+Mac)\n"
    exitWith (ExitFailure 1)

die str = System.IO.hPutStr stderr ( "Error[mojihame] : " ++ str ++ "\n")
          >> exitWith (ExitFailure 1)

main :: IO ()
main = do args <- getArgs
          case args of
              [tmpf]                       -> noopt tmpf "-"
              [tmpf,dataf]                 -> noopt tmpf dataf
              [('-':'l':label),tmpf,dataf] -> lopt (BS.pack label) tmpf dataf
              _          -> showUsage

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f

noopt :: String -> String -> IO()
noopt tmpf dataf = do t <- readF tmpf
                      d <- readF dataf
                      BS.putStr $ noopt' t (BS.words d)

noopt' :: BS.ByteString -> [BS.ByteString] -> BS.ByteString
noopt' template ws = BS.pack . concat $ map decode_underscores $ map (markToStr ws) (findPos $ BS.unpack template)

markToStr :: [BS.ByteString] -> (String,Int) -> String
markToStr ws (str,pos)
 | pos == -1        = str
 | pos > length ws  = str
 | otherwise        = str ++ BS.unpack (ws !! (pos-1))

findPos :: String -> [(String,Int)]
findPos [] = []
findPos template
 | b == []   = [(a,-1)]
 | e == True = (a ++ "%",-1) : findPos (drop 1 b)
 | n == []   = (a ++ "%",-1) : findPos (drop 1 b)
 | otherwise = (a,read n::Int) : findPos bb
    where a = takeWhile (/= '%') template
          b = dropWhile (/= '%') template
          e = escaped (reverse a)
          n = takeWhile (\x -> x >= '0' && x <= '9') (drop 1 b)
          bb = dropWhile (\x -> x >= '0' && x <= '9') (drop 1 b)

escaped :: String -> Bool
escaped []                 = False
escaped ('\\':[])          = True
escaped ('\\':'\\':revstr) = escaped revstr
escaped ('\\':_:revstr)    = True
escaped _                  = False

lopt :: BS.ByteString -> String -> String -> IO ()
lopt label tmpf dataf = do t <- readF tmpf
                           d <- readF dataf
                           lopt' (splitTemplate label t) (BS.lines d)

splitTemplate :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
splitTemplate lb t = map BS.unlines lst
    where lst = splitWhen (iio lb) (BS.lines t)
          iio a b = isInfixOf (BS.unpack a) (BS.unpack b)

lopt' :: [BS.ByteString] -> [BS.ByteString] -> IO ()
lopt' (pr:t:pt:_) ds = mapM_ BS.putStr $ [pr] ++ map (lopt'' t) ds ++ [pt]
    where lopt'' t d = noopt' t (BS.words d)

decode_underscores :: [Char] -> [Char]
decode_underscores [] = []
decode_underscores ('\\':'_':xs) = '_' : decode_underscores xs
decode_underscores ['_'] = []
decode_underscores ('_':xs) = ' ' : decode_underscores xs
decode_underscores (x:xs) = x : decode_underscores xs
