#!/usr/bin/env runghc
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Char
import Control.Applicative hiding ((<|>)) 
import Data.Text as T hiding (filter,head,last,map,zip,repeat,init)

{--
self（Open usp Tukubai）

designed by Nobuaki Tounaka
written  by Hinata Yanagi

The MIT License

Copyright (C) 2026 Universal Shell Programming Laboratory

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
    System.IO.hPutStr stderr "Usage    : self <f1> <f2> ... [<file>]\n"
    System.IO.hPutStr stderr "Version  : Sun Sep  6 01:13:37 JST 2026\n"
    System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD)\n"

main :: IO ()
main = do
    args <- getArgs
    case args of
      []            -> showUsage
      ["-h"]        -> showUsage
      ["--help"]    -> showUsage
      ["--version"] -> showUsage
      "-d":as       -> directMode as
      _             -> readF (getFileName os) >>= mainProc (getFields os)
                                     where os = setOpts args

readF :: String -> IO T.Text
readF "-" = do
  string <- getContents
  return $ T.pack string

readF f   = do
  string <- readFile f
  return $ T.pack string

------------
-- output --
------------

directMode :: [String] -> IO ()
directMode as = mainProc fs (T.pack str) 
                where str = last as
                      fs = getFields $ setOpts (init as)

mainProc :: [Field] -> T.Text -> IO ()
mainProc fs cs = putStr $ T.unpack $ T.unlines [ lineProc nfs c nf | c <- T.lines cs ]
                   where nf =
                           case T.lines cs of
                           [] -> error "field index exceeds the length"
                           x:xs -> Prelude.length $ myWords x
                         nfs = [ normalizeField f nf | f <- fs ]

myWords :: T.Text -> [T.Text]
myWords line = filter (/= x) $ T.split (\character -> character == ' ') line
               where x = T.pack ""

normalizeField :: Field -> Int -> Field
normalizeField (SimpleField x) nf     = SimpleField (solveNF x nf)
normalizeField (Range x y) nf         = Range (solveNF x nf) (solveNF y nf)
normalizeField (SubField x y) nf      = SubField (solveNF x nf) y
normalizeField (SubSubField x y z) nf = SubSubField (solveNF x nf) y z

solveNF :: Int -> Int -> Int
solveNF x nf = if x >= 0 then x else x + nf + 1

lineProc :: [Field] -> T.Text -> Int -> T.Text
lineProc fs ln nf = T.unwords [ getWords f ws | f <- fs ]
                    where ws = ln : (myWords ln)

getWords :: Field -> [T.Text] -> T.Text
getWords (SimpleField n) ws     =
  if Prelude.length ws > n then ws !! n else error "field index exceeds the length"
getWords (Range x y) ws         = T.unwords $ Prelude.take (y-x+1) ( Prelude.drop x ws )
getWords (SubField x y) ws      = cutWord w y 0 where w = ws !! x
getWords (SubSubField x y z) ws = cutWord w y z where w = ws !! x

cutWord :: T.Text -> Int -> Int -> T.Text
cutWord str frm 0 = cutWordFrm (T.unpack str) frm 0
cutWord str frm to = T.pack $ cutWordTo x to 0 
                     where x = T.unpack $ cutWordFrm (T.unpack str) frm 0

cutWordFrm :: String -> Int -> Int -> T.Text
cutWordFrm [] num cutted = error "wrong cut point"
cutWordFrm str num cutted = if cutted == num-1 then T.pack str else cutWordFrm s num (cutted+n)
                         where split = takeChar str
                               c = fst split
                               s = snd split
                               n = wc c

cutWordTo :: String -> Int -> Int -> String
cutWordTo str num cutted = if num == cutted then [] else c ++ (cutWordTo s num (cutted+n))
                         where split = takeChar str
                               c = fst split
                               s = snd split
                               n = wc c

takeChar :: String -> (String,String)
takeChar [] = error "wrong cut pos"
takeChar (c:[]) = ([c],[])
takeChar (c:a:[]) = if (ord c) < 128 then ([c],[a]) else error "not supported charcter"
takeChar (c:a:b:[]) = if (ord c) < 128 then ([c],(a:b:[])) else ((c:a:b:[]),[])
takeChar (c:a:b:cs) = if (ord c) < 128 then ([c],(a:b:cs)) else ((c:a:b:[]),cs)

wc :: String -> Int
wc []           = 0
wc (c:[])       = 1
wc (c:a:[])     = 2
wc (c:a:b:cs) = wc' $ ord c
                 where wc' n = if n < 128
                       then 1 + wc (a:b:cs)
                       else (hanzen ((ord a)*256+(ord b))) + wc cs
                       hanzen m = if m >= 0xBDA1 && m <= 0xBE9F then 1 else 2

-- 半角カナ：EFBDA1 ~ EFBE9F

------------------------
-- handling of options --
------------------------

getFileName :: [Option] -> String
getFileName ((FileName s):opts) = s
getFileName (opt:opts)          = getFileName opts
getFileName []                  = "-"

getFields :: [Option] -> [Field]
getFields ((Select a):opts) = a : getFields opts
getFields (opt:opts)       = getFields opts
getFields []               = []

------------------------
-- parsing of options --
------------------------

data Field = SimpleField Int 
            | Range Int Int 
            | SubField Int Int
            | SubSubField Int Int Int deriving Show

data Option = Select Field | FileName String | Error String deriving Show

showOpts :: [Option] -> IO ()
showOpts opts = print [ f opt | opt <- opts ]
                where f (Select x) = f' x
                      f (FileName x) = "file:" ++ x
                      f' (SimpleField s) = Prelude.show s
                      f' (Range s t) = (Prelude.show s) ++ "/" ++ (Prelude.show t)
                      f' (SubField s t) = (Prelude.show s) ++ "." ++ (Prelude.show t)
                      f' (SubSubField s t u) = (Prelude.show s) ++ "." ++ (Prelude.show t) ++ "." ++ (Prelude.show u)

setOpts :: [String] -> [Option]
setOpts as = [ fnc a | a <- as ]
             where fnc str = case parse parseOption "" str of
                                  Right opt -> opt
                                  Left err -> Error ( Prelude.show err ) 

parseOption :: Parser Option
parseOption = try(parseSelect) <|> try(parseFileName)

parseSelect :: Parser Option
parseSelect = Select <$> ( try(parseRange) <|> try(parseSubSubField) 
                        <|> try(parseSubField) <|> try(parseSimpleField) ) 

parseRange :: Parser Field
parseRange = do first <- parseSimpleField
                char '/'
                second <- parseSimpleField
                return ( Range (n first) (n second) )
                where n (SimpleField num) = num

parseSubField :: Parser Field
parseSubField = do first <- parseSimpleField
                   char '.'
                   second <- many1 digit
                   return $ SubField (n first) (read second)
                   where n (SimpleField num) = num

parseSubSubField :: Parser Field
parseSubSubField = do f <- parseSimpleField
                      char '.'
                      s <- many1 digit
                      char '.'
                      t <- many1 digit
                      return $ SubSubField (n f) (read s) (read t)
                      where n (SimpleField num) = num

parseSimpleField :: Parser Field
parseSimpleField = try(liftM (SimpleField . read) $ many1 digit)
               <|> try(parseNFM) <|> try(parseNF)


parseNFM :: Parser Field
parseNFM = do string "NF-"
              num <- many1 digit
              return $ SimpleField ( -1 * (read num) - 1 )

parseNF :: Parser Field
parseNF = string "NF" >> (return . SimpleField) (-1)

parseFileName :: Parser Option
parseFileName =  many1 ( letter <|> digit <|> symbol ) >>= return . FileName

symbol :: Parser Char
symbol = oneOf "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
