import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Char
import Control.Applicative hiding ((<|>)) 
import Data.ByteString.Lazy.Char8 as BS hiding (filter,head,last,map,zip,repeat,init,hPutStr)

{--
self（Open usp Tukubai）

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
showUsage = do hPutStr stderr
		("Usage    : self <f1> <f2> ... <file>\n" ++ 
		"Thu Jul 25 21:26:20 JST 2013\n" ++
		"Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

main :: IO ()
main = do args <- getArgs
	  case args of
		["-h"]     -> showUsage
		["--help"] -> showUsage
		"-d":as    -> directMode as
		_          -> readF (getFileName os) >>= mainProc (getFields os)
                                     where os = setOpts args

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f

------------
-- output --
------------

directMode :: [String] -> IO ()
directMode as = mainProc fs (BS.pack str) 
                where str = last as
                      fs = getFields $ setOpts (init as)

mainProc :: [Field] -> BS.ByteString -> IO ()
mainProc fs cs = BS.putStr $ BS.unlines [ lineProc nfs c nf | c <- BS.lines cs ]
                   where nf = Prelude.length $ myWords $ head ( BS.lines cs)
                         nfs = [ normalizeField f nf | f <- fs ]

myWords :: BS.ByteString -> [BS.ByteString]
myWords line = filter (/= x) $ BS.split ' ' line
               where x = BS.pack ""

normalizeField :: Field -> Int -> Field
normalizeField (SimpleField x) nf     = SimpleField (solveNF x nf)
normalizeField (Range x y) nf         = Range (solveNF x nf) (solveNF y nf)
normalizeField (SubField x y) nf      = SubField (solveNF x nf) y
normalizeField (SubSubField x y z) nf = SubSubField (solveNF x nf) y z

solveNF :: Int -> Int -> Int
solveNF x nf = if x >= 0 then x else x + nf + 1

lineProc :: [Field] -> BS.ByteString -> Int -> BS.ByteString
lineProc fs ln nf = BS.unwords [ getWords f ws | f <- fs ]
                    where ws = ln : (myWords ln)

getWords :: Field -> [BS.ByteString] -> BS.ByteString
getWords (SimpleField n) ws     = ws !! n
getWords (Range x y) ws         = BS.unwords $ Prelude.take (y-x+1) ( Prelude.drop x ws )
getWords (SubField x y) ws      = cutWord w y 0 where w = ws !! x
getWords (SubSubField x y z) ws = cutWord w y z where w = ws !! x

cutWord :: BS.ByteString -> Int -> Int -> BS.ByteString
cutWord str frm 0 = cutWordFrm (BS.unpack str) frm 0
cutWord str frm to = BS.pack $ cutWordTo x to 0 
                     where x = BS.unpack $ cutWordFrm (BS.unpack str) frm 0

cutWordFrm :: String -> Int -> Int -> BS.ByteString
cutWordFrm [] num cutted = error "wrong cut point"
cutWordFrm str num cutted = if cutted == num-1 then BS.pack str else cutWordFrm s num (cutted+n)
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
                      f' (SimpleField s) = show s
                      f' (Range s t) = (show s) ++ "/" ++ (show t)
                      f' (SubField s t) = (show s) ++ "." ++ (show t)
                      f' (SubSubField s t u) = (show s) ++ "." ++ (show t) ++ "." ++ (show u)

setOpts :: [String] -> [Option]
setOpts as = [ fnc a | a <- as ]
             where fnc str = case parse parseOption "" str of
                                  Right opt -> opt
                                  Left err -> Error ( show err ) 

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
