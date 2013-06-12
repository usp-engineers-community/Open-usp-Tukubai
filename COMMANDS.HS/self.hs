import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Char

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
		"Wed Jun 12 22:14:11 JST 2013\n" ++
		"Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

main :: IO ()
main = do
	args <- getArgs
	case args of
		["-h"]     -> showUsage
		["--help"] -> showUsage
		"-d":as    -> directMode as
		_          -> do if f == "-"
                                     then getContents >>= mainProc fields
                                     else readFile f >>= mainProc fields
                                     where f = getFileName opts
                                           fields = getFields opts
                                           opts = setOpts args

------------
-- output --
------------

directMode :: [String] -> IO ()
directMode as = mainProc fields str
                where str = last as
                      fields = getFields opts
                      opts = setOpts (init as)

mainProc :: [Field] -> String -> IO ()
mainProc fs cs = putStr $ unlines [ lineProc nfs c nf | c <- lines cs ]
                   where nf = length $ words ( lines cs !! 0 )
                         nfs = [ normalizeField f nf | f <- fs ]

normalizeField :: Field -> Int -> Field
normalizeField (SimpleField x) nf = SimpleField (solveNF x nf)
normalizeField (Range x y) nf = Range (solveNF x nf) (solveNF y nf)
normalizeField (SubField x y) nf = SubField (solveNF x nf) y
normalizeField (SubSubField x y z) nf = SubSubField (solveNF x nf) y z

solveNF :: Int -> Int -> Int
solveNF x nf = if x >= 0 then x else x + nf + 1

lineProc :: [Field] -> String -> Int -> String
lineProc fs ln nf = unwords [ getWords f ws | f <- fs ]
                    where ws = ln : words ln

getWords :: Field -> [String] -> String
getWords (SimpleField n) ws = ws !! n
getWords (Range x y) ws = unwords $ take (y-x+1) ( drop x ws )
getWords (SubField x y) ws = cutWord w y 0 where w = ws !! x
getWords (SubSubField x y z) ws = cutWord w y z where w = ws !! x

widthCount :: String -> [Int]
widthCount cs = pileUp [ wc c | c <- cs ] 1
                  where
                       wc c = wc' (ord c)
                       wc' n = if n < 128 then 1 else (hanzen n)
                       hanzen m = if m >= 0xFF61 && m <= 0xFF9F then 1 else 2

pileUp :: [Int] -> Int -> [Int]
pileUp (n:[]) m = m : [m + n]
pileUp (n:ns) m = m : (pileUp ns (n + m))

cutWord :: String -> Int -> Int -> String
cutWord str frm 0 = drop x str
                      where x = if cutpos then error "bad cut position"
                                else length $ filter ( < frm ) wc
                            cutpos = filter (==frm) (init wc) == []
                            wc = widthCount str
cutWord str frm num = drop x (take y str)
                      where x = if cutpos then error "bad cut position"
                                else length $ filter ( < frm ) wc
                            y = if cutpos2 then error "bad length"
                                else length $ filter ( < to ) wc
                            cutpos = filter (==frm) (init wc) == []
                            cutpos2 = (filter (==to) wc) == [] 
                            to = frm + num
                            wc = widthCount str

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
            | SubSubField Int Int Int

data Option = Select Field | FileName String | Error String

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
parseSelect = do r <- ( try(parseRange) 
                        <|> try(parseSubSubField) 
                        <|> try(parseSubField) 
                        <|> try(parseSimpleField) )
                 return $ Select r

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
parseNF = string "NF" >> (return $ SimpleField (-1) )

parseFileName :: Parser Option
parseFileName =  many1 ( letter <|> digit <|> symbol ) >>= return . FileName

symbol :: Parser Char
symbol = oneOf "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
