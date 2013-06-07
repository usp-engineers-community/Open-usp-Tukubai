import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Time
import Data.Time.Format (formatTime)
import Data.Time.Format (parseTime)
import System.Locale (defaultTimeLocale)

{--
calclock（Open usp Tukubai）

designed by USP lab.
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
		("Usage    : calclock <f1> <f2> ... <file>\n" ++ 
		"Sat Jun  8 08:08:49 JST 2013\n" ++
		"Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

main :: IO ()
main = do
	args <- getArgs
	case args of
		["-h"]     -> showUsage
		["--help"] -> showUsage
		_          -> do if f == "-"
                                     then getContents >>= mainProc opt
                                     else readFile f >>= mainProc opt
                                          where opt = setOpts args
                                                f = getFileName opt

revMode :: [Option] -> Bool
revMode ((Rev n):ops) = True
revMode ops = False

extFields :: [Option] -> [Int]
extFields ((Field n):ops) = n : extFields ops
extFields (_:ops) = extFields ops
extFields [] = []

getFileName :: [Option] -> String
getFileName ((FileName s):opts) = s
getFileName (opt:opts)          = getFileName opts
getFileName []                  = "-"

mainProc :: [Option] -> String -> IO ()
mainProc as cs = putStr $ unlines [ lineProc fs c flg | c <- lines cs ]
                           where fs = extFields as
                                 flg = if revMode as then 'r' else 'n'

lineProc :: [Int] -> String -> Char -> String
lineProc ops ln flg = unwords [ wordProc ops w flg | w <- nln ]
                      where nln = zip [1..] (words ln)

wordProc :: [Int] -> (Int,String) -> Char -> String
wordProc ops (n,str) flg = if length ( filter (==n) ops ) == 0
                           then str else ans
                           where ans = if flg == 'n'
                                 then normal str else rev str

normal :: String -> String
normal str = str ++ " " ++ (getAns (length str) str)

getAns :: Int -> String -> String
getAns 8 = toStr . normal8
getAns 12 = toStr . normal12
getAns 14 = toStr . normal14

normal8 :: String -> Maybe UTCTime
normal8 str = parseTime defaultTimeLocale "%Y%m%d" str :: Maybe UTCTime

normal12 :: String -> Maybe UTCTime
normal12 str = parseTime defaultTimeLocale "%Y%m%d%H%M" str :: Maybe UTCTime

normal14 :: String -> Maybe UTCTime
normal14 str = parseTime defaultTimeLocale "%Y%m%d%H%M%S" str :: Maybe UTCTime

toStr :: Maybe UTCTime -> String
toStr (Just s) = formatTime defaultTimeLocale "%s" s
toStr Nothing = [""] !! 2

rev :: String -> String
rev str = str ++ " " ++ toStrRev (parseTime defaultTimeLocale "%s" str :: Maybe UTCTime )

toStrRev :: Maybe UTCTime -> String
toStrRev (Just s) = formatTime defaultTimeLocale "%Y%m%d%H%M%S" s
toStrRev Nothing = ["abc"] !! 10

--formatTime defaultTimeLocale "%Y-%m-%d" time

--parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2011-01-12 13:30:22" :: Maybe ZonedTime

data Option = Field Int | FileName String | Rev Bool | Error String

setOpts :: [String] -> [Option]
setOpts as = [ fnc a | a <- as ]
             where fnc str = case parse parseOpt "" str of
                                  Right opt -> opt
                                  Left err -> Error ( show err )

parseOpt :: Parser Option
parseOpt = try(parseROpt) <|> try(parseField) <|> try(parseFileName)

parseROpt :: Parser Option
parseROpt = string "-r" >> (return $ Rev True)

parseField :: Parser Option 
parseField = liftM (Field . read) $ many1 digit

parseFileName :: Parser Option
parseFileName =  many1 ( letter <|> digit <|> symbol ) >>= return . FileName

symbol :: Parser Char
symbol = oneOf "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
