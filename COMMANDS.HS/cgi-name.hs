import System.Environment
import System.IO
import Numeric
import Data.Char
import Data.List.Split
import qualified Data.ByteString.Lazy.Char8 as BS 

{--
cgi-name（Open usp Tukubai）

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
showUsage = do System.IO.hPutStr stderr
		("Usage: cgi-name [-d<c>] [-i<c>]\n" ++ 
		"Sun Dec 29 13:34:08 JST 2013\n" ++
		"Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

main :: IO ()
main = do args <- getArgs
	  case args of
		["-h"]     -> showUsage
		["--help"] -> showUsage
		_          -> readF f >>= main' dstr istr
                    where opt = snd $ parseOpt (args,[" ","","-"])
                          dstr = opt !! 0
                          istr = opt !! 1
                          f    = opt !! 2

type Opt = [String] -- DString, IString, Filename

parseOpt :: ([String],Opt) -> ([String],Opt)
parseOpt ([],opt)                                 = ([],opt)
parseOpt ((('-':'d':str):args),[_,istr,filename]) = parseOpt (args,[str,istr,filename])
parseOpt ((('-':'i':str):args),[dstr,_,filename]) = parseOpt (args,[dstr,str,filename])
parseOpt ((str:args),[dstr,istr,_])               = parseOpt (args,[dstr,istr,str])

main' :: String -> String -> BS.ByteString -> IO ()
main' dstr istr = BS.putStr . BS.unlines . map (removeEq istr . BS.pack . normalize . decodeStr dstr . BS.unpack ) . BS.split '&'

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f

removeEq istr cs = BS.unwords $ [fst x,v']
    where x = BS.break ( == '=') cs
          v = BS.tail $ snd x
          v' = if v == (BS.pack "") then BS.pack istr else v

decodeStr :: String -> String -> String
decodeStr dstr []            = []
decodeStr dstr ['\n']        = []
decodeStr dstr (' ':str)     = dstr ++ decodeStr dstr str
decodeStr dstr ('%':a:b:str) = chr ((digitToInt a)*16 + digitToInt b) : decodeStr dstr str
decodeStr dstr (a:str)       = a : decodeStr dstr str

normalize []        = []
normalize ('+':str) = ' ' : normalize str
normalize (ch:str)  
  | n <= 8               = normalize str
  | n >= 11 && n <= 31   = normalize str
  | n == 127             = normalize str
  | otherwise            = ch : normalize str
    where n = ord ch
