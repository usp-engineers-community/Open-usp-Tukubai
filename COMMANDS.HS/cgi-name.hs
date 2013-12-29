import System.Environment
import System.IO
import Numeric
import Data.Char
import Data.List.Split
import qualified Data.ByteString.Lazy.Char8 as BS 

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
showUsage = do System.IO.hPutStr stderr
		("Usage: cgi-name \n" ++ 
		"Sun Dec 29 12:53:51 JST 2013\n" ++
		"Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

main :: IO ()
main = do args <- getArgs
	  case args of
		["-h"]     -> showUsage
		["--help"] -> showUsage
		[f]        -> readF f   >>= main'
		_          -> readF "-" >>= main'

main' :: BS.ByteString -> IO ()
main' = BS.putStr . BS.unlines . map (removeEq . BS.pack . normalize . decodeStr . BS.unpack ) . BS.split '&'

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f

removeEq cs = BS.unwords $ [fst x,BS.tail (snd x)]
    where x = BS.break ( == '=') cs

decodeStr :: String -> String
decodeStr []            = []
decodeStr ['\n']        = []
decodeStr ('%':a:b:str) = chr ((digitToInt a)*16 + digitToInt b) : decodeStr str
decodeStr (a:str)       = a : decodeStr str

normalize []        = []
normalize ('+':str) = ' ' : normalize str
normalize (ch:str)  
  | n <= 8               = normalize str
  | n >= 11 && n <= 31   = normalize str
  | n == 127             = normalize str
  | otherwise            = ch : normalize str
    where n = ord ch
