import qualified Data.ByteString.Lazy.Char8 as BS
import System.Environment
import System.IO

{--
self（Open usp Tukubai）

designed by Nobuaki Tounaka
written by Ryuichi Ueda
thanks to @master_q

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
		("Usage    : self <n...> <file>\n" ++ 
		"Wed Aug 15 19:29:08 JST 2012\n" ++
		"Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

main :: IO ()
main = do
	args <- getArgs
	case args of
		["-h"] -> showUsage
		["--help"] -> showUsage
		[] -> showUsage
		_ -> BS.getContents >>= putStr . BS.unpack . BS.unlines . makeOut args 
	
makeOut :: [String] -> BS.ByteString -> [BS.ByteString]
makeOut as cs = [ makeLine as c | c <- BS.lines (cs) ]

makeLine :: [String] -> BS.ByteString -> BS.ByteString
makeLine as ln = BS.unwords $ addOrder ([ w | w <- BS.words ln ] )

addOrder :: [BS.ByteString] -> [BS.ByteString]
addOrder wds = wds



