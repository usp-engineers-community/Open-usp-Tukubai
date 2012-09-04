import qualified Data.ByteString.Lazy.Char8 as BS
import System.Environment
import System.IO

-- incomplete version
-- cannot manipulate Japanese
-- cannot execute sub string operation

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
		_ -> BS.getContents >>= putStr . unlines . makeOut (parseOpts args )

parseOpts :: [String] -> [(Int,(Int,Int))]
parseOpts args = [ parseOpt a | a <- args ]

parseOpt :: String -> (Int,(Int,Int))
parseOpt a = ( toInt $ fst $ brk , subPart $ snd $ brk )
		where
			brk = break (== '.') a

subPart :: String -> (Int,Int)
subPart "" = (0, 0)
subPart s = ( toInt $ fst $ b , subPart' $ snd $ b )
	where
		b = break (== '.') $ tail s
		subPart' :: String -> Int
		subPart' "" = 0
		subPart' last = toInt $ tail last

toInt :: String -> Int
toInt "NF" = -1
toInt n = read n::Int
	
makeOut :: [(Int,(Int,Int))] -> BS.ByteString -> [String]
makeOut as cs = [ BS.unpack $ makeLine as (makeBaseStr c) | c <- BS.lines cs ]

makeBaseStr :: BS.ByteString -> [BS.ByteString]
makeBaseStr line = line : (BS.words line)

makeLine :: [(Int,(Int,Int))] -> [BS.ByteString] -> BS.ByteString
makeLine as ws = BS.unwords [ choiceWord a ws | a <- as ]

choiceWord :: (Int,(Int,Int)) -> [BS.ByteString] -> BS.ByteString
choiceWord a ws =  if n == -1 then last ws else ws !! n
			where n = fst a 
