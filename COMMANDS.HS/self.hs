import qualified Data.ByteString.Lazy.Char8 as BS
import Codec.Binary.UTF8.String as CBUS
import System.Environment
import System.IO

-- incomplete version
-- cannot manipulate Japanese

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
		_ -> BS.getContents >>= putBSLines . makeOut (parseOpts args )

putBSLines :: [BS.ByteString] -> IO ()
putBSLines = putStr . CBUS.decodeString . BS.unpack . BS.unlines

parseOpts :: [String] -> [((Int,Int),(Int,Int))]
parseOpts args = [ parseOpt a | a <- args ]

parseOpt :: String -> ((Int,Int),(Int,Int))
parseOpt a = ( mainPart f , subPart s )
		where
			f = fst $ break (== '.') a
			s = snd $ break (== '.') a

mainPart :: String -> (Int,Int)
mainPart str = (f,s)
		where
			f = toNum $ fst $ break (== '/') str
			s = if s' == "" then f else toNum $ tail s'
			s' = snd $ break (== '/') str

subPart :: String -> (Int,Int)
subPart "" = (0, 0)
subPart str = ( toNum f , subPart' s )
	where
		f = fst $ break (== '.') $ tail str
		s = snd $ break (== '.') $ tail str
		subPart' :: String -> Int
		subPart' "" = 0
		subPart' lst = read ( tail lst )::Int

toNum :: String -> Int
toNum str = (if f == "NF" then -1 else read f::Int ) + ( if s == "" then 0 else read s::Int)
	where
		f = fst $ break (== '-') str
		s = snd $ break (== '-') str
	
makeOut :: [((Int,Int),(Int,Int))] -> BS.ByteString -> [BS.ByteString]
makeOut as cs = [ makeLine as (makeBaseStr c) | c <- BS.lines cs ]

makeBaseStr :: BS.ByteString -> [BS.ByteString]
makeBaseStr line = line : (BS.words line)

makeLine :: [((Int,Int),(Int,Int))] -> [BS.ByteString] -> BS.ByteString
makeLine as ws = BS.unwords [ choiceWord a ws | a <- as ]

choiceWord :: ((Int,Int),(Int,Int)) -> [BS.ByteString] -> BS.ByteString
choiceWord a ws = BS.unwords [ makeSubWord (snd a) (ws !! n) |  n <- makeSeq (fst a) (length ws) ]

makeSeq :: (Int,Int) -> Int -> [Int]
makeSeq a num = [x..y]
		where
			f = fst a
			s = snd a
			x = if f >= 0 then f else num + f
			y = if s >= 0 then s else num + s

makeSubWord :: (Int,Int) -> BS.ByteString -> BS.ByteString
makeSubWord (0,0) str = str
makeSubWord a str = cutTail (snd a) (cutFront (fst a) str)
		where 
			cutFront :: Int -> BS.ByteString -> [Char]
			cutFront n str = drop (n-1) (BS.unpack str)
			cutTail :: Int -> [Char] -> BS.ByteString
			cutTail n str = BS.pack $ take n str
