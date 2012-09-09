import qualified Data.ByteString.Lazy.Char8 as BS
import Codec.Binary.UTF8.String as CBUS
import System.Environment
import System.IO
import Data.List

-- incomplete version
-- cannot manipulate Japanese

{--
delf（Open usp Tukubai）

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
		("Usage    : delf <n...> <file>\n" ++ 
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

parseOpts :: [String] -> [(Int,Int)]
parseOpts as = [ parseSlash a | a <- as ]

parseSlash :: String -> (Int,Int)
parseSlash a = ( toNum f , toNum s ) 
		where
			f = fst $ break (== '/') a
			s = if s' == "" then f else tail s'
			s' = snd $ break (== '/') a

toNum :: String -> Int
toNum str = (if f == "NF" then -1 else read f::Int ) + ( if s == "" then 0 else read s::Int)
	where
		f = fst $ break (== '-') str
		s = snd $ break (== '-') str
	
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

makeOut :: [(Int,Int)] -> BS.ByteString -> [BS.ByteString]
makeOut as cs = [ makeLine as c | c <- BS.lines cs ]

makeLine :: [(Int,Int)] -> BS.ByteString -> BS.ByteString
makeLine as ln = selectWords (fieldList as fs) (BS.words ln)
			where fs = length $ BS.words ln

selectWords :: [Int] -> [BS.ByteString] -> BS.ByteString
selectWords fs ws = BS.unwords [ ws !! f | f <- fs ]

fieldList :: [(Int,Int)] -> Int -> [Int]
fieldList ds nf = fieldInv (fieldMake ds nf) nf

fieldInv :: [Int] -> Int -> [Int]
fieldInv fs nf = [0..(nf-1)] \\ fs

fieldMake :: [(Int,Int)] -> Int -> [Int]
fieldMake [] _ = []
fieldMake (d:ds) nf = [x..y] ++ fieldMake ds nf
		where
			f = fst d
			s = snd d
			x = if f > 0 then f-1 else nf + f
			y = if s > 0 then s-1 else nf + s
