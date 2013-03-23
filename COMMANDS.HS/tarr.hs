import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List
import Codec.Binary.UTF8.String as CBUS
import System.Environment
import System.IO

-- incomplete version
-- cannot manipulate Japanese

{--
tarr（Open usp Tukubai）

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
		("Usage    : tarr [num=<n>] [-<m>] <file>\n" ++ 
		"Sat Mar 23 21:53:43 JST 2013\n" ++
		"Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

main :: IO ()
main = do
	args <- getArgs
	case args of
		["-h"]     -> showUsage
		["--help"] -> showUsage
		[]         -> do foldAll
		["-"]      -> do foldAll
		_          -> do if (fileName args) == "-" 
					then BS.getContents >>= mainProc (parseArgs args)
					else BS.readFile (fileName args) >>= mainProc (parseArgs args)

--単語ごとに全部改行して出力
foldAll = BS.getContents >>= putBSLines . fold
	where fold cs = BS.unlines $ concat [ BS.words c | c <- BS.lines cs ]

--UTF-8の出力のお約束
putBSLines :: BS.ByteString -> IO ()
putBSLines = putStr . CBUS.decodeString . BS.unpack 

--tarr実行
mainProc :: (Int,Int) -> BS.ByteString -> IO()
mainProc (  0,m) cs = putBSLines $ mFold m cs
mainProc (num,m) cs = putBSLines $ keyFold num m $ BS.lines cs

-- -<m> オプションのときに1行をm単語ずつ折り返し
mFold :: Int -> BS.ByteString -> BS.ByteString
mFold m cs = BS.unlines $ concat [ mSet m ( BS.words c ) | c <- BS.lines cs ]

-- 単語のリストから単語をm個ずつまとめたリストへ
mSet :: Int -> [ BS.ByteString ] -> [ BS.ByteString ]
mSet m [] = []
mSet m ws = (BS.unwords $ fst s) : mSet m (snd s)
		where s = splitAt m ws 

-- キー(num=<n>)への対応
keyFold :: Int -> Int -> [ BS.ByteString ] -> BS.ByteString
keyFold num m cs = BS.unlines $ concat [ keyExpand num m ( BS.words c ) | c <- cs ]

-- オプションからファイル名取り出し
fileName :: [String] -> String
fileName as = if "num=" `isPrefixOf` lst || "-" `isPrefixOf` lst then "-" else lst
	where lst = last as

-- 1行のトークンをキーと値のペアの文字列にして出力
keyExpand :: Int -> Int -> [ BS.ByteString ] ->  [ BS.ByteString ]
keyExpand num m ws = [ BS.unwords [ k , v ] | v <- vs ]
		where k = BS.unwords $ take num ws
		      vs = mSet m (drop num ws)

-- オプションからパラメータ取り出し
parseArgs :: [String] -> (Int,Int)
parseArgs as = (getNum $ head0 $ filter findNum as, getM $ head1 $ filter findM as)
	where
		findNum a = "num=" `isPrefixOf` a
		getNum a = read (drop 4 a)::Int
		head0 :: [String] -> String
		head0 [] = "00000"
		head0 as = head as

		findM a = "-" `isPrefixOf` a && length a > 1
		getM a = read (drop 1 a)::Int
		head1 :: [String] -> String
		head1 [] = "01"
		head1 as = head as
