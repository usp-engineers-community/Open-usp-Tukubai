import Data.List
import System.Environment
import System.IO

{--
yarr（Open usp Tukubai）

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
		("Usage    : yarr [num=<n>] [-<m>] <file>\n" ++ 
		"Mon Apr 29 16:57:49 JST 2013\n" ++
		"Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

main :: IO ()
main = do
	args <- getArgs
	case args of
		["-h"]     -> showUsage
		["--help"] -> showUsage
		[]         -> do unfoldAll
		["-"]      -> do unfoldAll
		_          -> do if (fileName args) == "-" 
					then getContents >>= mainProc (parseArgs args)
					else readFile (fileName args) >>= mainProc (parseArgs args)

splitAtKey :: Int -> [String] -> [ ([String], [String]) ]
splitAtKey num [] = []
splitAtKey num (c:cs) = splitAt num (words c) : splitAtKey num cs

mergeByKey :: ([String], [String]) -> [ ([String], [String]) ] -> [ ([String], [String]) ]
mergeByKey c [] = [c]
mergeByKey ([""],[""]) (r:rs) = mergeByKey r rs
mergeByKey c (r:rs) = if (fst c) == (fst r)
			then mergeByKey (fst c, (snd c)++(snd r)) rs
			else c : mergeByKey r rs

--単語ごとに全部改行して出力
unfoldAll = getContents >>= putStrLn . unfold
	where unfold cs = unwords $ concat [ words c | c <- lines cs ]

--yarr実行
mainProc :: (Int,Int) -> String -> IO()
mainProc (  0,0) cs = putStrLn . unwords $ concat [ words c | c <- lines cs ]
mainProc (  0,m) cs = putStr $ mUnfold m cs
--mainProc (num,m) cs = putStr $ keyUnfold num m $ lines cs
mainProc (num,0) cs = putStr . fold . mergeByKey ([""],[""]) $ splitAtKey num (lines cs)
mainProc (num,m) cs = putStr . mFold m $ mergeByKey ([""],[""]) $ splitAtKey num (lines cs)

-- -<m> オプションのときに1行をm単語ずつ折り返し
fold :: [ ([String], [String]) ] -> String
fold cs = unlines [ fold' (fst c) (snd c) | c <- cs ]
		where fold' k vs = (unwords k) ++ " " ++ (unwords vs) 

-- -<m> オプションのときに1行をm単語ずつ折り返し
mFold :: Int -> [ ([String], [String]) ] -> String
mFold m cs = unlines $ concat [ mFold' m (fst c) (snd c) | c <- cs ]

mFold' :: Int -> [String] -> [String] -> [String]
mFold' m k vs = [(unwords k) ++ " " ++ c | c <- mSet m vs ]

-- -<m> オプションのときに1行をm単語ずつ折り返し
mUnfold :: Int -> String -> String
--mUnfold m cs = unlines $ concat [ mSet m ( words c ) | c <- lines cs ]
mUnfold m cs = unlines $ mSet m ( words c )
	where c = unwords $ lines cs

-- 単語のリストから単語をm個ずつまとめたリストへ
mSet :: Int -> [ String ] -> [ String ]
mSet m [] = []
mSet m ws = (unwords $ fst s) : mSet m (snd s)
		where s = splitAt m ws 

-- キー(num=<n>)への対応
keyUnfold :: Int -> Int -> [ String ] -> String
keyUnfold num m cs = unlines $ concat [ keyExpand num m ( words c ) | c <- cs ]

-- オプションからファイル名取り出し
fileName :: [String] -> String
fileName as = if "num=" `isPrefixOf` lst || "-" `isPrefixOf` lst then "-" else lst
	where lst = last as

-- 1行のトークンをキーと値のペアの文字列にして出力
keyExpand :: Int -> Int -> [ String ] ->  [ String ]
keyExpand num m ws = [ unwords [ k , v ] | v <- vs ]
		where k = unwords $ take num ws
		      vs = mSet m (drop num ws)

-- オプションからパラメータ取り出し
parseArgs :: [String] -> (Int,Int)
parseArgs as = (getNum $ head0 $ filter findNum as, getM $ head1 $ filter findM as)
	where
		findNum a = "num=" `isPrefixOf` a
		getNum a = read (drop 4 a)::Int
		head0 :: [String] -> String
		head0 [] = "num=0"
		head0 as = head as

		findM a = "-" `isPrefixOf` a && length a > 1
		getM a = read (drop 1 a)::Int
		head1 :: [String] -> String
		head1 [] = "-0"
		head1 as = head as
