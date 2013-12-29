import System.Environment
import System.IO
import Data.Char

{--
keta（Open usp Tukubai）

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
		("Usage: keta n1 n2 .. <filename>\n" ++ 
		 "       keta -v <filename>\n" ++ 
		 "       keta -- <filename>\n" ++ 
		"Sun Dec 29 15:58:05 JST 2013\n" ++
		"Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

main :: IO ()
main = do args <- getArgs
          case args of 
              ["-h"]     -> showUsage
              ["--help"] -> showUsage
              ("-v":as)  -> do cs <- readF (getFilename as) 
                               let wlns = [ words ln | ln <- lines cs ]
                               let widths = map (wordsWid) wlns
                               putStrLn $ unwords $ map show $ head $ solveWid widths
              ("--":as)  -> main' True  $ parseOpt (Opt [] "-") as
              _          -> main' False $ parseOpt (Opt [] "-") args

main' :: Bool -> Opt -> IO ()
main' inv (Opt [] fname) = do cs <- readF fname
                              let wlns = [ words ln | ln <- lines cs ]
                              let widths = map (wordsWid) wlns
                              let outwid = head $ solveWid widths
                              let outwid' = if inv then map (\x -> x*(-1)) outwid else outwid
                              putStr $ unlines $ keta outwid' (zip widths wlns)
main' _ (Opt fs fname) = do cs <- readF fname
                            let wlns = [ words ln | ln <- lines cs ]
                            let widths = map (wordsWid) wlns
                            putStr $ unlines $ keta fs (zip widths wlns)

data Opt = Opt [Int] String

parseOpt :: Opt -> [String] -> Opt
parseOpt opt [] = opt
parseOpt (Opt fs fname) (a:as)
  | isDigits a                           = parseOpt (Opt (fs ++ [read a]) fname) as
  | head a == '-' && isDigits (drop 1 a) = parseOpt (Opt (fs ++ [read a]) fname) as
  | otherwise                            = parseOpt (Opt fs a) as

isDigits :: String -> Bool
isDigits [ch]    = ch >= '0' && ch <= '9'
isDigits []      = False
isDigits (ch:cs) = if ch >= '0' && ch <= '9' then isDigits cs else False

keta :: [Int] -> [([Int],[String])] -> [String]
keta _ [] = []
keta owidth ((iwidth,ws):wlns) = ketaLn owidth iwidth ws : keta owidth wlns

ketaLn :: [Int] -> [Int] -> [String] -> String
ketaLn [ow] [iw] [w]            = ketaW ow iw w
ketaLn (ow:ows) (iw:iws) (w:ws) = ketaW ow iw w ++ " " ++ ketaLn ows iws ws 
ketaLn [] [] []                 = []
ketaLn _ _ _                    = error "field num error"

ketaW ow iw w 
  | ow >= 0   = padr ++ w
  | otherwise = w ++ padl
    where padr = take (ow - iw)  $ repeat ' '
          padl = take (-ow - iw) $ repeat ' '

getFilename []       = "-"
getFilename (a:args) = a 

readF :: String -> IO String
readF "-" = getContents
readF f   = readFile f

wordsWid :: [String] -> [Int]
wordsWid ws = map wordWid ws

solveWid :: [[Int]] -> [[Int]]
solveWid (a:[])   = [a]
solveWid (a:b:ks) = solveWid $ (takeMaxes a b) : ks

takeMaxes :: [Int] -> [Int] -> [Int]
takeMaxes [] _ = []
takeMaxes _ [] = []
takeMaxes (a:as) (b:bs) = if a > b then a : takeMaxes as bs else b : takeMaxes as bs

wordWid w = sum $ map charWid w

charWid ch
  | n < 0xFF                   = 1
  | n >= 0xFF61 && n <= 0xFF91 = 1
  | otherwise = 2
    where n = ord ch
