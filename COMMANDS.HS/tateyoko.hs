import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS

{--
tateyoko（Open usp Tukubai）

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
                ("Usage    : tateyoko <file>\n" ++ 
                "Wed Jun 12 22:37:46 JST 2013\n" ++
                "Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

main :: IO ()
main = do
        args <- getArgs
        case args of
            ["-h"]     -> showUsage
            ["--help"] -> showUsage
            _          -> do readF f >>= mainProc
                             where f = if (length args) > 0
                                       then args !! 0 else "-"

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f = BS.readFile f

mainProc :: BS.ByteString -> IO ()
mainProc cs = BS.putStr $ BS.unlines [ BS.unwords a | a <- arr ]
              where arr = tateyoko [ BS.words ln | ln <- BS.lines cs ] n
                    n = length $ BS.words $ ( BS.lines cs ) !! 0

tateyoko :: [[BS.ByteString]] -> Int -> [[BS.ByteString]]
tateyoko ws n = [ mtakes ws m | m <- [0..(n-1)] ]

mtakes :: [[BS.ByteString]] -> Int -> [BS.ByteString]
mtakes lns m = [ (ln !! m) | ln <- lns ]
