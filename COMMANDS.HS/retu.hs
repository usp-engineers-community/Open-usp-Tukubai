#!/usr/bin/env runghc --
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Environment
import System.IO

{--
retu（Open usp Tukubai）

designed by Nobuaki Tounaka
written  by Ryuichi Ueda

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
showUsage = do
    hPutStr stderr "Usage    : retu [-f] <file>\n"
    hPutStr stderr "Thu Aug 16 22:23:09 JST 2012\n"
    hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-h"] -> showUsage
        ["--help"] -> showUsage
        [] -> printCount
        ["-"] -> printCount
        ("-f":as) -> fmode as
        _ -> mmode args

fmode :: [String] -> IO ()
fmode [] = putStr ""
fmode (arg:args) = do printCountF arg ; fmode args

mmode :: [String] -> IO ()
mmode [] = putStr ""
mmode (arg:args) = do printCountM arg; mmode args

printCount :: IO ()
printCount = BS.getContents >>= putStr . unlines . toStr . countFile

printCountM :: String -> IO ()
printCountM arg = BS.readFile arg >>= putStr . unlines . toStr . countFile

printCountF :: String -> IO()
printCountF arg = do
            bs <- BS.readFile arg
            putStr $ unlines $ countFileF arg bs

countFileF :: String -> BS.ByteString -> [String]
countFileF arg bs = [ arg ++ " " ++ show(x) | x <- (countFile bs) ]

countFile :: BS.ByteString -> [Int]
countFile bs = unq $ [ length ( BS.split ' ' ln ) | ln <- (BS.lines bs) ]

unq :: [Int] -> [Int]
unq [n] = [n]
unq (n:ns) = if n == head ns then unq (ns) else n : unq (ns)

toStr :: [Int] -> [String]
toStr ns = [ show n | n <- ns ]
