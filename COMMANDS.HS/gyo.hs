#!/usr/bin/env runghc
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Environment
import System.IO

{--
gyo（Open usp Tukubai）

designed by Nobuaki Tounaka
written  by Hinata Yanagi

The MIT License

Copyright (C) 2025 Universal Shell Programming Laboratory

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
    hPutStr stderr "Usage    : gyo [-f] [<file>]\n"
    hPutStr stderr "Version  : Mon Jan 20 17:18:16 JST 2025\n"
    hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD)\n"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> printCount
        ["-"]         -> printCount
        ("-f":as)     -> fmode as
        ["-h"]        -> showUsage
        ["--help"]    -> showUsage
        ["--version"] -> showUsage
        _ -> mmode args

printCount :: IO ()
printCount = BS.getContents >>= print . countLines

printCountF :: String -> IO ()
printCountF arg = BS.readFile arg >>= print . countLines

fmode :: [String] -> IO ()
fmode [] = putStr ""
fmode (arg:args) = do putStr ( arg ++ " " ) ; printCountF arg ; fmode args

mmode :: [String] -> IO ()
mmode [] = putStr ""
mmode (arg:args) = do printCountF arg; mmode args

countLines :: BS.ByteString -> Int
countLines bs = length $ BS.lines bs
