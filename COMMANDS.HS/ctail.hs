#!/usr/bin/env runghc --
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Environment
import System.IO

{--
ctail（Open usp Tukubai）

designed by Nobuaki Tounaka
written  by Hinata Yanagi

The MIT License

Copyright (C) 2022 Universal Shell Programming Laboratory

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
        System.IO.hPutStr stderr "Usage    : ctail [-f] <file>\n"
        System.IO.hPutStr stderr "Version  : Tue Apr 19 14:38:55 JST 2022\n"
        System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n"

main :: IO ()
main = do
    args <- getArgs
    case args of
                []         -> showUsage
                ["-h"]     -> showUsage
                [num]      -> readF "-"  >>= mainProc (readN num)
                [num,file] -> readF file >>= mainProc (readN num)
                _          -> showUsage

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f = BS.readFile f

readN :: String -> Int
readN str = (read str::Int) * (-1)

mainProc :: Int -> BS.ByteString -> IO ()
mainProc num str = proc num 0 (BS.lines str) []

proc :: Int -> Int -> [BS.ByteString] -> [BS.ByteString] -> IO ()
proc _ _ [] _ = do return ()
proc num 0 (ln:lns) []
  | num == 0  = BS.putStr $ BS.unlines lns
  | otherwise = proc num 1 lns [ln]
proc num keep (ln:lns) (k:ks)
  | num > keep  = proc num (keep+1) lns ((k:ks) ++ [ln])
  | num == keep = BS.putStrLn k >> proc num keep lns (ks ++ [ln])

