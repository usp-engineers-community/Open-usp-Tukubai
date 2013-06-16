import System.Environment
import System.IO
import Data.List

{--
filehame（Open usp Tukubai）

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
showUsage = do hPutStr stderr ("Usage    : filehame <-lSTRING> <file1> <file2>\n" ++ 
                "Sun Jun 16 17:52:08 JST 2013\n" ++
                "Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

main :: IO ()
main = do args <- getArgs
          case args of
                (label:f1:f2:[]) -> do cs <- readF f1
                                       ins <- readF f2
                                       mainProc (drop 2 label) (lines cs) ins
                _                -> showUsage

mainProc :: String -> [String] -> String -> IO ()
mainProc "" _ _             = error "no label"
mainProc label [] ins       = putStr []
mainProc label (ln:lns) ins = f >> mainProc label lns ins
                            where f = if label `isInfixOf` ln
                                      then putStr ins
                                      else putStrLn ln 

readF :: String -> IO String
readF "-" = getContents
readF f   = readFile f
