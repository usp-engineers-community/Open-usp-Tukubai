#!/usr/bin/env runghc
import System.Environment
import System.Exit
import System.IO
import System.Process (callProcess)

{--
man2（Open usp Tukubai）

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
    System.IO.hPutStr stderr "Usage    : man2 <command>\n"
    System.IO.hPutStr stderr "Version  : Mon Jan 20 17:18:16 JST 2025\n"
    System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD)\n"
    exitWith (ExitFailure 1)

directory_manual = "/usr/local/share/open-usp-tukubai/devel/man"

main = do args <- getArgs
          case args of
              []             -> showUsage
              ["-h"]         -> showUsage
              ["--help"]     -> showUsage
              ["--version"]  -> showUsage
              [command_name] -> do
                callProcess "less" [directory_manual ++ "/" ++ command_name ++ ".txt"]


