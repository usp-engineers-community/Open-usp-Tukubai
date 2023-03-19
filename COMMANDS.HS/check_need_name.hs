#!/usr/bin/env runghc --
import System.Environment
import System.Exit
import System.IO
import Data.ByteString.Lazy.Char8 as BS hiding (filter,take,concat,map,drop,length)

{--
check_need_name（Open usp Tukubai）

designed by Nobuaki Tounaka
written  by Hinata Yanagi

The MIT License

Copyright (C) 2023 Universal Shell Programming Laboratory

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
                System.IO.hPutStr stderr "Usage    : check_need_name [--blank <string>] <check_file> [<name_file>]\n"
                System.IO.hPutStr stderr "Version  : Sun Mar 19 15:46:12 JST 2023\n"
                System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD)\n"
                exitFailure

main :: IO ()
main = do args <- getArgs
          case args of
                []            -> showUsage
                ["-h"]        -> showUsage
                ["--help"]    -> showUsage
                ["--version"] -> showUsage
                ["--blank",str,check,name] -> do cf <- readF check
                                                 nf <- readF name
                                                 doCheck (BS.pack str) cf nf
                [check]       -> do cf <- readF check
                                    nf <- readF "-"
                                    doCheck (BS.pack "_") cf nf
                [check, name] -> do cf <- readF check
                                    nf <- readF name
                                    doCheck (BS.pack "_") cf nf
                _             -> showUsage

type Key = BS.ByteString

doCheck :: BS.ByteString -> BS.ByteString -> BS.ByteString -> IO ()
doCheck ngstr cf nf = putWithFinalState outputkeys
    where keys        = parseCheckFile $ BS.lines cf
          valuekeys   = getValueKeys ngstr (BS.lines nf)
          novaluekeys = getNoValueKeys ngstr (BS.lines nf)
          outputkeys  = concat [ check k valuekeys novaluekeys | k <- keys ]

putWithFinalState :: [Key] -> IO ()
putWithFinalState [] = do return ()
putWithFinalState ks = do BS.putStr $ BS.unlines ks
                          exitWith $ ExitFailure 1

check :: Key -> [Key] -> [Key] -> [Key]
check k vs nvs
  | kvs == [] && nkvs == [] = [k]
  | otherwise               = nkvs
    where kvs = getRelated k vs
          nkvs = getRelated k nvs

getRelated :: Key -> [Key] -> [Key]
getRelated k vs = filter (isRelated k) vs

isRelated :: Key -> Key -> Bool
isRelated k v = if k == v then True else f k v
    where f k v = if k `BS.isPrefixOf` v then g k (drop (length $ BS.unpack k) $ BS.unpack v) else False
          g k []       = True
          g k ('_':[]) = False
          g k ('_':cs) = g k cs
          g k (c:cs)   = if c >= '0' && c <= '9' then g k cs else False


parseCheckFile :: [BS.ByteString] -> [Key]
parseCheckFile clns = [ (myWords ln) !! 0 | ln <- clns ]

getValueKeys :: BS.ByteString -> [BS.ByteString] -> [Key]
getValueKeys ngstr lns = concat [getValueKeys' ngstr (myWords ln) | ln <- lns ]

getNoValueKeys :: BS.ByteString -> [BS.ByteString] -> [Key]
getNoValueKeys ngstr lns = concat [getNoValueKeys' ngstr (myWords ln) | ln <- lns ]

getNumKeys :: [Key] -> [Key]
getNumKeys ks = filter ( isNoKey ) ks

isNoKey :: Key -> Bool
isNoKey k = isNoKey' (BS.unpack $ BS.reverse k)

isNoKey' :: String -> Bool
isNoKey' (c:'_':cs) = if c >= '0' && c <= '9' then True else False
isNoKey' (a:b:cs)   = if a >= '0' && a <= '9' && b >= '0' && b <= '9' then isNoKey' (b:cs) else False
isNoKey' _          = False

getValueKeys' :: BS.ByteString -> [BS.ByteString] -> [Key]
getValueKeys' ngstr ws
  | length ws < 2    = []
  | ws !! 1 == ngstr = []
  | otherwise        = take 1 ws

getNoValueKeys' :: BS.ByteString -> [BS.ByteString] -> [Key]
getNoValueKeys' ngstr ws
  | length ws < 2    = take 1 ws
  | ws !! 1 == ngstr = take 1 ws
  | otherwise        = []

myWords :: BS.ByteString -> [BS.ByteString]
myWords line = filter (/= x) $ BS.split ' ' line
               where x = BS.pack ""

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f


