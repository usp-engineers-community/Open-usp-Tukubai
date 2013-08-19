import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.ByteString.Lazy.Char8 as BS hiding (filter,head,drop,take,reverse)
import Control.Applicative hiding ((<|>), many)

{--
kasan（Open usp Tukubai）

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
showUsage = do System.IO.hPutStr stderr ("Usage    : kasan [+r] [ref=<ref>] key=<n> <file>\n" ++ 
                "Mon Aug 19 15:05:05 JST 2013\n" ++
                "Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

main :: IO ()
main = do args <- getArgs
          case args of
              ["-h"]     -> showUsage
              ["--help"] -> showUsage
              _          -> mainProc (setOpts args)

mainProc :: Opts -> IO ()
mainProc (Opts rep ref key file) = readF file >>= mainProc' rep ref key


mainProc' :: Bool -> Int -> [Int] -> BS.ByteString -> IO ()
mainProc' rep ref ks cs = out rep [ parseLine ref ks ln | ln <- BS.lines cs ] nullRecord
 where nullRecord = Record [] (BS.pack "") [] []

out :: Bool -> [Record] -> Record -> IO ()
out rep []     _    = do return ()
out rep (r:rs) prep = BS.putStrLn (out' rep newr) >> out rep rs newr
                      where newr = newRecord r prep

out' :: Bool -> Record -> BS.ByteString
out' rep (Record ws refstr fs vs) = BS.unwords $ out'' [1..(Prelude.length ws)] rep fs vs ws

out'' :: [Int] -> Bool -> [Int] -> [Double] -> [BS.ByteString] -> [BS.ByteString]
out'' []     _   _  _  _  = []
out'' (n:ns) rep fs vs ws = ans : out'' ns rep fs rvs ws
        where x = filter (==n) fs
              w = ws !! (n-1)
              nstr = BS.pack $ formatNum $ show $ head vs
              repstr = if rep then nstr else BS.unwords [w,nstr]
              ans = if x == [] then w else repstr
              rvs = if x == [] then vs else (drop 1 vs)

formatNum :: String -> String
formatNum str = if x == [] then str else reverse $ cutZero (reverse str)
        where x = filter (== '.') str

cutZero :: String -> String
cutZero ('.':cs) = cs
cutZero ('0':cs) = cutZero cs
cutZero (c:cs)   = c:cs

newRecord :: Record -> Record -> Record
newRecord (Record ws refstr fs vs) (Record pws prefstr pfs pvs)
  | pvs == []         = Record ws refstr fs vs
  | refstr /= prefstr = Record ws refstr fs vs
  | refstr == prefstr = Record ws refstr fs [ (fst x) + (snd x) | x <- Prelude.zip vs pvs]

parseLine :: Int -> [Int] -> BS.ByteString -> Record
parseLine ref ks ln = Record ws refstr ks vs
          where ws = myWords ln
                refstr = if ref == 0 then (BS.pack "") else (ws !! (ref-1))
                vs = f ks ws
                f [] _      = []
                f (k:ks) ws = (read (BS.unpack $ ws !! (k-1))::Double ) : f ks ws

myWords :: BS.ByteString -> [BS.ByteString]
myWords line = filter (/= x) $ BS.split ' ' line
               where x = BS.pack ""

data Record = Record [BS.ByteString] BS.ByteString [Int] [Double] deriving Show

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f

data Opts = Opts Bool Int [Int] String | Error String deriving Show

setOpts :: [String] -> Opts
setOpts as = case parse args "" ((Prelude.unwords as) ++ " ") of
                  Right opt -> opt
                  Left err -> Error ( show err )

args = Opts <$> (try(string "+r " >> return True) <|> return False)
            <*> (try(ref) <|> return 0)
            <*> (try(key) <|> return [0])
            <*> (try(filename) <|> return "-")

ref = do string "ref="
         a <- many1 digit
         char ' '
         return (read a)

key = do string "key="
         a <- try(atnums) <|> try(slashnums) <|> simplepos
         char ' '
         return a

atnums = do a <- intnum
            nums <- many1 atnum
            return (a:nums)

atnum = char '@' >> intnum

slashnums = do a <- intnum
               char '/'
               b <- intnum
               return [a..b]

simplepos = do a <- many1 digit
               return [read a]

intnum = many1 digit >>= return . read

filename =  many1 ( letter <|> digit <|> symbol )

symbol = oneOf "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
