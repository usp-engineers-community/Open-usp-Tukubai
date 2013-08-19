import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.ByteString.Lazy.Char8 as BS hiding (filter,head,drop,take,zip,length)
import Control.Applicative hiding ((<|>), many)
import Text.Printf

{--
ratio（Open usp Tukubai）

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
showUsage = do System.IO.hPutStr stderr ("Usage    : ratio [-<m>] [ref=<ref>] key=<n> <file>\n" ++ 
                "Mon Aug 19 11:03:28 JST 2013\n" ++
                "Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

main :: IO ()
main = do args <- getArgs
          case args of
              ["-h"]     -> showUsage
              ["--help"] -> showUsage
              _          -> mainProc (setOpts args)

mainProc :: Opts -> IO ()
mainProc (Opts keta ref key file) = readF file >>= mainProc' keta ref key

mainProc' :: Int -> Int -> [Int] -> BS.ByteString -> IO ()
mainProc' keta ref ks cs = out keta [ parseLine ref ks ln | ln <- BS.lines cs ]

out :: Int -> [Record] -> IO ()
out _    [] = do return ()
out keta rs = outBlock keta a >> out keta b 
    where x = takeBlock (BS.pack "") ([],rs)
          a = fst x
          b = snd x

outBlock :: Int -> [Record] -> IO ()
outBlock keta rs = BS.putStr $ BS.unlines [out' keta r sums | r <- rs ]
    where sums = keySum [] rs

out' :: Int -> Record -> [Double] -> BS.ByteString
out' keta (Record ws _ fs vs) ovs = BS.unwords $ out'' [1..(length ws)] ws fs ansvs
     where ansvs = [BS.pack $ printf fmt $ 100 * (fst v) / (snd v) | v <- zip vs ovs]
           fmt = "%." ++ (show keta) ++ "f"

out'' :: [Int] -> [BS.ByteString] -> [Int] -> [BS.ByteString] -> [BS.ByteString]
out'' []     _  _  _  = []
out'' (n:ns) ws fs vs = ans : out'' ns ws fs rvs
       where x = filter (==n) fs
             w = ws !! (n-1)
             ans = if x == [] then w else BS.unwords [w,head vs]
             rvs = if x == [] then vs else (drop 1 vs)

keySum :: [Double] -> [Record] -> [Double]
keySum sums [] = sums
keySum sums ((Record _ _ fs vs):rs) 
  | sums == [] = keySum vs rs
  | otherwise  = keySum [(fst x) + (snd x) | x <- zip sums vs] rs

takeBlock :: BS.ByteString -> ([Record],[Record]) -> ([Record],[Record])
takeBlock ref (as,[])     = (as,[])
takeBlock _   ([],(r:rs)) = takeBlock ref2 ([r],rs)
      where ref2 = f r
            f (Record _ x _ _) = x
takeBlock ref (as,(r:rs)) = if ref2 == ref then takeBlock ref ((as++[r]),rs) else (as,(r:rs))
      where ref2 = f r
            f (Record _ x _ _) = x

newRecord :: Record -> Record -> Record
newRecord (Record ws refstr fs vs) (Record pws prefstr pfs pvs)
  | pvs == []         = Record ws refstr fs vs
  | refstr /= prefstr = Record ws refstr fs vs
  | refstr == prefstr = Record ws refstr fs [ (fst x) + (snd x) | x <- Prelude.zip vs pvs]

myWords :: BS.ByteString -> [BS.ByteString]
myWords line = filter (/= x) $ BS.split ' ' line
               where x = BS.pack ""

parseLine :: Int -> [Int] -> BS.ByteString -> Record
parseLine ref ks ln = Record ws refstr ks vs
          where ws = myWords ln
                refstr = if ref == 0 then (BS.pack "") else (ws !! (ref-1))
                vs = [read $ BS.unpack $ ws !! (k-1) | k <- ks ]

data Record = Record [BS.ByteString] BS.ByteString [Int] [Double] deriving Show

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f

data Opts = Opts Int Int [Int] String | Error String deriving Show

setOpts :: [String] -> Opts
setOpts as = case parse args "" ((Prelude.unwords as) ++ " ") of
                  Right opt -> opt
                  Left err -> Error ( show err )

args = Opts <$> (try(keta) <|> return 1)
            <*> (try(ref) <|> return 0)
            <*> (try(key) <|> return [0])
            <*> (try(filename) <|> return "-")

keta = do char '-'
          a <- many1 digit
          char ' '
          return $ read a

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
