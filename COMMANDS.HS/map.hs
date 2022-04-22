#!/usr/bin/env runghc --
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as BS
import System.IO
import System.Exit
import Text.Read

showUsage :: IO ()
showUsage = do System.IO.hPutStr stderr (
                "Usage    : map <num=<n>> <file> \n" ++
                "Version  : Thu Oct 23 08:52:44 JST 2014\n" ++
                "Open usp Tukubai (LINUX+FREEBSD+Mac), Haskell ver.\n")
               exitWith (ExitFailure 1) 

udie str = System.IO.hPutStr stderr ( "Error[map] : " ++ str ++ "\n")
          >> exitWith (ExitFailure 1)

main :: IO ()
main = do args <- getArgs
          case args of
              ["-h"]     -> showUsage
              ["--help"] -> showUsage
              [num]      -> readF "-"  >>= main' (getNum num)
              [num,file] -> readF file >>= main' (getNum num)
              _          -> showUsage

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f

type UWord   = BS.ByteString
type Key    = BS.ByteString
type SubKey = BS.ByteString
type Values = [UWord]
type Line   = (Key,SubKey,Values)
type Data   = [Line]

main' :: Either String Int -> BS.ByteString -> IO ()
main' (Left  str) cs = udie str
main' (Right num) cs = header num h_axis >> mapM_ (body h_axis) (splitByKey d)
    where d = [ makeLine num ln | ln <- BS.lines cs ]
          h_axis = hAxis $ map ( \(_,s,_) -> s ) d
          hAxis []     = []
          hAxis (e:es) = e : (hAxis $ filter ( /= e ) es )

splitByKey :: [Line] -> [[Line]]
splitByKey []  = []
splitByKey lns@((key,_,_):_) = a : splitByKey b
    where a = takeWhile (\(k,_,_) -> key == k) lns
          b = dropWhile (\(k,_,_) -> key == k) lns

header :: Int -> [SubKey] -> IO ()
header num ss = BS.putStrLn $ BS.unwords (keyf ++ ss)
    where keyf = replicate num (BS.pack "*")

body :: [SubKey] -> [Line] -> IO ()
body ss lns@((k,_,_):_) = BS.putStrLn $ BS.unwords (k:(body' ss lns))

body' :: [SubKey] -> [Line] -> [UWord]
body' []   _  = []
body' subs [] = replicate (length subs) (BS.pack "0")
body' (sub:subs) alns@((_,s,(v:_)):lns) 
  | sub == s  = v : body' subs lns
  | otherwise = BS.pack "0" : body' subs alns

makeLine :: Int -> BS.ByteString -> Line
makeLine num ln = (k,s,v)
    where k = BS.unwords $ take num $ BS.words ln
          s = head $ drop num $ BS.words ln
          v = drop (num + 1) $ BS.words ln
    
getNum :: String -> Either String Int
getNum ('n':'u':'m':'=':str) = getNum' (readMaybe str)
getNum _                     = Left "no num option"

getNum' :: Maybe Int -> Either String Int
getNum' Nothing = Left "invalid number for num option"
getNum' (Just n) 
 | n > 0     = Right n
 | otherwise = Left "invalid number for num option"
