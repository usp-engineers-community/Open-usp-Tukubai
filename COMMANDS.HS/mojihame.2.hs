import System.IO
import System.Environment
import System.Exit
import qualified Data.ByteString.Lazy.Char8 as BS

showUsage :: IO ()
showUsage = do System.IO.hPutStr stderr (
                "Usage    : mojihame [-lLABEL] <template> <data> \n" ++
                "Fri Feb  6 17:18:24 JST 2015\n" ++
                "Open usp Tukubai (LINUX+FREEBSD+Mac), Haskell ver.\n")
               exitWith (ExitFailure 1) 

die str = System.IO.hPutStr stderr ( "Error[mojihame] : " ++ str ++ "\n")
          >> exitWith (ExitFailure 1)

main :: IO ()
main = do args <- getArgs
          case args of
              [tmpf,dataf]                 -> noopt tmpf dataf
              [('-':'l':label),tmpf,dataf] -> lopt (BS.pack label) tmpf dataf
              _          -> showUsage

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f   = BS.readFile f

noopt :: String -> String -> IO()
noopt tmpf dataf = do template <- readF tmpf
                      d <- readF dataf
                      BS.putStr $ noopt' template (BS.words d)

noopt' :: BS.ByteString -> [BS.ByteString] -> BS.ByteString
noopt' template ws = undefined

findPos :: BS.ByteString -> (BS.ByteString,Int,BS.ByteString)
findPos = undefined

lopt :: BS.ByteString -> String -> String -> IO()
lopt = undefined
