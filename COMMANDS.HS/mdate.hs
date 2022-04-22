#!/usr/bin/env runghc --
import System.Environment
import System.IO
import Data.List.Split hiding (oneOf)
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.ByteString.Lazy.Char8 as BS hiding (count,reverse,init,filter,drop,zip)
import Control.Applicative hiding ((<|>), many)
import Data.Time
import Data.Time.Format (formatTime)
import Data.Time.Format (parseTime)
import Text.Printf
import Data.List as DL

{--
mdate（Open usp Tukubai）

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
showUsage = do System.IO.hPutStr stderr ("Usage    : \n" ++
                              " DIRECT-MODE\n"++
                              "\n"++
                              "   日付  mdate -y <yyyymmdd>               : 曜日\n"++
                              "         mdate -e <yyyywwdd>/±<dif>        : dif 日先までの日付を連続出力\n"++
                              "         mdate -e <yyyymmdd1> <yyyymmdd2>  : 日付の範囲を連続出力\n"++
                              "         mdate <yyyywwdd>/±<dif>           : dif 日先の日付\n"++
                              "         mdate <yyyymmdd1> <yyyymmdd2>     : 日付の差\n"++
                              "         mdate <yyyymm>m/±<dif>            : dif 月先の月\n"++
                              "         mdate -e <yyyymm>m/±<dif>         : dif 月先までの月を連続出力\n"++
                              "         mdate <yyyymm1>m <yyyymm2>m       : 月の差\n"++
                              "         mdate -ly <yyyymm>m               : 前年月\n"++
                              "\n"++
                              " FILTER-MODE\n"++
                              "   日付  mdate -f -y <f>               : 曜日\n"++
                              "         mdate -f -e <f>/±<dif>        : dif 日先までの日付に展開\n"++
                              "         mdate -f -e <f1> <f2>         : 日付間の展開\n"++
                              "         mdate -f <f>/±<dif>           : dif 日先の日付\n"++
                              "         mdate -f <f1> <f2>            : 日付の差\n"++
                              "         mdate -f <f1> ±<f2>           : 日付の加算\n"++
                              "         mdate -f -e <f1> ±<f2>        : 日付の加算 展開\n"++
                              "         mdate -f -ly <f>              : 前年日\n"++
                              "   月次  mdate -f -d <f>m              : 日付を１カ月分出力\n"++
                              "         mdate -f <f>m/±<dif>          : dif 月先の月\n"++
                              "         mdate -f -e <f>m ±<dif>       : dif 月先の月まで展開\n"++
                              "         mdate -f <f1>m <f2>m          : 月の差\n"++
                              "         mdate -f -e <f1>m <f2>m       : 月の展開\n"++
                              "         mdate -f <f>m ±<dif>          : 月の加算\n"++
                              "         mdate -f -e <f>m ±<dif>       : 月の加算展開\n"++
                              "         mdate -f -ly <f>m             : 前年月\n"++
                              "\n"++
                              "Version  : Fri Apr 22 16:12:00 JST 2022\n" ++
                              "Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

main :: IO ()
main = do args <- getArgs
          case args of
              ("-f":as)  -> filterMode (setOpts as)
              ("-h":as)  -> showUsage
              []         -> showUsage
              _          -> directMode (setOpts args) 

directMode :: Opts -> IO ()
directMode (Week str) = BS.putStrLn $ toWeekStr (date8 str)
directMode (EnumDDiff r) = BS.putStrLn $ BS.unwords [ toDayStr d | d <- enumDiff r ]
directMode (EnumRange str1 str2) = BS.putStrLn $ BS.unwords [ toDayStr d | d <- getDaysRange str1 str2 ]
directMode (DayAhead r) = BS.putStrLn $ toDayStr $ dayAhead r
directMode (DayDiff str1 str2) = System.IO.putStrLn $ dayDiff (date8 str1) (date8 str2)
directMode (MonthAhead r) = BS.putStrLn $ monthAhead r
directMode (EnumMDiff r) = BS.putStrLn $ enumMDiff r
directMode (MonthDiff str1 str2) = BS.putStrLn $ monthDiff str1 str2
directMode (LastYearMonth m) = System.IO.putStrLn $ printf "%06d" m
directMode op = print op

filterMode :: Opts -> IO ()
filterMode (FWeek pos file) = readF file >>= filterWeek pos
filterMode (FEnumDDiff pos diff file) = readF file >>= filterEnumDayDiff pos diff
filterMode (FEnumRange f1 f2 file) = readF file >>= filterEnumDayRange f1 f2
filterMode (FDayAhead pos diff file) = readF file >>= filterDayAhead pos diff
filterMode (FDayDiff f1 f2 file) = readF file >>= filterDayDiff f1 f2
filterMode (FDayPlus f1 sign f2 file) = readF file >>= filterDayPlus f1 sign f2
filterMode (FDayEnumPlus f1 sign f2 file) = readF file >>= filterDayEnumPlus f1 sign f2
filterMode (FLastYearDay f1 file) = readF file >>= filterLastYearDay f1
filterMode (FEnumMonth f1 file) = readF file >>= filterEnumMonth f1
filterMode (FMonthAhead f1 diff file) = readF file >>= filterMonthAhead f1 diff
filterMode (FEnumMonthDiff f1 diff file) = readF file >>= filterEnumMonthDiff f1 diff
filterMode (FMonthDiff f1 f2 file) = readF file >>= filterMonthDiff f1 f2
filterMode (FEnumMonthRange f1 f2 file) = readF file >>= filterMonthRange f1 f2
filterMode (FMonthPlus f1 sign f2 file) = readF file >>= filterMonthPlus f1 sign f2
filterMode (FEnumMonthPlus f1 sign f2 file) = readF file >>= filterEnumMonthPlus f1 sign f2
filterMode (FLastYearMonth f1 file) = readF file >>= filterLastYearMonth f1
filterMode op = print op

type Record = BS.ByteString
type UWords = [BS.ByteString]

filterLastYearMonth :: Int -> BS.ByteString -> IO ()
filterLastYearMonth f1 cs = BS.putStr $ BS.unlines [ fLYM f1 (mwords ln) | ln <- (BS.lines cs) ]

fLYM f1 wds = BS.unwords [ g f1 nw | nw <- zip [1..] wds ]
             where ym = read (BS.unpack $ wds !! (f1-1))::Int
                   ly = BS.pack $ show $ ym - 100
                   g f1 (n,w) = if n == f1 then BS.unwords [w,ly] else w

filterEnumMonthPlus :: Int -> Char -> Int -> BS.ByteString -> IO ()
filterEnumMonthPlus f1 sign f2 cs = BS.putStr $ BS.unlines [ fEMP f1 sign f2 (mwords ln) | ln <- (BS.lines cs) ]

fEMP f1 sign f2 wds = BS.unwords [ g f2 nw | nw <- zip [1..] wds ]
             where str = BS.unpack $ wds !! (f1-1)
                   n = read (BS.unpack (wds !! (f2-1)))::Int
                   diff = if sign == '+' then n else n*(-1)
                   y = read (DL.take 4 str)::Int
                   m = read (drop 4 str)::Int
                   ma = enumMDiff (MonthRange y m diff)
                   g f2 (n,w) = if n == f2 then BS.unwords [w,ma] else w

filterMonthPlus :: Int -> Char -> Int -> BS.ByteString -> IO ()
filterMonthPlus f1 sign f2 cs = BS.putStr $ BS.unlines [ fMA2 f1 sign f2 (mwords ln) | ln <- (BS.lines cs) ]

fMA2 f1 sign f2 wds = BS.unwords [ g f2 nw | nw <- zip [1..] wds ]
             where str = BS.unpack $ wds !! (f1-1)
                   n = read (BS.unpack (wds !! (f2-1)))::Int
                   diff = if sign == '+' then n else n*(-1)
                   y = read (DL.take 4 str)::Int
                   m = read (drop 4 str)::Int
                   ma = monthAhead (MonthRange y m diff)
                   g f2 (n,w) = if n == f2 then BS.unwords [w,ma] else w

filterMonthRange :: Int -> Int -> BS.ByteString -> IO ()
filterMonthRange f1 f2 cs = BS.putStr $ BS.unlines [ fMR f1 f2 (mwords ln) | ln <- (BS.lines cs) ]

fMR f1 f2 wds = BS.unwords [ g f2 nw | nw <- zip [1..] wds ]
             where d1 = read (BS.unpack $ wds !! (f1-1))::Int
                   d2 = read (BS.unpack $ wds !! (f2-1))::Int
                   frm = if d1 < d2 then d1 else d2 
                   to  = if d1 < d2 then d2 else d1 
                   ds  = BS.unwords $ DL.map (BS.pack . show) $ filter (month) [frm..to]
                   g f2 (n,w) = if n == f2 then BS.unwords [w,ds] else w
                   month num = (num-1) `mod` 100 < 12

filterMonthDiff :: Int -> Int -> BS.ByteString -> IO ()
filterMonthDiff f1 f2 cs = BS.putStr $ BS.unlines [ fMD f1 f2 (mwords ln) | ln <- (BS.lines cs) ]

fMD f1 f2 wds = BS.unwords [ g f2 nw | nw <- zip [1..] wds ]
             where str1 = BS.unpack $ wds !! (f1-1)
                   str2 = BS.unpack $ wds !! (f2-1)
                   md = monthDiff str1 str2
                   g f2 (n,w) = if n == f2 then BS.unwords [w,md] else w

filterEnumMonthDiff :: Int -> Int -> BS.ByteString -> IO ()
filterEnumMonthDiff f1 diff cs = BS.putStr $ BS.unlines [ fEMD f1 diff (mwords ln) | ln <- (BS.lines cs) ]

fEMD f1 diff wds = BS.unwords [ g f1 nw | nw <- zip [1..] wds ]
             where str = wds !! (f1-1)
                   y = read (DL.take 4 $ BS.unpack str)::Int
                   m = read (drop 4 $ BS.unpack str)::Int
                   ma = enumMDiff (MonthRange y m diff)
                   g f1 (n,w) = if n == f1 then BS.unwords [w,ma] else w

filterMonthAhead :: Int -> Int -> BS.ByteString -> IO ()
filterMonthAhead f1 diff cs = BS.putStr $ BS.unlines [ fMA f1 diff (mwords ln) | ln <- (BS.lines cs) ]

fMA f1 diff wds = BS.unwords [ g f1 nw | nw <- zip [1..] wds ]
             where str = wds !! (f1-1)
                   y = read (DL.take 4 $ BS.unpack str)::Int
                   m = read (drop 4 $ BS.unpack str)::Int
                   ma = monthAhead (MonthRange y m diff)
                   g f1 (n,w) = if n == f1 then BS.unwords [w,ma] else w

filterEnumMonth :: Int -> BS.ByteString -> IO ()
filterEnumMonth f1 cs = BS.putStr $ BS.unlines [ fEM f1 (mwords ln) | ln <- (BS.lines cs) ]

fEM f1 wds = BS.unwords [ g f1 nw | nw <- zip [1..] wds ]
             where ds = enumMonth (wds !! (f1-1))
                   g f1 (n,w) = if n == f1
                                then BS.unwords [w,ds]
                                else w

enumMonth :: BS.ByteString -> BS.ByteString
enumMonth str = BS.unwords $ filter (BS.isPrefixOf m) [ toDayStr d | d <- enumDiff (DayRange firstday 31) ]
                where firstday = (BS.unpack m) ++ "01"
                      m = BS.take 6 str

filterLastYearDay :: Int -> BS.ByteString -> IO ()
filterLastYearDay f1 cs = BS.putStr $ BS.unlines [ fLYD f1 (mwords ln) | ln <- (BS.lines cs) ]

fLYD f1 wds = BS.unwords [ g f1 nw | nw <- zip [1..] wds ]
                 where ly = (read $ BS.unpack (wds !! (f1-1))) - 10000
                       ans = if ly `mod` 10000 == 229 then ly - 1 else ly
                       g f1 (n,w) = if n == f1
                                    then BS.unwords [w,BS.pack $ show ans]
                                    else w


filterDayEnumPlus :: Int -> Char -> Int -> BS.ByteString -> IO ()
filterDayEnumPlus f1 sign f2 cs = BS.putStr $ BS.unlines [ fDEP f1 sign f2 (mwords ln) | ln <- (BS.lines cs) ]

fDEP f1 sign f2 wds = BS.unwords [ g f1 sign f2 nw | nw <- zip [1..] wds ]
                 where d1 = BS.unpack $ wds !! (f1-1)
                       d2 = read $ BS.unpack $ wds !! (f2-1)
                       diff = if sign == '+' then d2 else d2*(-1)
                       ds = BS.unpack $ toDayStr $ dayAhead (DayRange d1 diff)
                       days = BS.unwords [ toDayStr d | d <- getDaysRange d1 ds ]
                       g f1 sign f2 (n,w) = if n == f2
                                            then BS.unwords [w,days]
                                            else w

filterDayPlus :: Int -> Char -> Int -> BS.ByteString -> IO ()
filterDayPlus f1 sign f2 cs = BS.putStr $ BS.unlines [ fDP f1 sign f2 (mwords ln) | ln <- (BS.lines cs) ]

fDP f1 sign f2 wds = BS.unwords [ g f1 sign f2 nw | nw <- zip [1..] wds ]
                 where d1 = BS.unpack $ wds !! (f1-1)
                       d2 = read $ BS.unpack $ wds !! (f2-1)
                       diff = if sign == '+' then d2 else d2*(-1)
                       ds = toDayStr $ dayAhead (DayRange d1 diff)
                       g f1 sign f2 (n,w) = if n == f2
                                            then BS.unwords [w,ds]
                                            else w


filterDayDiff :: Int -> Int -> BS.ByteString -> IO ()
filterDayDiff f1 f2 cs 
 | f1 == f2-1 = BS.putStr $ BS.unlines [ fDD f1 f2 (mwords ln) | ln <- (BS.lines cs) ]
 | otherwise  = error "連続したフィールドを指定してください。"

fDD f1 f2 wds = BS.unwords [ g f1 f2 nw | nw <- zip [1..] wds ]
                 where d1 = date8 $ BS.unpack $ wds !! (f1-1)
                       d2 = date8 $ BS.unpack $ wds !! (f2-1)
                       ds = BS.pack $ dayDiff d1 d2
                       g f1 f2 (n,w) = if n == f2
                                       then BS.unwords [w,ds]
                                       else w

filterEnumDayRange :: Int -> Int -> BS.ByteString -> IO ()
filterEnumDayRange f1 f2 cs 
 | f1 == f2-1 = BS.putStr $ BS.unlines [ fEDR f1 f2 (mwords ln) | ln <- (BS.lines cs) ]
 | otherwise  = error "連続したフィールドを指定してください。"

fEDR f1 f2 wds = BS.unwords [ g f1 f2 nw | nw <- zip [1..] wds ]
                 where d1 = BS.unpack $ wds !! (f1-1)
                       d2 = BS.unpack $ wds !! (f2-1)
                       ds = BS.unwords [ toDayStr d | d <- getDaysRange d1 d2 ]
                       g f1 f2 (n,w) = if n == f2
                                       then BS.unwords [w,ds]
                                       else w

filterDayAhead :: Int -> Int -> BS.ByteString -> IO ()
filterDayAhead pos diff cs = BS.putStr $ BS.unlines [ f pos diff (mwords ln) | ln <- (BS.lines cs) ]
            where f pos diff wds = BS.unwords [ g pos diff w | w <- zip [1..] wds ]
                  g pos diff (n,wd) = if pos == n
                                      then BS.unwords [wd,toDayStr $ dayAhead (DayRange (BS.unpack wd) diff)]
                                      else wd

filterEnumDayDiff :: Int -> Int -> BS.ByteString -> IO ()
filterEnumDayDiff pos diff cs = BS.putStr $ BS.unlines [ f pos diff (mwords ln) | ln <- (BS.lines cs) ]
            where f pos diff wds = BS.unwords [ g pos diff w | w <- zip [1..] wds ]
                  g pos diff (n,wd) = if pos == n
                                 then BS.unwords $ if wd == (DL.head gain) then gain else (wd : gain)
                                 else wd
                                 where gain = [ toDayStr d | d <- enumDiff (DayRange (BS.unpack wd) diff) ]

filterWeek :: Int -> BS.ByteString -> IO ()
filterWeek pos cs = BS.putStr $ BS.unlines [ f pos (mwords ln) | ln <- BS.lines cs ]
             where f pos wds = BS.unwords [ g pos w | w <- zip [1..] wds ]
                   g pos (n,wd) = if pos == n
                                    then BS.unwords [wd,toWeekStr (date8 $ BS.unpack wd)]
                                    else wd

readF :: String -> IO BS.ByteString
readF "-" = BS.getContents
readF f = BS.readFile f

mwords :: BS.ByteString -> [BS.ByteString]
mwords ws = filter (/= BS.pack "") $ BS.split ' ' ws

monthDiff :: String -> String -> BS.ByteString
monthDiff str1 str2 = BS.pack $ show diff
                      where y1 = read (DL.take 4 str1)::Int
                            y2 = read (DL.take 4 str2)::Int
                            m1 = read (drop 4 str1)::Int
                            m2 = read (drop 4 str2)::Int
                            diff =  (y1 - y2)*12 + m1 - m2

monthAhead :: MonthRange -> BS.ByteString
monthAhead (MonthRange y m diff) = toMonthStr yadd mrem
                          where m0 = m - 1 + diff
                                yadd = y + (m0 `div` 12)
                                mrem = 1 + (m0 `mod` 12)

enumMDiff :: MonthRange -> BS.ByteString
enumMDiff (MonthRange y m diff) = BS.unwords [ BS.pack $ show n | n <- nums ]
                          where t = monthAhead (MonthRange y m diff)
                                n1 = y*100 + m
                                n2 = read (BS.unpack t)::Int
                                fnum = if n1 < n2 then n1 else n2
                                tnum = if n1 < n2 then n2 else n1
                                nums = filter (f) [fnum..tnum]
                                f n = n `mod` 100 < 13 && n `mod` 100 /= 0

toMonthStr :: Int -> Int -> BS.ByteString
toMonthStr y m = BS.pack $ (yyyy y) ++ (mm m)

yyyy :: Int -> String
yyyy y = printf "%04d" y

mm :: Int -> String
mm m = printf "%02d" m

dayDiff :: UTCTime -> UTCTime -> String
dayDiff a b = init $ show x
              where x = (diffUTCTime a b)/(24*3600)

getDaysRange :: String -> String -> [UTCTime]
getDaysRange str1 str2 = if str1 > str2 then getDayR' d2 d1 else getDayR' d1 d2
                  where d1 = date8 str1
                        d2 = date8 str2
                        getDayR' frm to = if frm == to
                                          then [to]
                                          else frm : getDayR' (nextDay frm) to

enumDiff :: DayRange -> [UTCTime]
enumDiff (DayRange d diff) 
 | diff == 0 = [date8 d]
 | diff > 0  = enumDPlus (date8 d) diff 0
 | otherwise = reverse $ enumDMinus (date8 d) (-diff) 0

enumDMinus :: UTCTime -> Int -> Int -> [UTCTime]
enumDMinus day diff c 
  | c == 0    = day : enumDMinus day diff 1
  | diff == c = [nday]
  | otherwise = nday : enumDMinus nday diff (c+1)
                   where nday = prevDay day

enumDPlus :: UTCTime -> Int -> Int -> [UTCTime]
enumDPlus day diff c 
  | c == 0    = day : enumDPlus day diff 1
  | diff == c = [nday]
  | otherwise = nday : enumDPlus nday diff (c+1)
                   where nday = nextDay day

date8 :: String -> UTCTime
date8 str = f d
            where d = parseTimeM True defaultTimeLocale "%Y%m%d" str :: Maybe UTCTime
                  f (Just x) = x

dayAhead :: DayRange -> UTCTime
dayAhead (DayRange day diff) = addUTCTime (dayToSec diff) (date8 day)

nextDay :: UTCTime -> UTCTime
nextDay d = addUTCTime (24*3600) d

prevDay :: UTCTime -> UTCTime
prevDay d = addUTCTime (-24*3600) d

-- I'dont know how to cast with a simple way!
dayToSec :: Int -> NominalDiffTime
dayToSec n
  | n == 0    = 0
  | n < 0     = (dayToSec (n+1)) - 24*3600
  | otherwise = 24*3600 + (dayToSec (n-1))

toWeekStr :: UTCTime -> BS.ByteString
toWeekStr s = BS.pack $ formatTime defaultTimeLocale "%u" s

toDayStr :: UTCTime -> BS.ByteString
toDayStr d = BS.pack $ formatTime defaultTimeLocale "%Y%m%d" d

data Opts = Week String 
          | EnumDDiff DayRange
          | EnumRange String String
          | DayAhead DayRange
          | DayDiff String String
          | MonthAhead MonthRange
          | EnumMDiff MonthRange
          | MonthDiff String String
          | LastYearMonth Int
          | FWeek Int String
          | FEnumDDiff Int Int String
          | FEnumRange Int Int String
          | FDayAhead Int Int String
          | FDayDiff Int Int String
          | FDayPlus Int Char Int String
          | FDayEnumPlus Int Char Int String
          | FLastYearDay Int String
          | FEnumMonth Int String
          | FMonthAhead Int Int String
          | FEnumMonthDiff Int Int String
          | FMonthDiff Int Int String
          | FEnumMonthRange Int Int String
          | FMonthPlus Int Char Int String
          | FEnumMonthPlus Int Char Int String
          | FLastYearMonth Int String
          | Error String deriving Show

data DayRange = DayRange String Int deriving Show
data MonthRange = MonthRange Int Int Int deriving Show

setOpts :: [String] -> Opts
setOpts as = case parse args "" ((Prelude.unwords as) ++ " ") of
                  Right opt -> opt
                  Left err -> Error ( show err ) 

fweek =  do string "-y "
            n <- many1 digit
            char ' '
            f <- try(filename) <|> return "-"
            return $ FWeek (read n) f

args = try(week) <|>
       try(enumdiff) <|>
       try(enumrange) <|>
       try(dayahead) <|> 
       try(daydiff) <|>
       try(monthahead) <|>
       try(enummdiff) <|>
       try(monthdiff) <|>
       try(lastyearmonth) <|>
       try(fweek) <|>
       try(fenumdiff) <|>
       try(fenumrange) <|>
       try(fdayahead) <|>
       try(fdaydiff) <|>
       try(fdayplus) <|>
       try(fdayenumplus) <|>
       try(flastyearday) <|>
       try(fenummonth) <|>
       try(fmonthahead) <|>
       try(fenummonthdiff) <|>
       try(fmonthdiff) <|>
       try(fenummonthrange) <|>
       try(fmonthplus) <|>
       try(fenummonthplus) <|>
       try(flastyearmonth) <?> "error"

flastyearmonth = do string "-ly "
                    a <- many1 digit
                    string "m "
                    f <- try(filename) <|> return "-"
                    return $ FLastYearMonth (read a) f

fenummonthplus = do string "-e "
                    a <- many1 digit
                    string "m "
                    s <- oneOf "+-"
                    b <- many1 digit
                    char ' '
                    f <- try(filename) <|> return "-"
                    return $ FEnumMonthPlus (read a) s (read b) f

fmonthplus = do a <- many1 digit
                string "m "
                s <- oneOf "+-"
                b <- many1 digit
                char ' '
                f <- try(filename) <|> return "-"
                return $ FMonthPlus (read a) s (read b) f

fenummonthrange = do string "-e "
                     a <- many1 digit
                     string "m "
                     b <- many1 digit
                     string "m "
                     f <- try(filename) <|> return "-"
                     return $ FEnumMonthRange (read a) (read b) f

fmonthdiff = do a <- many1 digit
                string "m "
                b <- many1 digit
                string "m "
                f <- try(filename) <|> return "-"
                return $ FMonthDiff (read a) (read b) f

fenummonthdiff = do string "-e "
                    a <- many1 digit
                    string "m/"
                    n <- try(minusnum) <|> try(plusnum)
                    char ' '
                    f <- try(filename) <|> return "-"
                    return $ FEnumMonthDiff (read a) n f

fmonthahead = do a <- many1 digit
                 string "m/"
                 n <- try(minusnum) <|> try(plusnum)
                 char ' '
                 f <- try(filename) <|> return "-"
                 return $ FMonthAhead (read a) n f

fenummonth = do string "-d "
                a <- many1 digit
                string "m "
                f <- try(filename) <|> return "-"
                return $ FEnumMonth (read a) f

flastyearday = do string "-ly "
                  a <- many1 digit
                  char ' '
                  f <- try(filename) <|> return "-"
                  return $ FLastYearDay (read a) f

fdayenumplus = do string "-e "
                  a <- many1 digit
                  char ' '
                  b <- oneOf "+-"
                  c <- many1 digit
                  char ' '
                  f <- try(filename) <|> return "-"
                  return $ FDayEnumPlus (read a) b (read c) f

fdayplus = do a <- many1 digit
              char ' '
              b <- oneOf "+-"
              c <- many1 digit
              char ' '
              f <- try(filename) <|> return "-"
              return $ FDayPlus (read a) b (read c) f

fdaydiff = do a <- many1 digit
              char ' '
              b <- many1 digit
              char ' '
              f <- try(filename) <|> return "-"
              return $ FDayDiff (read a) (read b) f

fdayahead = do a <- many1 digit
               char '/'
               n <- try(minusnum) <|> try(plusnum)
               char ' '
               f <- try(filename) <|> return "-"
               return $ FDayAhead (read a) n f

fenumrange = do string "-e "
                a <- many1 digit
                char ' '
                b <- many1 digit
                char ' '
                f <- try(filename) <|> return "-"
                return $ FEnumRange (read a) (read b) f

fenumdiff = do string "-e "
               p <- many1 digit
               char '/'
               n <- try(minusnum) <|> try(plusnum)
               char ' '
               f <- try(filename) <|> return "-"
               return $ FEnumDDiff (read p) n f

week = string "-y " >> (Week <$> day8 )

day8 = count 8 digit

dayahead = DayAhead <$> dayrange 
enumdiff = string "-e " >> EnumDDiff <$> dayrange 
enumrange = do string "-e "
               a <- day8
               char ' '
               b <- day8
               return $ EnumRange a b

lastyearmonth = do string "-ly "
                   n <- count 6 digit
                   char 'm'
                   return $ LastYearMonth ((read n) - 100)

monthdiff = do a <- count 6 digit
               string "m "
               b <- count 6 digit
               char 'm'
               return $ MonthDiff a b

daydiff = do a <- day8
             char ' '
             b <- day8
             return $ DayDiff a b

dayrange = do a <- day8
              char '/'
              b <- try(minusnum) <|> try(plusnum)
              return $ DayRange a b

enummdiff = string "-e " >> EnumMDiff <$> monthrange
monthahead = MonthAhead <$> monthrange

monthrange = do a <- count 4 digit
                b <- count 2 digit
                string "m/"
                c <- try(minusnum) <|> try(plusnum)
                return $ MonthRange (read a) (read b) c

plusnum = char '+' >> many1 digit >>= return . read
minusnum = do a <- char '-'
              b <- many1 digit
              return $ read (a:b)

filename = many1 ( try(letter) <|> try(digit) <|> symbol ) >>= return

symbol = oneOf "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
