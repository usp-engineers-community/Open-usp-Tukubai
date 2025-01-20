#!/usr/bin/env runghc
import System.Environment
import System.Exit
import System.IO
import Data.Maybe (fromJust)
import Data.Ix (range)
import Data.List (drop, elemIndex, singleton, take)
import Data.Time.Calendar

{--
ysum（Open usp Tukubai）

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


command_name = "yobi"
version = "Mon Mar 20 09:33:42 JST 2023"

showUsage :: IO ()
showUsage = do
  System.IO.hPutStr stderr $ "Usage    : " ++ command_name ++ " [-e|-j] <field> [<filename>]\n"
  System.IO.hPutStr stderr $ "           " ++ command_name ++ " -d [-e|-j] <string>\n"
  System.IO.hPutStr stderr $ "Version  : Mon Jan 20 17:18:17 JST 2025\n"
  System.IO.hPutStr stderr $ "           Open usp Tukubai (LINUX+FREEBSD)\n"
  exitFailure

data Language = Index | English | Japanese deriving Show

main :: IO ()
main = do args <- getArgs
          case args of
            ["-h"]        -> showUsage
            ["--help"]    -> showUsage
            ["--version"] -> showUsage
            ["-e", field_index] -> yobi' English (read field_index) stdin
            ["-e", field_index, file_name] -> openFile file_name ReadMode >>= yobi' English (read field_index)
            ["-j", field_index] -> yobi' Japanese (read field_index) stdin
            ["-j", field_index, file_name] -> openFile file_name ReadMode >>= yobi' Japanese (read field_index)
            ["-d", string] ->
              yobi Index string >>= System.IO.putStrLn
            ["-d", "-e", string] ->
              yobi English string >>= System.IO.putStrLn
            ["-d", "-j", string] ->
              yobi Japanese string >>= System.IO.putStrLn
            [field_index'] ->
              case reads field_index' :: [(Int, String)] of
               [(field_index, "")] -> yobi' Index field_index stdin
               _ -> showUsage
            [field_index', file_name] ->
              case reads field_index' :: [(Int, String)] of
               [(field_index, "")] ->
                 openFile file_name ReadMode >>= yobi' Index field_index
               _ -> showUsage
            _ -> showUsage

yobi language date = do
  output_days <-
    case language of
     Index    -> return index_days
     Japanese -> return japanese_days
     English  -> return english_days

  case date of
   [year_1, year_2, year_3, year_4, month_1, month_2, date_1, date_2] ->
    let (year, month, date) =
         ((read ([year_1] ++ [year_2] ++ [year_3] ++ [year_4]) :: Integer),
          (read ([month_1] ++ [month_2]) :: Int),
          (read ([date_1] ++ [date_2]) :: Int)) in do
      let day = dayOfWeek $ fromGregorian year month date in
        return $ output_days !! (fromJust $ elemIndex day days)
   _ -> error "日付が8桁整数になっていません。"
  where 
    days = [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]
    index_days = fmap singleton $ range ('0', '7')
    japanese_days = ["日", "月", "火", "水", "木", "金", "土"]
    english_days = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]

yobi' :: Language -> Int -> Handle -> IO ()

yobi' language field_index source =
  hIsEOF source >>= \eof ->
    case eof of
     True -> hClose source
     False -> do
      columns <- words <$> hGetLine source 
      System.IO.putStrLn $ unwords $
            take field_index columns ++
            (yobi language $ columns !! (field_index - 1)) ++
            drop field_index columns
      yobi' language field_index source
