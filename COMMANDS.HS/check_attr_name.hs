#!/usr/bin/env runghc --
import Control.Monad (foldM)
import Data.Char (isAlpha, isAlphaNum, isAscii, isDigit, isPrint)
import Data.Ix (range)
import Data.List (dropWhile, intersect, map, singleton)
import Data.Map hiding (mapMaybe, singleton, take)
import Data.Maybe (fromJust, isJust, mapMaybe, maybeToList)
import qualified Data.Text (pack, unpack, words)
import Data.Text.Normalize (NormalizationMode (NFC), normalize) -- unicode-transforms をインストールすること。
import GHC.List (elem)
import System.Directory (doesFileExist)
import System.Environment
import System.Exit
import System.IO
import Text.Read

{--
check_attr_name（Open usp Tukubai）

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
  System.IO.hPutStr stderr "Usage    : check_attr_name <check_file> [<name_file>]\n"
  System.IO.hPutStr stderr "Option   : --through <string> --ngword <ng_file>\n"
  System.IO.hPutStr stderr "Version  : Sat Aug 19 21:33:04 JST 2023\n"
  System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD)\n"
  exitFailure

data Option = Option
  {
    through :: Maybe String,
    ngword :: Maybe String
  }

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> showUsage
    ["-h"] -> showUsage
    ["--help"] -> showUsage
    ["--version"] -> showUsage
    arguments -> check_attr_name arguments Option {through = Nothing, ngword = Nothing}

width_of [] = 0
width_of (needle : rest) =
  let width = width_of rest
   in if needle `Prelude.elem` halfwidth
        then width + 1
        else width + 2
  where
    halfwidth =
      -- "Halfwidth and Fullwidth Forms" (http://www.unicode.org/charts/PDF/UFF00.pdf)
      range ('!', '~')
        ++ range ('｟', '\xFFBE')
        ++ range ('\xFFC2', '\xFFC7')
        ++ range ('\xFFCA', '\xFFCF')
        ++ range ('\xFFD2', '\xFFD7')
        ++ range ('\xFFDA', '\xFFDC')
        ++ range ('\xFFE0', '\xFFE6')
        ++ range ('\xFFE8', '\xFFEE')

checkers =
  Data.Map.fromList
    [ ('N', \value eq -> length value == eq && all isDigit (value :: [Char])),
      ('n', \value lte -> length value <= lte && all isDigit value),
      ('S', \value eq -> length value == eq && (isJust $ (readMaybe value :: Maybe Int))),
      ('s', \value lte -> length value <= lte && (isJust $ (readMaybe value :: Maybe Int))),
      ('V', \value eq -> length value == eq && (isJust $ (readMaybe value :: Maybe Float))),
      ('v', \value lte -> length value <= lte && (isJust $ (readMaybe value :: Maybe Float))),
      ('F', \value eq -> length value == eq && (fromJust $ (readMaybe value :: Maybe Float)) >= 0),
      ('f', \value lte -> length value <= lte && (fromJust $ (readMaybe value :: Maybe Float)) >= 0),
      ('E', \value eq -> length value == eq && all isAlpha value),
      ('e', \value lte -> length value <= lte && all isAlpha value),
      ('A', \value eq -> length value == eq && all isAscii value),
      ('a', \value lte -> length value <= lte && all isAscii value),
      ('B', \value eq -> length value == eq && all isAlphaNum value),
      ('b', \value lte -> length value <= lte && all isAlphaNum value),
      ('H', \value eq -> width_of value == eq),
      ('h', \value lte -> width_of value <= lte),
      ('Z', \value eq -> width_of value == eq),
      ('z', \value lte -> width_of value <= lte),
      ( 'k',
        \value eq ->
          length value == eq
            && all
              ( \member ->
                  Prelude.elem member $ singleton $ range ('\12448', '\12543')
              )
              [value]
      ),
      ( 'k',
        \value lte ->
          length value <= lte
            && all
              ( \member ->
                  Prelude.elem member $ singleton $ range ('\12448', '\12543')
              )
              [value]
      ),
      ('X', \value eq -> length value == eq && all Data.Char.isPrint value),
      ('x', \value lte -> length value <= lte && all Data.Char.isPrint value),
      ('C', \value eq -> length value == eq && True),
      ('c', \value lte -> length value <= lte && True),
      ( 'O',
        \value eq ->
          length value == eq
            && all
              ( \member ->
                  Prelude.elem member $ singleton $ range ('A', 'Z')
              )
              [value]
      ),
      ( 'o',
        \value lte ->
          length value <= lte
            && all
              ( \member ->
                  Prelude.elem member $ singleton $ range ('A', 'Z')
              )
              [value]
      ),
      ('J', \value eq -> length value == eq && all (\character -> width_of [character] == 2 || isAlphaNum character) value),
      ('j', \value lte -> length value <= lte && all (\character -> width_of [character] == 2 || isAlphaNum character) value)
    ]

check_attr_name ("--through" : through : arguments) option =
  check_attr_name arguments option {through = Just through}
check_attr_name ("--ngword" : ngword : arguments) option = do
  existence <- doesFileExist ngword

  if not existence
    then error $ "ファイル '" ++ ngword ++ "' をオープンできません。"
    else do
      ngword_content <- Prelude.filter (\needle -> not $ isAscii needle) <$> readFile ngword
      check_attr_name arguments option {ngword = Just ngword_content}
check_attr_name arguments option =
  case arguments of
    [path_check_file] -> do
      check_file <- openFile path_check_file ReadMode
      checks <- hGetContents check_file >>= load_checks
      check_attr_name' ExitSuccess option (Data.Map.fromList checks) stdin
      hClose (check_file)
    [path_check_file, path_name_file] -> do
      check_file <- openFile path_check_file ReadMode
      checks <- hGetContents check_file >>= load_checks
      name_file <- openFile path_name_file ReadMode
      check_attr_name' ExitSuccess option (Data.Map.fromList checks) name_file
      hClose (check_file)
      hClose (name_file)
    _ -> showUsage

check_attr_name' exit_status option checks name_file = do
  eof <- hIsEOF name_file

  if eof
    then do
      exitWith exit_status
    else do
      -- [タグ名, データ] が設定される。
      name_value <- take 2 <$> words <$> hGetLine name_file
      -- 末尾の _(数値) を除く。
      let name =
            reverse $
              dropWhile ((==) '_') $
                dropWhile isDigit $
                  reverse $
                    name_value !! 0
       in do
            case Data.Map.lookup name checks of
              Nothing ->
                check_attr_name' exit_status option checks name_file
              Just (category, length_check) ->
                -- Unicode 文字列の正規化を行う
                let normalized_value = Data.Text.unpack $ normalize NFC (Data.Text.pack (name_value !! 1))
                 in if through option == Just (name_value !! 1)
                      then -- "--through" で設定された文字列に一致した場合は無視する。
                        check_attr_name' exit_status option checks name_file
                      else
                        if (checkers ! category) normalized_value length_check
                          &&
                          -- "--ngword" で設定されたファイルの内容と交差がないことを確認する。
                          intersect (concat $ maybeToList (ngword option)) (name_value !! 1) == []
                          then do
                            check_attr_name' exit_status option checks name_file
                          else do
                            putStrLn $ unwords [head name_value, [category] ++ show length_check]
                            check_attr_name' (ExitFailure 1) option checks name_file

load_checks content = do
  return
    $ mapMaybe
      ( \fields ->
          case fields of
            name : (category : length_str) : _ ->
              case Data.Map.lookup category checkers of
                Nothing -> error $ "属性が正しくありません: \"" ++ (category : length_str) ++ "\""
                Just checker ->
                  if not $ all isDigit length_str
                    then Nothing
                    else
                      let length_check = read length_str :: Int
                       in Just (name, (category, length_check))
            other ->
              error $ show other
      )
    $ fmap words
    $ lines content

