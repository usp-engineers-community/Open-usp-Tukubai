#!/usr/bin/env runghc
import Codec.Compression.Zlib.Internal (compress, defaultCompressParams, gzipFormat) -- zlib をインストールすること。
import Data.Array ((!))
import Data.ByteString (hPut, pack, unpack)
import Data.ByteString.Lazy.Internal (toStrict)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Char (isDigit)
import Data.Foldable (for_)
import Data.List.Split (splitOn) -- split をインストールすること。
import Data.Map (empty, insert, lookup, member, toList)
import Data.Maybe (catMaybes)
import Data.Set hiding (drop, foldl, map, take)
import System.Directory (doesPathExist)
import System.Environment
import System.Exit (exitFailure, exitSuccess)
import System.IO
import Prelude hiding (lookup)

{--
keycut（Open usp Tukubai）

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
  hPutStr
    stderr
    ( "Usage    : keycut [options] <filename> <file>\n"
        ++ "Option   : -d キーの削除 / -a ファイル追記 / -c 圧縮\n"
        ++ "Version  : Mon Jan 20 17:18:16 JST 2025\n"
        ++ "Open usp Tukubai (LINUX)\n"
    )
  exitFailure

data Option = Append | Delete | Compress deriving (Eq, Show)

option_of_flag 'a' = Just Append
option_of_flag 'd' = Just Delete
option_of_flag 'z' = Just Compress
option_of_flag _ = Nothing

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> showUsage
    ["-h"] -> showUsage
    ["--help"] -> showUsage
    ["--version"] -> showUsage
    parameters ->
      let (flags, arguments) = span (\parameter -> head parameter == '-') parameters
       in let options =
                catMaybes $
                  Prelude.map option_of_flag $
                    concat $
                      fmap tail flags
           in -- 開いたファイルのディスクリプタが格納される。
              let descriptors = Data.Map.empty
               in let key_fields = Data.Set.empty
                   in case arguments of
                        [filename] -> do
                          return stdin >>= keycut key_fields options filename descriptors
                        [filename, path_file] ->
                          if path_file == "-"
                            then return stdin >>= keycut key_fields options filename descriptors
                            else do
                              openFile path_file ReadMode >>= keycut key_fields options filename descriptors
                        _ ->
                          showUsage

-- 出力先のファイル名を作成する。
render filename record key_fields =
  case filename of
    [] -> []
    '%' : field_index' ->
      case span isDigit field_index' of
        ("", remaining) ->
          render remaining record key_fields
        (field_index', remaining) ->
          let field_index = read field_index'
           in let field = record !! (field_index - 1)
               in let key_fields' = Data.Set.insert field_index key_fields
                   in case remaining of
                        '.' : remaining' ->
                          case span isDigit remaining' of
                            ("", remaining'') ->
                              field ++ "." ++ render remaining'' record key_fields'
                            (start_position, remaining'') ->
                              case remaining'' of
                                '.' : remaining''' ->
                                  case span isDigit remaining''' of
                                    ("", remaining'''') ->
                                      drop (read start_position) field ++ render remaining'''' record key_fields'
                                    (length, remaining''''') ->
                                      -- 部分文字列の開始位置と長さが指定される場合:
                                      take (read length) $ drop (read start_position) field ++ render remaining''''' record key_fields'
                                other ->
                                  drop (read start_position) field ++ render other record key_fields'
                        remaining' ->
                          -- フィールド全体を指示した場合:
                          field ++ render remaining' record key_fields'
    other : filename' ->
      other : (render filename' record key_fields)

-- 含まれるキー・フィールドを求める。
keys_of :: String -> Set Int
keys_of [] = Data.Set.empty
keys_of ('%' : field_index') =
  case span isDigit field_index' of
    ("", remaining) ->
      keys_of remaining
    (field_index', remaining) ->
      Data.Set.insert (read field_index') $ keys_of remaining
keys_of (other : remaining) =
  keys_of remaining

keycut key_fields options filename descriptors file = do
  eof <- hIsEOF file

  if eof
    then do
      -- 開いているファイルをすべて閉じる。
      for_ (map snd $ Data.Map.toList descriptors) hClose
      hClose file
    else do
      record <- words <$> hGetLine file
      let filename_target = render filename record key_fields
       in do
            (target, descriptors') <-
              case lookup filename_target descriptors of
                Nothing -> do
                  -- 新しくファイルを開く場合:
                  file <-
                    if Compress `elem` options
                      then do
                        -- -z オプションの処理を行う。
                        handle <- openBinaryFile filename_target mode
                        hSetBinaryMode handle True
                        return handle
                      else openFile filename_target mode
                  return (file, Data.Map.insert filename_target file descriptors)
                Just descriptor -> do
                  -- すでにファイルを開いている場合:
                  return (descriptor, descriptors)

            if Delete `elem` options
              then -- -d オプションの処理を行う。

                let keys = keys_of filename
                 in do
                      writeLn target
                        $ unwords
                        $ map snd
                        $ Prelude.filter
                          ( \(index, value) ->
                              not $ Data.Set.member index keys
                          )
                        $ zip [1 ..] record
              else
                writeLn target $
                  unwords $
                    record
            keycut key_fields options filename descriptors' file
  where
    mode = if Append `elem` options then AppendMode else WriteMode
    writeLn target s =
      -- オプションを認識して必要であれば GZip で処理して書き込みを行う。
      if Compress `elem` options
        then
          let payload = fromString $ s ++ "\n"
           in hPut target (toStrict $ compress gzipFormat defaultCompressParams payload)
        else hPutStrLn target s

