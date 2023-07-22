#!/usr/bin/env runghc --
import Codec.MIME.Type
import Codec.MIME.Parse (parseMIMEMessage)
import Control.Monad (forM_)
import Data.Ix (range)
import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import qualified Data.Text.IO
import Data.Text (unpack)
import Data.Text.Internal (pack)
import Data.Maybe (mapMaybe)
import System.Environment
import System.Exit (exitSuccess)
import System.IO

{--
mime-read（Open usp Tukubai）

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
        hPutStr stderr (
         "Usage    : mime-read <name> [<MIME-file>]\n" ++
         "           mime-read -v [<MIME-file>]\n"     ++
         "Version  : Sat Jul 22 20:19:49 JST 2023\n" ++
         "Open usp Tukubai (LINUX)\n")

main :: IO ()
main = do
    args <- getArgs
    case args of
        []             -> showUsage
        ["-h"]         -> showUsage
        ["--help"]     -> showUsage
        ["--version"]  -> showUsage
        "-v":mime_file ->
          case mime_file of
            []     -> return stdin           >>= mime_read_list
            [path] -> openFile path ReadMode >>= mime_read_list
        name:mime_file ->
          case mime_file of
            []     -> return stdin           >>= mime_read name
            [path] -> openFile path ReadMode >>= mime_read name

dequote quoted =
  case quoted of
    '"':quoted' ->
      if last quoted' /= '"' then quoted else
        take (length quoted' - 1) quoted'
mime_read_list source = do
  content <- mime_val_content<$> parseMIMEMessage <$> pack <$> hGetContents source

  case content of
    Single _ -> return ()
    Multi portions ->
      forM_ (zip portions $ range (1, length portions)) (\(portion, index) -> do
        case  mime_val_content portion of
          Single _ -> return ()
          Multi multiparts -> do
            forM_ multiparts (\multipart -> do
              forM_ (mimeParams $ mime_val_type multipart) (\mime_param -> do
                if paramName mime_param /= pack "name" then return () else
                  putStrLn $ unwords [show index, unpack $ paramValue mime_param]))
        forM_ (filter (\header ->
          paramName header == pack "content-disposition" ||
          paramName header == pack "content-type" ) $ mime_val_headers portion) (\header -> do
            case splitOn " \t" $ unpack $ paramValue header of
              [_, value]   ->
                case stripPrefix "name=" value of
                  Just name ->
                    putStrLn $ unwords [show index, dequote name]
                  _         ->
                    return ()
              [attachment] ->
                case words attachment of
                  [_, parameter] ->
                    case stripPrefix "filename=" parameter of
                      Just parameter' ->
                        putStrLn $ unwords [show index, dequote parameter']
                      _             ->
                        return ()))
  hClose source

mime_read name source = do
  content <- mime_val_content <$> parseMIMEMessage <$> pack <$> hGetContents source
  stdout_binary <- openBinaryFile "/dev/fd/1" WriteMode

  case content of
    Single _ -> return ()
    Multi portions -> do
      forM_ portions (\portion -> do
        case mime_val_content portion of
          Single  _ -> return ()
          Multi multiparts ->
            forM_ multiparts (\multipart -> do
              if notElem name $
                 mapMaybe ((\name_value ->
                   case name_value of
                     [_, value] -> stripPrefix "name=" value
                     _          -> Nothing). splitOn " \t" . unpack . paramValue) $
                 filter (\header ->
                   paramName header == pack "content-type") 
                 (mime_val_headers multipart) then
                   return ()
              else
                 case mime_val_content multipart of
                   Multi _ ->
                     -- Multi が返却される例は不明だから無視している。
                     return ()
                   Single content ->
                     hPutStr stdout_binary $ unpack content))
      return ()

  hFlush stdout_binary
  hClose source

