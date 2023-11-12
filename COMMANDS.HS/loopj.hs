#!/usr/bin/env rungh
import Control.Monad (foldM_, forM_, liftM, mapM)
import Data.List
import System.Directory (doesFileExist)
import System.Environment
import System.Exit (exitFailure, exitSuccess)
import System.IO

{--
loopj（Open usp Tukubai）

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
         "Usage    : loopj [-d<string>] num=<num> <file1> <file2> ..\n" ++
         "Version  : Sun Nov 12 13:32:42 JST 2023\n" ++
         "Open usp Tukubai (LINUX)\n")
        exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        []             -> showUsage
        ["-h"]         -> showUsage
        ["--help"]     -> showUsage
        ["--version"]  -> showUsage
        parameters     -> loopjArguments parameters

loopjArguments [] = showUsage
loopjArguments (argument:arguments) =
  case stripPrefix "-d" argument of
    Nothing ->
      case stripPrefix "num=" argument of
        Nothing -> showUsage
        Just num' ->
          loopj "0" (read num' :: Int) arguments
    Just dummy ->
      case arguments of
        prefixed_num:path:paths ->
          case stripPrefix "num=" prefixed_num of
            Nothing -> showUsage
            Just num' ->
              loopj dummy (read num' :: Int) (path:paths)
        _ -> showUsage

key_value (key, value) = (unwords key, value)

loopj dummy num paths = do
  forM_ paths (\path -> do
    existence <- doesFileExist path

    if existence then return () else
      error $ "ファイル '" ++ path ++ "' をオープンできません。")

  files <-
    mapM (\path ->
      if path == "-" then
        return stdin
      else
        openFile path ReadMode) paths

  records <- mapM (\file ->
    liftM (key_value . splitAt num . words) <$>
    lines                       <$>
    hGetContents file           ) files

  case findIndex (\records -> records == []) records of
    Just index ->
      error $ "0バイトファイル " ++ paths !! index ++ " は連結できません。"
    Nothing -> do
      foldM_ (\records -> \_ -> do
        key <- return $ minimum $ map (fst . (\list ->
          if list == [] then ("_", [""])
          else head list)) records
     
        if key == "_" then do
          forM_ files hClose
          exitSuccess
        else do
          putStrLn $
            unwords $
            key:(concat $
              map ((\record ->
                (if fst record == key then id
                 else map (\_ -> dummy)) $
                 snd record) . (\list -> if list == [] then ("_", [""])  else head list)) records)
          return $ map (\records' ->
            if records' == [] then
              []
            else if fst (head records') == key then
              tail records'
            else
              records') records) records [1..]

