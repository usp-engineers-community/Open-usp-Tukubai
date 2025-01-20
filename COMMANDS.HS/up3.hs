#!/usr/bin/env runghc
import Control.Monad (fmap, void)
import Data.List (stripPrefix)
import System.Environment
import System.Exit
import System.IO

{--
up3（Open usp Tukubai）

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
    System.IO.hPutStr stderr "Usage    : up3 key=<key> <master> [<tran>]\n"
    System.IO.hPutStr stderr "Version  : Mon Jan 20 17:18:17 JST 2025\n"
    System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD)\n"
    exitWith (ExitFailure 1)

main = do args <- getArgs
          case args of
              []                  -> showUsage
              ["-h"]              -> showUsage
              ["--help"]          -> showUsage
              ["--version"]       -> showUsage
              [key, path_master]  ->
                up3' key path_master stdin
              [key, path_master, path_tran] -> do
                tran <- openFile path_tran ReadMode
                up3' key path_master tran
              _                   -> showUsage

data Index = Field Int | NF Int deriving Show

extract_key record [] = []

extract_key record ((Field index'):fields) =
  let index = index' - 1 in
    (record !! index):(extract_key record fields)

extract_key record ((NF offset):fields) =
  let index = length record + offset - 1  in
    (record !! index):(extract_key record fields)

up3' prefixed_key path_master tran = do
    master <- openFile path_master ReadMode
    key_fields <-
      case stripPrefix "key=" prefixed_key of
        Nothing  -> do
          showUsage
          return []
        Just key ->
          return $ foldMap (fmap parse_index . words . fmap replace_slash) $ words $ fmap replace_atmark key
    trail_key_or_nothing <- up3 key_fields master tran (Nothing, [])

    case trail_key_or_nothing of
      (Nothing, _) ->
        return ()
      (Just trail_key, residual) -> do
        System.IO.putStrLn $ unwords residual
        void $ up3_tran key_fields master tran (trail_key, [])

    hClose tran
    hClose master
  where
    parse_index index =
      case stripPrefix "NF" index of
        Nothing ->
          Field $ read index
        Just "" ->
          NF 0
        Just offset ->
          NF $ read $ offset
    replace_slash character =
      if character == '/' then ' ' else character
    replace_atmark character =
      if character == '@' then ' ' else character

up3 key_fields master tran residual_or_nothing = do
    eof_master <- hIsEOF master 
 
    if eof_master then do
      hClose master
      return residual_or_nothing
    else do
      record <- words <$> hGetLine master
      key    <- return $ extract_key record key_fields
      System.IO.putStrLn $ unwords record 
      residual_or_nothing' <- up3_master key_fields master tran key Nothing
      up3 key_fields master tran residual_or_nothing'

up3_master key_fields master tran key residual_or_nothing = do
    eof_master <- hIsEOF master

    if eof_master then
      case residual_or_nothing of
        Nothing ->
          return (Nothing, [])
        Just residual ->
          return (Just key, residual)
    else do
      record <- words <$> hGetLine master

      key' <- return $ extract_key record key_fields

      if key /= key' then do
        case residual_or_nothing of
          Nothing -> return ()
          Just residual ->
            System.IO.putStrLn $ unwords residual

        record' <- up3_tran key_fields master tran (key, [])
        System.IO.putStrLn $ unwords record
        up3_master key_fields master tran key' (Just $ snd record')
      else do
        System.IO.putStrLn $ unwords record
        up3_master key_fields master tran key residual_or_nothing

up3_tran key_fields master tran key_record = do
    eof_tran <- hIsEOF tran

    if eof_tran then
      return key_record
    else do
      record      <- words <$> hGetLine tran
      key_record' <- return $ (extract_key record key_fields, record)

      if fst key_record /= fst key_record' then
        return key_record'
      else do
        System.IO.putStrLn $ unwords record
        up3_tran key_fields master tran key_record
