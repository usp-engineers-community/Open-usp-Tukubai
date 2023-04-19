#!/usr/bin/env runghc --
import Data.Ix (range)
import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import System.Environment
import System.Exit
import System.IO

{--
sm4（Open usp Tukubai）

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
    System.IO.hPutStr stderr "Usage    : sm4 <k1> <k2> <d1> <d2> <s1> <s2> [<file>]\n"
    System.IO.hPutStr stderr "Version  : Thu Apr 20 08:26:58 JST 2023\n"
    System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD)\n"
    exitWith (ExitFailure 1)

die str = hPutStr stderr ( "Error[sm4] : " ++ str ++ "\n")
          >> exitWith (ExitFailure 1)

data FieldIndex = Field Int | NF Int

main = do args <- getArgs
          case args of
              []                     -> showUsage
              ["-h"]                 -> showUsage
              ["--help"]             -> showUsage
              ["--version"]          -> showUsage
              k1:k2:d1:d2:s1:s2:rest -> sm4 rest k1 k2 d1 d2 s1 s2 Nothing
              _                      -> showUsage

fieldIndex position =
  case stripPrefix "NF" position of
    Nothing ->
      if position == "x" then
        Field 0
      else
        Field $ read position
    Just "" ->
      NF    0
    Just offset ->
      NF    $ read offset

sm4 filename_or_nothing k1 k2 d1 d2 s1 s2 sum_or_nothing = do
  file <-
    case filename_or_nothing of
      []         -> return stdin
      [filename] -> openFile filename ReadMode

  (total, primary_key', nf_or_nothing, dummy_length) <-
    sm4' file (fieldIndex k1) (fieldIndex k2)
              (fieldIndex d1) (fieldIndex d2)
              (fieldIndex s1) (fieldIndex s2) [] Nothing Nothing 0

  case (primary_key', nf_or_nothing) of
    (_, Nothing) -> return ()
    (Just primary_key, Just nf) -> do
      if d1 == "x" && d2 == "x" then
        System.IO.putStrLn $ unwords $ take (length $ fromJust primary_key') (repeat "@") ++ fmap show total
      else
        System.IO.putStrLn $ unwords $ primary_key ++ take (read d2 - read d1 + 1) (repeat "@") ++ fmap show total

  hClose file

sm4' source k1' k2' d1' d2' s1' s2' sum primary_key' nf_or_nothing dummy_length' = do
    eof <- hIsEOF source

    if eof then do
      hClose source
      return (sum, primary_key', nf_or_nothing, dummy_length')
    else do
      record <- words <$> hGetLine source
      nf     <- return $ length record

      (k1, k2, d1, d2, s1, s2) <- return (resolve nf k1', resolve nf k2', resolve nf d1', resolve nf d2', resolve nf s1', resolve nf s2')
      primary_key <- return $ take (k2 - k1 + 1) $ drop (k1 - 1) record
      columns     <- return $ fmap (\column -> read column :: Int) $ take (s2 - s1 + 1) $ drop (s1 - 1) record

      if primary_key' == Nothing || primary_key' == Just primary_key then return () else
        case primary_key' of
          Just content ->
            System.IO.putStrLn $ unwords $ take (d2 - d1 + 1) (repeat "@") ++ fmap show sum

      System.IO.putStrLn $ unwords record
      dummy_length <- return $ d2 - d1 + 1

      if any (all ((== '@'))) record then
        sm4' source k1' k2' d1' d2' s1' s2' sum (Just primary_key) nf_or_nothing dummy_length
      else if length sum == 0 then
        -- sum が空の場合は現在のレコードを初期値として与える。
        sm4' source k1' k2' d1' d2' s1' s2' columns (Just primary_key) (Just nf) dummy_length
      else if primary_key' /= Just primary_key then
        sm4' source k1' k2' d1' d2' s1' s2' columns (Just primary_key) (Just nf) dummy_length
      else do
        sum' <- return $ fmap (\double -> fst double + snd double) $ zip sum columns
        sm4' source k1' k2' d1' d2' s1' s2' sum' (Just primary_key) (Just nf) dummy_length
  where
   resolve nf field_index =
     case field_index of
       Field position -> position
       NF    offset   -> nf - offset
