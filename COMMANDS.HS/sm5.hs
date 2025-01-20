#!/usr/bin/env runghc
import Data.Ix (range)
import Data.List (stripPrefix)
import System.Environment
import System.Exit
import System.IO

{--
sm5（Open usp Tukubai）

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
    System.IO.hPutStr stderr "Usage    : sm5 <k1> <k2> <s1> <s2> [<file>]\n"
    System.IO.hPutStr stderr "Version  : Mon Jan 20 17:18:17 JST 2025\n"
    System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD)\n"
    exitWith (ExitFailure 1)

die str = hPutStr stderr ( "Error[sm5] : " ++ str ++ "\n")
          >> exitWith (ExitFailure 1)

data FieldIndex = Field Int | NF Int

main = do args <- getArgs
          case args of
              []                -> showUsage
              ["-h"]            -> showUsage
              ["--help"]        -> showUsage
              ["--version"]     -> showUsage
              k1:k2:s1:s2:rest  -> sm5 rest k1 k2 s1 s2
              _                 -> showUsage

fieldIndex position =
  case stripPrefix "NF" position of
    Nothing ->
      Field $ read position
    Just "" ->
      NF    0
    Just offset ->
      NF    $ read offset

sm5 filename_or_nothing k1 k2 s1 s2 = do
  file <-
    case filename_or_nothing of
      []         -> return stdin
      [filename] -> openFile filename ReadMode

  (total, nf_or_nothing) <- sm5' file (fieldIndex k1) (fieldIndex k2) (fieldIndex s1) (fieldIndex s2) [] Nothing

  case nf_or_nothing of
    Nothing -> return ()
    Just nf ->
      System.IO.putStrLn $ unwords $ take (1 + nf) (repeat "@") ++ fmap show total

  hClose file

sm5' source k1' k2' s1' s2' sum nf_or_nothing = do
    eof <- hIsEOF source

    if eof then do
      hClose source
      return (sum, nf_or_nothing)
    else do
      record <- words <$> hGetLine source
      nf     <- return $ length record
      (k1, k2, s1, s2) <- return (resolve nf k1', resolve nf k2', resolve nf s1', resolve nf s2')
      columns <- return $ range (s2, s1) ++ (map (\column -> read column :: Int) $ take (1 + s2 - s1) $ drop (s1 - 1) record)
      System.IO.putStrLn $ unwords record

      if any (all ((== '@'))) record then
        sm5' source k1' k2' s1' s2' sum (Just nf)
      else if length sum == 0 then
        -- sum が空の場合は現在のレコードを初期値として与える。
        sm5' source k1' k2' s1' s2' columns (Just nf)
      else
        -- sum に値がある場合は現在のレコードと加算する。
        sm5' source k1' k2' s1' s2' (fmap (\double -> fst double + snd double) $ zip sum columns) (Just $ k2 - k1)
  where
   resolve nf field_index =
     case field_index of
       Field position -> position
       NF    offset   -> nf - offset
