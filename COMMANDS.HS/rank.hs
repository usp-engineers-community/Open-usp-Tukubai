#!/usr/bin/env runghc --
import Data.List (stripPrefix)
import Data.Map ((!), (!?), empty, fromList, union)
import System.Environment (getArgs)
import System.IO

{--
rank（Open usp Tukubai）

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

data State = State {
  ref :: Maybe String,
  key :: Maybe String,
  index :: Int
}

showUsage :: IO ()
showUsage = do
   System.IO.hPutStr stderr "Usage    : rank [ref=<ref>] [key=<key>] [<file>]\n"
   System.IO.hPutStr stderr "Version  : Sat Apr 22 18:26:01 JST 2023\n"
   System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD)\n"

main :: IO ()
main = do args <- getArgs
          case args of
              ["-h"]        -> showUsage
              ["--help"]    -> showUsage
              ["--version"] -> showUsage
              arguments     ->
                rank arguments Data.Map.empty State {
                  ref = Nothing, key = Nothing, index = 0} stdin

collectJust [] = []

collectJust (Nothing:remaining) =
  collectJust remaining

collectJust (Just value:remaining) =
  value:collectJust remaining

-- state = (ref, rank)
rank (argument:arguments) options' state source = do
    if length options == 0 then
      case argument:arguments of
        [path] -> openFile path ReadMode >>= rank [] options' state
        _      -> showUsage
    else
      rank arguments (union options options') state source
  where
    options =
      fromList       $
      collectJust    $ 
      fmap (\name ->
        case stripPrefix (name ++ "=") argument of
          Nothing    -> Nothing
          Just label -> Just (name, label)) ["ref", "key"]

rank [] options state source = do
    eof <- hIsEOF source
 
    if eof then
      hClose source
    else do
      record <- words <$> hGetLine source
      maybe_ref <- return $
        case ref_index' of
          Nothing ->
            Nothing
          Just ref_index ->
            Just $ record !! ref_index
      maybe_key <- return $
        case key_index of
          Nothing    -> Nothing
          Just index -> Just $ record !! index
      index_duplicate <-
        if ref state /= Nothing && ref state /= maybe_ref then do
          return (1, 0)
        else if key state == Nothing then
          return (1 + index state, 0)
        else if key state /= maybe_key then
          return (1 + index state, 0)
        else
          return (index state, 1)

      System.IO.putStrLn $ unwords $ (show $ fst index_duplicate):record

      rank [] options State {
        ref = maybe_ref, key = maybe_key,
        index = fst index_duplicate + snd index_duplicate
      } source
  where
    key_index  :: Maybe Int =
      case options !? "key" of
        Nothing ->
          Nothing
        Just key ->
          Just $ read key - 1
    ref_index' :: Maybe Int =
      ((\ref -> ref - 1) <$> read <$> options !? "ref")
