#!/usr/bin/env runghc
import Control.Monad (fmap, foldM_, forM_)
import Data.Ix (range)
import Data.List (stripPrefix, transpose)
import Data.Maybe (fromJust, isJust)
import System.Environment
import System.Exit
import System.IO
import System.Process (callProcess)

{--
unmap（Open usp Tukubai）

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
    System.IO.hPutStr stderr "Usage    : unmap       num=<n>x<m> [<file>]\n"
    System.IO.hPutStr stderr "           unmap +yarr num=<n>x<m> [<file>]\n"
    System.IO.hPutStr stderr "           unmap +arr  num=<n>x<m> [<file>]\n"
    System.IO.hPutStr stderr "Version  : Mon Jan 20 17:18:17 JST 2025\n"
    System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD)\n"
    exitWith (ExitFailure 1)

data Mode = None | Arr | Yarr deriving Show

main = do args <- getArgs
          case args of
              []              -> showUsage
              ["-h"]          -> showUsage
              ["--help"]      -> showUsage
              ["--version"]   -> showUsage
              -- "+yarr":options -> unmap Yarr Nothing options
              -- "+arr":options  -> unmap Arr  Nothing options
              options         -> unmap None Nothing options

unmap mode header_or_nothing [] = showUsage

unmap mode header_or_nothing options = do
   source <- source'
   num    <- return $ fromJust num'

   header <-
     case header_or_nothing of
       Nothing ->
         drop (head num) <$> words <$> hGetLine source
       Just header ->
         header

   heading <- return $
     case num of
       [n]    -> 1
       [n, m] -> m

   unmap' (drop heading header) num source
   hClose source
  where
    stride' = fmap read $ stripPrefix "-" (head options) :: Prelude.Maybe Int
    num_options =
        if Nothing == stride' then
          (head options, tail options)
        else
          let options' = tail options in
            (head options', tail options')
    num' = 
        fmap (fmap (read :: String -> Int)  . words) $
        fmap (fmap $ \character ->
          if character == 'x' then ' '
          else character) $
        stripPrefix "num=" $
        fst num_options
    source' =
      case snd num_options of
        []         -> return stdin
        [filename] -> openFile filename ReadMode
        _          -> do
          showUsage
          return stdout

unmap' header num source = do
    eof <- hIsEOF source
 
    if eof then return () else do
      records <- mapM (\index ->
          words               <$>
          hGetLine source     ) $
        range (1, length num)

      leading <- return $ take (head num) $ head records

      mapM_ System.IO.putStrLn  $
        map (\record ->
          unwords $ leading ++ [record]) $
        map (unwords . list_of) $
        zip header              $
        map unwords             $
        transpose (map (drop (1 + head num)) records)

      unmap' header num source
  where
    list_of tuple = [fst tuple, snd tuple]
