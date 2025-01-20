#!/usr/bin/env runghc
import Control.Monad.Zip (mzip)
import Data.List (intercalate, isPrefixOf, partition)
import Data.Map ((!?), fromList)
import System.Environment
import System.Exit (exitFailure)
import System.IO
import Text.Regex.Posix ((=~))

{--
nameread（Open usp Tukubai）

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
        System.IO.hPutStr stderr "Usage    : nameread [-d<c>|-i<string>|-e|-el] <name> [<namefile>]\n"
        System.IO.hPutStr stderr "Version  : Mon Jan 20 17:18:16 JST 2025\n"
        System.IO.hPutStr stderr "Open usp Tukubai (LINUX+FREEBSD)\n"
        exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
                []            -> showUsage
                ["-h"]        -> showUsage
                ["--help"]    -> showUsage
                ["--version"] -> showUsage
                _             -> nameread args

nameread arguments' = do
    options <- return $
                 fromList $
                 fmap (\option ->
                   if isPrefixOf "-e" option then
                       (drop 1 option, arguments !! 0)
                   else
                     let name_value = tail option in
                       ([head name_value], tail name_value)) options'
    source <-
      case arguments of
        [name] ->
          return stdin
        [name, filename] ->
          openFile filename ReadMode
        _ -> do
          showUsage
          openFile "/dev/null" ReadMode
    nameread' source options arguments
  where
    (options', arguments) = partition (\argument -> head argument == '-') arguments'

nameread' source options arguments = do
  eof <- hIsEOF source

  if eof then hClose source else do
    pattern_or_nothing <- return $ mconcat $ map (\name -> mzip (Just name) (options !? name)) ["e", "el"]

    line <- hGetLine source
    case words line of
      [] -> return ()
      name:value_or_nothing -> do
        case pattern_or_nothing of
          Nothing ->
            if name /= arguments !! 0 then return ()
            else
              System.IO.putStrLn $
              (if with_name then name ++ " " else "") ++
              case value_or_nothing of
                [] -> null_alternative
                fields -> intercalate delimiter  fields
          Just pattern ->
            case name =~ snd pattern :: [[String]] of
              [_] -> System.IO.putStrLn $
                (if fst pattern == "e" then "" else name ++ delimiter) ++
                case value_or_nothing of
                  []     -> null_alternative
                  fields -> intercalate delimiter fields
              _     -> return ()
    nameread' source options arguments
  where
    delimiter =
      case options !? "d" of
        Nothing -> " "
        Just string -> string
    null_alternative =
      case options !? "i" of
        Nothing -> ""
        Just string -> string
    with_name =
      case options !? "l" of
        Nothing -> False
        Just "" -> True
