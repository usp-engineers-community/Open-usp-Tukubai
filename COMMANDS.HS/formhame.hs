import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Applicative hiding ((<|>), many)
import Data.List 
import qualified Data.Text as DT (replace)
import qualified Text.Regex as RE (mkRegex,subRegex)

{--
formhame（Open usp Tukubai）

designed by Nobuaki Tounaka
written by Ryuichi Ueda

The MIT License

Copyright (C) 2013 Universal Shell Programming Laboratory

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
showUsage = do System.IO.hPutStr stderr ("Usage    : formhame <html_template> <data>\n" ++ 
                "Thu Dec 12 23:26:27 JST 2013\n" ++
                "Open usp Tukubai (LINUX+FREEBSD), Haskell ver.\n")

data Opts = NormalOpts TemplateFile DataFile | Error String deriving Show

type KeyValue = (String,String)

type TemplateFile = String
type DataFile = String

main :: IO ()
main = do args <- getArgs
          case args of
              ["-h"]     -> showUsage
              ["--help"] -> showUsage
              _          -> main' (setOpts args) 

main' :: Opts -> IO ()
main' (NormalOpts tempfile datafile) = do tempcs <- readF tempfile
                                          datacs <- readF datafile
                                          formhame ( readData (lines datacs) ) (lines tempcs)

readF :: String -> IO String
readF "-" = getContents
readF f = readFile f

readData :: [String] -> [KeyValue]
readData lns = map (f . words) lns
    where f (a:b:c) = (a,b)
          f (a:[])  = (a,"")

formhame :: [KeyValue] -> [String] -> IO ()
formhame _ [] = return ()
formhame kvs (ln:lns) 
  | pickedkvs == [] = putStrLn ln >> formhame kvs lns
  | otherwise       = replaceValue tp pickedkvs (fst x) >> formhame kvs (snd x)
    where pickedkvs = pickKvs kvs ln
          x = cutTargetHtml tp (ln:lns)
          tp = checkType ln

replaceValue :: String -> [KeyValue] -> [String] -> IO ()
replaceValue "text"  (kv:[]) (ln:[]) = replaceValueText kv ln
replaceValue "radio" (kv:[]) (ln:[]) = replaceValueRadio kv ln
replaceValue "checkbox" kvs  (ln:[]) = replaceValueChkbox kvs ln
replaceValue "textarea" (kv:[]) lns  = replaceTextArea kv (unwords lns)
replaceValue "select"   (kv:[]) lns  = replaceSelect kv lns
replaceValue _ _ lns                 = putStr $ unlines lns

{-- RADIO --}
replaceValueRadio :: KeyValue -> String -> IO ()
replaceValueRadio (k,v) ln = putStrLn newln
    where bareln = RE.subRegex ckd ln "" 
          ckd = RE.mkRegex "(checked=\"checked\"|checked) *"
          newln = checkRadio v bareln

checkRadio :: String -> String -> String
checkRadio val ln = if vstr `isInfixOf` ln then newln else ln
    where vstr = "value=\"" ++ val ++ "\""
          re = RE.mkRegex " */>$"
          newln = RE.subRegex re ln " checked=\"checked\" />"

{-- CHECKBOX --}
replaceValueChkbox :: [KeyValue] -> String -> IO ()
replaceValueChkbox kvs ln = putStrLn newln
    where bareln = RE.subRegex ckd ln "" 
          ckd = RE.mkRegex "(checked=\"checked\"|checked) *"
          newln = checkChkbox kvs bareln

checkChkbox :: [KeyValue] -> String -> String
checkChkbox []          ln = ln
checkChkbox ((k,v):kvs) ln = if f v ln then rep ln else checkChkbox kvs ln
    where f val ln = ("value=\"" ++ val ++ "\"") `isInfixOf` ln
          rep str = RE.subRegex ckd str " checked=\"checked\" />"
          ckd = RE.mkRegex " */>"

{-- TEXT --}
replaceValueText :: KeyValue -> String -> IO ()
replaceValueText (k,v) ln = putStrLn newln
    where re = RE.mkRegex "value=\"[^\"]*\""
          str = "value=\"" ++ v ++ "\""
          newln = if isValue ln then RE.subRegex re ln str else insertValue ln str

{-- SELECT --}
replaceSelect :: KeyValue -> [String] -> IO ()
replaceSelect (k,v) lns = putStr $ unlines ans
    where barelns = map (\ln -> RE.subRegex re ln "") lns
          re = RE.mkRegex " *(selected=\"selected\"|selected) *"
          target = "value=\"" ++ v ++ "\""
          ans = [ if target `isInfixOf` ln then RE.subRegex re2 ln sel else ln | ln <- barelns ]
          re2 = RE.mkRegex "<option"
          sel = "<option selected=\"selected\""

getSelectBlock :: [String] -> ([String],[String]) -> ([String],[String])
getSelectBlock []       (a,[]) = (a,[])
getSelectBlock (ln:lns) (a,[])
  | "</select>" `isInfixOf` ln = (a ++ [ln],lns)
  | otherwise                  = getSelectBlock lns (a ++ [ln],[])

{-- TEXTAREA --}
replaceTextArea :: KeyValue -> String -> IO ()
replaceTextArea (k,v) ln = putStr $ unlines ans
    where lns = lines $ RE.subRegex re ln ">\n<"
          re = RE.mkRegex ">.*<"
          re2 = RE.mkRegex "\\\\n"
          vn = RE.subRegex re2 v "\n"
          ans = (head lns) : vn : (drop 1 lns)

getTextBoxBlock :: [String] -> ([String],[String]) -> ([String],[String])
getTextBoxBlock []       (a,[]) = (a,[])
getTextBoxBlock (ln:lns) (a,[])
  | "</textarea>" `isInfixOf` ln = (a ++ [ln],lns)
  | otherwise                    = getTextBoxBlock lns (a ++ [ln],[])

{-- utilities --}
isValue :: String -> Bool
isValue ln = "value=\"" `isInfixOf` ln

insertValue :: String -> String -> String
insertValue str vstr = unwords $ a ++ [vstr,b]
   where ws = words str
         a = init ws
         b = last ws

checkType :: String -> String
checkType ln
  | "type=\"text\"" `isInfixOf` ln     = "text"
  | "type=\"radio\"" `isInfixOf` ln    = "radio"
  | "type=\"checkbox\"" `isInfixOf` ln = "checkbox"
  | "<textarea " `isInfixOf` ln        = "textarea"
  | "<select " `isInfixOf` ln          = "select"
  | otherwise = "unknown"

cutTargetHtml :: String -> [String] -> ([String],[String])
cutTargetHtml tp []      = ([],[])
cutTargetHtml tp (ln:[]) = ([ln],[])
cutTargetHtml tp lns
  | tp == "textarea"  = getTextBoxBlock lns ([],[])
  | tp == "select"    = getSelectBlock lns ([],[])
  | otherwise         = ([head lns], drop 1 lns)

pickKvs :: [KeyValue] -> String -> [KeyValue]
pickKvs []  _  = []
pickKvs kvs ln = filter (\x -> f x ln) kvs
    where f (k,v) ln = isInfixOf ("name=\"" ++ k ++ "\"") ln

setOpts :: [String] -> Opts
setOpts as = case parse args "" ((unwords as) ++ " ") of
                  Right opt -> opt
                  Left err -> Error ( show err ) 

args = do f1 <- try(filename) <|> return "-"
          char ' '
          f2 <- try(filename) <|> return "-"
          return $ NormalOpts f1 f2

filename = many1 ( try(letter) <|> try(digit) <|> symbol )

symbol = oneOf "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
