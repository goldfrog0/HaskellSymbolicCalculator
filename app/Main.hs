module Main where

import ParsingLib
import MyLib
import Data.Maybe

main :: IO ()
main = do
  input <- getLine
  print $ (eval.fst) $ fromJust (runParser exprP input)
