{-# LANGUAGE TemplateHaskell #-}

-- | Creates a Template Haskell parser.  Parses strings in the postal
-- language and pretty-prints the result.
module Main where

import Pinchot
import Pinchot.Examples.Postal
import System.Environment (getArgs)
import Text.Show.Pretty (ppShow)

import Text.Earley (Prod, Grammar, parser, fullParses)

ruleTreeToCode ''Char [''Show] postal

postalGrammar :: Grammar r (Prod r String Char Address)
postalGrammar = $(earleyGrammar "" postal)

main :: IO ()
main = do
  a1:[] <- getArgs
  putStrLn . ppShow $ fullParses (parser postalGrammar) a1
