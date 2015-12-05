{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

-- | Creates a Template Haskell parser.  Parses strings in the postal
-- language and pretty-prints the result.
module Main where

import Pinchot
import Pinchot.Examples.Postal
import System.Environment (getArgs)
import Text.Show.Pretty (ppShow)

import Text.Earley (Prod, Grammar, parser, allParses)

ruleTreeToCode ''Char [''Show] postal

postalParser :: Grammar r (Prod r String Char Address)
postalParser = $(earleyParser "" postal)

main :: IO ()
main = do
  a1:[] <- getArgs
  putStrLn . ppShow $ allParses (parser postalParser) a1
