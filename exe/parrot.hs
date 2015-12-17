{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}

-- | Reads the string given as the first argument.  Parses it and
-- then, for each parse result, uses 'terminals' to print the
-- terminals.  Each result output should be the same as the input.
module Main where

import Data.Foldable (toList)
import Pinchot
import Pinchot.Examples.Postal
import System.Environment (getArgs)

import Text.Earley (parser, fullParses)

ruleTreeToTypes noOptics ''Char [] postal

main :: IO ()
main = do
  a1:[] <- getArgs
  let (ls, _) = fullParses (parser $(earleyGrammar "" postal)) a1
      printSeq = putStrLn . toList . terminals
  mapM_ printSeq ls
