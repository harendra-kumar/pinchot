{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

-- | Creates a Template Haskell AST for a parser
-- and pretty-prints it to standard output.
module Main where

import Language.Haskell.TH
import Pinchot
import Pinchot.Examples.Postal


main :: IO ()
main = runQ [| $(earleyGrammar "" postal) |] >>= putStrLn . pprint
