{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Language.Haskell.TH
import Pinchot
import Pinchot.Examples.Postal


main :: IO ()
main = runQ [| $(ruleParser postal) |] >>= putStr . pprint
