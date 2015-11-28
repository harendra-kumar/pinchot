{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Pinchot
import Postal

import Prelude hiding (Word)

makeAst "Char" postal


main :: IO ()
main = do
  allPinchotRulesToStdout "Postal" "Char" (Just "Data.Char") postal
  sequence_ . replicate 4 $ putStrLn "--"
  ancestorsToStdout "Postal" "Char" (Just "Data.Char") postal
