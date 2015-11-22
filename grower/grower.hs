{-# LANGUAGE OverloadedStrings, OverloadedLists, RecursiveDo #-}
module Main where

import Pinchot

import Data.Monoid ((<>))

-- | A grammar for simple postal addresses.  This example would never
-- hold up to real-world usage but it gives you a flavor of how
-- Pinchot works.
postal :: Pinchot Char (Rule Char)
postal = mdo
  digit <- terminal "Digit" (include '0' '9')
  digits <- nonTerminal "Digits"
    [("DigitsEnd", []), ("DigitsNext", [digit, digits])]
  letter <- terminal "Letter" (include 'a' 'z' <> include 'A' 'Z')
  return letter

ast :: Pinchot Char (Rule Char)
ast = mdo
  one <- terminal "One" (solo '1')
  two <- terminal "Two" (solo '2')
  three <- terminal "Three" (solo '3')
  four <- terminal "Four" (solo '4')
  five <- terminal "Five" (solo '5')
  six <- terminal "Six" (solo '6')
  seven <- terminal "Seven" (solo '7')
  eight <- terminal "Eight" (solo '8')
  nine <- terminal "Nine" (solo '9')
  zero <- terminal "Zero" (solo '0')
  digit <- nonTerminal "Digit" [("D0", [zero])
    , ("D1", [one]), ("D2", [two]), ("D3", [three]), ("D4", [four])
    , ("D5", [five]), ("D6", [six]), ("D7", [seven]), ("D8", [eight])
    , ("D9", [nine])
    ]
  digits <- nonTerminal "Digits" [("DigitsEnd", []),
    ("DigitsNext", [digit, digits])]
  return digits

main :: IO ()
main = do
  allPinchotRulesToStdout "Char" ast
  sequence_ . replicate 4 $ putStrLn "--"
  ancestorsToStdout "Char" ast
