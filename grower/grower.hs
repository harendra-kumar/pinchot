{-# LANGUAGE OverloadedStrings, OverloadedLists, RecursiveDo #-}
module Main where

import Pinchot

main :: IO ()
main = astToStdout $ mdo
  one <- terminal "One" (alone '1')
  two <- terminal "Two" (alone '2')
  three <- terminal "Three" (alone '3')
  four <- terminal "Four" (alone '4')
  five <- terminal "Five" (alone '5')
  six <- terminal "Six" (alone '6')
  seven <- terminal "Seven" (alone '7')
  eight <- terminal "Eight" (alone '8')
  nine <- terminal "Nine" (alone '9')
  zero <- terminal "Zero" (alone '0')
  digit <- nonTerminal "Digit" ("D0", [zero])
    [ ("D1", [one]), ("D2", [two]), ("D3", [three]), ("D4", [four])
    , ("D5", [five]), ("D6", [six]), ("D7", [seven]), ("D8", [eight])
    , ("D9", [nine])
    ]
  digits <- nonTerminal "Digits" ("DigitsEnd", [])
    [("DigitsNext", [digit, digits])]
  return digits
