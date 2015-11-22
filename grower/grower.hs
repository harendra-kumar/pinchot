{-# LANGUAGE OverloadedStrings, OverloadedLists, RecursiveDo #-}
module Main where

import Pinchot

import Data.Monoid ((<>))
import Data.Sequence (fromList)

-- | A grammar for simple postal addresses.  This example would never
-- hold up to real-world usage but it gives you a flavor of how
-- Pinchot works.
postal :: Pinchot Char (Rule Char)
postal = mdo
  digit <- terminal "Digit" (include '0' '9')
  digits <- list1 "Digits" digit
  letter <- terminal "Letter" (include 'a' 'z' <> include 'A' 'Z')
  north <- terminal "North" (solo 'N')
  south <- terminal "South" (solo 'S')
  east <- terminal "East" (solo 'E')
  west <- terminal "West" (solo 'W')
  direction <- nonTerminal "Direction"
    [ ("DNorth", [north]), ("DSouth", [south]), ("DEast", [east])
    , ("DWest", [west])]
  street <- terminalSeq "Street" (fromList "St")
  avenue <- terminalSeq "Avenue" (fromList "Ave")
  way <- terminalSeq "Way" (fromList "Way")
  boulevard <- terminalSeq "Boulevard" (fromList "Blvd")
  suffix <- nonTerminal "Suffix"
    [ ("SStreet", [street]), ("SAvenue", [avenue]), ("SWay", [way])
    , ("SBoulevard", [boulevard])]
  space <- terminal "Space" (solo ' ')
  comma <- terminal "Comma" (solo ',')
  return comma

{-
  word <- list1 "Word" letter
  preSpacedWord <- nonTerminal "PreSpacedWord"
    [("PreSpacedWord", [space, word])]
  preSpacedWords <- list "PreSpacedWords" preSpacedWord
  words <- nonTerminal "Words"
    [("Words", [word, preSpacedWords])]
-}


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
