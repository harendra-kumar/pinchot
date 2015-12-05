{-# LANGUAGE OverloadedStrings, OverloadedLists, RecursiveDo #-}
module Pinchot.Examples.Postal where

import Pinchot

import Data.Monoid ((<>))

-- | A grammar for simple U.S. postal addresses.  This example would never
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
  street <- terminalSeq "Street" "St"
  avenue <- terminalSeq "Avenue" "Ave"
  way <- terminalSeq "Way" "Way"
  boulevard <- terminalSeq "Boulevard" "Blvd"
  suffix <- nonTerminal "Suffix"
    [ ("SStreet", [street]), ("SAvenue", [avenue]), ("SWay", [way])
    , ("SBoulevard", [boulevard])]
  space <- terminal "Space" (solo ' ')
  comma <- terminal "Comma" (solo ',')

  -- Named "PostalWord" to avoid clash with Prelude.Word
  word <- list1 "PostalWord" letter
  preSpacedWord <- nonTerminal "PreSpacedWord"
    [("PreSpacedWord", [space, word])]
  preSpacedWords <- list "PreSpacedWords" preSpacedWord
  words <- nonTerminal "Words"
    [("Words", [word, preSpacedWords])]

  number <- wrap "Number" digits
  streetName <- wrap "StreetName" words
  city <- wrap "City" words
  state <- wrap "State" word
  zipCode <- wrap "ZipCode" digits
  directionSpace <- nonTerminal "DirectionSpace"
    [("DirectionSpace", [direction, space])]
  spaceSuffix <- nonTerminal "SpaceSuffix"
    [("SpaceSuffix", [space, suffix])]
  optDirection <- option "MaybeDirection" directionSpace
  optSuffix <- option "MaybeSuffix" spaceSuffix

  address <- nonTerminal "Address"
    [("Address", [ number, space, optDirection, streetName, optSuffix,
                   comma, space, city, comma, space, state,
                   space, zipCode ])]
  return address
