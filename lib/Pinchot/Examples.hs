{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Examples for the use of Pinchot.
-- You wil want to look at the source code for the modules; examining
-- just the Haddocks shows you the code that the Template Haskell
-- ultimately generates.
--
-- In "Pinchot.Examples.Postal" is an example grammar for US postal
-- addresses.
--
-- "Pinchot.Examples.PostalAstAllRules" shows you how to use
-- 'allRulesToCode' and 'earleyParser', while
-- "Pinchot.Examples.PostalAstRuleTree" shows you how to use
-- 'ruleTreeToCode' and 'earleyParser'.
--
-- Two executables are included in the @pinchot@ package.  To get
-- them, compile @pinchot@ with the @executables@ Cabal flag.  The
-- @print-postal-grammar@ executable will pretty print the Haskell
-- source that results from applying 'earleyGrammar' to the 'postal'
-- grammar.  The @postal-parser@ executable takes as its first and
-- sole argument a string.  It parses the string using the 'postal'
-- grammar and pretty prints the resultin parses to standard output.

module Pinchot.Examples where

import Pinchot
import Pinchot.Examples.Postal
