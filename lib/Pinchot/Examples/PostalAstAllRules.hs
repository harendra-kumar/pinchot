{-# LANGUAGE TemplateHaskell #-}

-- | Provides an example of the use of 'ruleTreeToCode'.  You will
-- want to look at the source code, as it has a Template Haskell
-- splice that produces all of the data types that you see in the
-- Haddocks.
module Pinchot.Examples.PostalAstAllRules where

import Pinchot
import Pinchot.Examples.Postal

-- Earley is imported only for the type signature for 'myParser'.  The
-- Template Haskell does not need the import.
import Text.Earley (Grammar, Prod)

-- This Template Haskell splice will produce a list of declarations,
-- with one declaration for each production rule in the grammar.
-- Unlike 'ruleTreeToCode', this splice will contain every rule that
-- was defined in the 'Pinchot'.
allRulesToCode ''Char [''Eq, ''Ord, ''Show] postal

myParser :: Grammar r (Prod r String Char Address)
myParser = $(earleyParser "" postal)
