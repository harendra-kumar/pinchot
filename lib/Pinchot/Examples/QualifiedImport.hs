{-# LANGUAGE TemplateHaskell #-}

-- | Provides an example of the use of 'earleyParser' with a qualified
-- import of the data types that comprise the grammar.

module Pinchot.Examples.QualifiedImport where

import Pinchot
import qualified Pinchot.Examples.PostalAstRuleTree as Ast
import qualified Pinchot.Examples.Postal as Postal

-- Earley is imported only for the type signature for 'myParser'.  The
-- Template Haskell does not need the import.
import Text.Earley (Grammar, Prod)

-- | Earley parser created using Template Haskell.

myParser :: Grammar r (Prod r String Char Ast.Address)
myParser = $(earleyGrammar "Ast" Postal.postal)
