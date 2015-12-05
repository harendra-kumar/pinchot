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

module Pinchot.Examples where

import Pinchot
