{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Pinchot.Examples.PostalAstAllRules where

import Pinchot
import Pinchot.Examples.Postal
import Text.Earley
import Data.Text

allRulesToCode ''Char postal

myParser :: Grammar r (Prod r Text Char Address)
myParser = $(ruleParser postal)
