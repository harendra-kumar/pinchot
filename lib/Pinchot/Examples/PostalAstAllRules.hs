{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
module Pinchot.Examples.PostalAstAllRules where

import Pinchot
import Pinchot.Examples.Postal
import Text.Earley

allRulesToCode ''Char postal

myParser :: Grammar r (Prod r e t Address)
myParser = $(ruleParser postal)
