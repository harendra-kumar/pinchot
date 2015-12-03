{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
module Pinchot.Examples.PostalAstAllRules where

import Pinchot
import Pinchot.Examples.Postal
import Text.Earley
import Data.Text (Text)

allRulesToCode ''Char postal

myParser = $(ruleParser postal)
