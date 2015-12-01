{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
module Pinchot.Examples.PostalAstAllRules where

import Pinchot
import Pinchot.Examples.Postal

allRulesToCode ''Char postal

myParser = $(ruleParser postal)
