{-# LANGUAGE TemplateHaskell #-}
module Pinchot.Examples.PostalAstAllRules where

import Pinchot
import Pinchot.Examples.Postal
import Text.Earley

allRulesToCode ''Char postal

myParser :: Grammar r (Prod r String Char Address)
myParser = $(earleyParser postal)
