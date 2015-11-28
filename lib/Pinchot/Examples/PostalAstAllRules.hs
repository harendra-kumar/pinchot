{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Pinchot.Examples.PostalAstAllRules where

import Pinchot
import Pinchot.Examples.Postal

allRulesToCode ''Char postal

