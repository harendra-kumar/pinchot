{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Pinchot.Examples.PostalAst where

import Pinchot
import Pinchot.Examples.Postal

import Prelude hiding (Word)

makeAst "Char" postal

