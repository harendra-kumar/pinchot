{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Pinchot where

import Pinchot.Intervals

import Control.Monad.Fix
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Char (isUpper)
import qualified Data.Text as X

data Rule = Rule Text (Either (Intervals Char) (Seq Branch))
  deriving (Eq, Ord, Show)

data Branch = Branch Text (Seq Rule)
  deriving (Eq, Ord, Show)

data Names = Names
  { rules :: Map Text (Either (Intervals Char) (Seq Branch))
  , branches :: Map Text (Seq Rule)
  } deriving (Eq, Ord, Show)

type Error = Text

newtype Pinchot a = Pinchot (ExceptT Error (State Names) a)
  deriving (Functor, Applicative, Monad, MonadFix)

newRule
  :: Text
  -> Either (Intervals Char) (Seq Branch)
  -> Pinchot ()
newRule name ei = Pinchot $ do
  st <- lift get
  if validName name st
    then let newSt = st { rules = M.insert name ei (rules st) }
         in lift (put newSt)
    else throwE name

validName
  :: Text
  -> Names
  -> Bool
validName name (Names rls bchs) = case X.uncons name of
  Nothing -> False
  Just (x, _) ->
    isUpper x && not (M.member name rls) && not (M.member name bchs)

newBranch
  :: Text
  -> Seq Rule
  -> Pinchot ()
newBranch name rs = Pinchot $ do
  st <- lift get
  if validName name st
    then let newSt = st { branches = M.insert name rs (branches st) }
         in lift (put newSt)
    else throwE name

terminal :: Text -> Intervals Char -> Pinchot Rule
terminal name ivls = do
  newRule name (Left ivls)
  return $ Rule name (Left ivls)

nonTerminal :: Text -> Seq (Text, Seq Rule) -> Pinchot Rule
nonTerminal name sq = do
  mapM (uncurry newBranch) sq
  return $ Rule name (Right (fmap (uncurry Branch) sq))

printTerminal
  :: Text
  -> Intervals Char
  -> Text
printTerminal name ivls = X.unlines [ openCom, com, closeCom, nt, derive ]
  where
    openCom = "{- Terminal from"
    com = "    " <> X.pack (show ivls)
    closeCom = "-}"
    nt = "newtype " <> name <> " = " <> name <> " Char"
    derive = "  deriving (Eq, Ord, Show)"

printBranch
  :: Bool
  -- ^ True if this is the first branch
  -> Branch
  -> Text
printBranch first (Branch name rules) = X.unwords (leader : name : rest)
  where
    leader = "  " <> if first then "=" else "|"
    rest = toList . fmap (\(Rule x _) -> x) $ rules

printAst :: Pinchot a -> Text
printAst = undefined
