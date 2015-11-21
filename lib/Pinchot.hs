{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Pinchot
  ( Intervals(..)
  , alone
  , Rule
  , Pinchot
  , terminal
  , nonTerminal
  , pinchotToAst
  , astToStdout
  ) where

import Pinchot.Intervals

import Control.Monad.Fix
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Sequence (Seq)
import Data.Text (Text, unpack)
import Data.Char (isUpper)
import qualified Data.Text as X
import qualified Data.Text.IO as XIO
import System.Exit (exitFailure)
import qualified System.IO as IO

data Rule = Rule Text (Either (Intervals Char) (Branch, Seq Branch))
  deriving (Eq, Ord, Show)

data Branch = Branch Text (Seq Rule)
  deriving (Eq, Ord, Show)

data Names = Names
  { rules :: Map Text (Either (Intervals Char) (Branch, Seq Branch))
  , branches :: Map Text (Seq Rule)
  , nextIndex :: Int
  , allRules :: Map Int Rule
  } deriving (Eq, Ord, Show)

type Error = Text

newtype Pinchot a = Pinchot (ExceptT Error (State Names) a)
  deriving (Functor, Applicative, Monad, MonadFix)

newRule
  :: Text
  -> Either (Intervals Char) (Branch, Seq Branch)
  -> Pinchot ()
newRule name ei = Pinchot $ do
  st <- lift get
  if validName name st
    then let newSt = st { rules = M.insert name ei (rules st)
                        , nextIndex = succ (nextIndex st)
                        , allRules = M.insert (nextIndex st) (Rule name ei)
                            (allRules st)
                        }
         in lift (put newSt)
    else throwE name

validName
  :: Text
  -> Names
  -> Bool
validName name (Names rls bchs _ _) = case X.uncons name of
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

nonTerminal :: Text -> (Text, Seq Rule) -> Seq (Text, Seq Rule) -> Pinchot Rule
nonTerminal name b1 sq = do
  uncurry newBranch b1
  mapM (uncurry newBranch) sq
  let branches = Right (uncurry Branch b1, fmap (uncurry Branch) sq)
  newRule name branches
  return $ Rule name branches

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
printBranch first (Branch name rules) = X.unwords (leader : name : rest) <> "\n"
  where
    leader = "  " <> if first then "=" else "|"
    rest = toList . fmap (\(Rule x _) -> x) $ rules

printNonTerminal
  :: Text
  -> (Branch, Seq Branch)
  -> Text
printNonTerminal name (b1, bs) = X.concat (line1 : linesRest)
  where
    line1 = "data " <> name <> "\n"
    linesRest = printBranch True b1 : toList (fmap (printBranch False) bs)
      <> ["  deriving (Eq, Ord, Show)"]

printRule
  :: Rule
  -> Text
printRule (Rule name ei) = case ei of
  Left term -> printTerminal name term
  Right nonTerm -> printNonTerminal name nonTerm

printAllRules :: Map Int Rule -> Text
printAllRules = X.concat . intersperse "\n\n"
  . fmap printRule
  . fmap snd
  . M.toAscList

printAst :: Map Text (Either (Intervals Char) (Branch, Seq Branch)) -> Text
printAst mp = terminals <> nonTerminals
  where
    label lbl = X.unlines ["--", "-- " <> lbl, "--", ""]
    terminals = label "Terminals"
      <> X.concat (intersperse "\n" (fmap (uncurry printTerminal) terms))
    nonTerminals = label "Non terminals"
      <> X.concat (intersperse "\n" (fmap (uncurry printNonTerminal) nonTerms))
    terms = mapMaybe f . M.toList $ mp
      where
        f (x, ei) = either (\i -> Just (x, i)) (const Nothing) ei
    nonTerms = mapMaybe f . M.toList $ mp
      where
        f (x, ei) = either (const Nothing) (\p -> Just (x, p)) ei

allPinchotRules :: Pinchot a -> Either Text Text
allPinchotRules (Pinchot exc) = case eiErr of
  Left err -> Left err
  Right _ -> Right $ printAllRules (allRules st')
  where
    (eiErr, st') = flip runState (Names M.empty M.empty 0 M.empty)
      . runExceptT $ exc

pinchotToAst :: Pinchot a -> Either Text Text
pinchotToAst (Pinchot exc) = case eiErr of
  Left err -> Left err
  Right _ -> Right $ printAst (rules st')
  where
    (eiErr, st') = flip runState (Names M.empty M.empty 0 M.empty)
      . runExceptT $ exc

astToStdout :: Pinchot a -> IO ()
astToStdout p = case allPinchotRules p of
  Left e -> do
    IO.hPutStrLn IO.stderr ("error: bad or duplicate name: " <> unpack e)
    exitFailure
  Right g -> XIO.putStr g
