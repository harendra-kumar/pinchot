{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Pinchot
  ( -- * Intervals
    Intervals
  , include
  , exclude
  , solo
  , pariah

  -- * Simple production rules
  , Pinchot
  , RuleName
  , Rule
  , terminal
  , terminalSeq
  , nonTerminal

  -- * Rules that modify other rules
  , list
  , list1
  , option

  -- * Rendering rules to source code text
  , allPinchotRules
  , allPinchotRulesToStdout
  , ancestors
  , ancestorsToStdout
  , Error(..)
  ) where

import Pinchot.Intervals

import Control.Exception (Exception, throwIO)
import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.State (State, runState, get, put)
import Data.Char (isUpper)
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Sequence (Seq, viewl, ViewL(EmptyL, (:<)), (<|))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Text.IO as XIO
import Data.Typeable (Typeable)

data RuleType t
  = RTerminal (Intervals t)
  | RBranch (Branch t, Seq (Branch t))
  | RSeqTerm (Seq t)
  | ROptional (Rule t)
  | RMany (Rule t)
  | RMany1 (Rule t)
  deriving (Eq, Ord, Show)

-- | A single production rule.  It may be a terminal or a non-terminal.
data Rule t = Rule Text (RuleType t)
  deriving (Eq, Ord, Show)

data Branch t = Branch Text (Seq (Rule t))
  deriving (Eq, Ord, Show)

data Names t = Names
  { rules :: Map Text (RuleType t)
  , branches :: Map Text (Seq (Rule t))
  , nextIndex :: Int
  , allRules :: Map Int (Rule t)
  } deriving (Eq, Ord, Show)

-- | Errors that may arise when constructing an AST.
data Error
  = InvalidName Text
  -- ^ A name was invalid.  The field is the invalid name.  The name
  -- might be invalid because it was already used, or because it does
  -- not begin with a capital letter.
  | EmptyNonTerminal Text
  -- ^ A non-terminal must have at least one summand.  The field is
  -- the name of the empty non-terminal.
  deriving (Show, Typeable)

instance Exception Error

-- | Constructs new 'Rule's.  @t@ is the type of the token; often this
-- will be 'Char'.
newtype Pinchot t a = Pinchot (ExceptT Error (State (Names t)) a)
  deriving (Functor, Applicative, Monad, MonadFix)

newRule
  :: Text
  -> RuleType t
  -> Pinchot t ()
newRule name ei = Pinchot $ do
  st <- lift get
  if validName name st
    then let newSt = st { rules = M.insert name ei (rules st)
                        , nextIndex = succ (nextIndex st)
                        , allRules = M.insert (nextIndex st) (Rule name ei)
                            (allRules st)
                        }
         in lift (put newSt)
    else throwE $ InvalidName name

validName
  :: Text
  -> Names t
  -> Bool
validName name (Names rls bchs _ _) = case X.uncons name of
  Nothing -> False
  Just (x, _) ->
    isUpper x && not (M.member name rls) && not (M.member name bchs)

newBranch
  :: Text
  -> Seq (Rule t)
  -> Pinchot t ()
newBranch name rs = Pinchot $ do
  st <- lift get
  if validName name st
    then let newSt = st { branches = M.insert name rs (branches st) }
         in lift (put newSt)
    else throwE $ InvalidName name

-- | Type synonym for the name of a production rule.  
-- This will be the name of the type constructor for the corresponding
-- type in the AST, so this must be a valid Haskell type constructor
-- name.  The type constructor name must not collide with any other
-- type constructor name or data contstructor name.
type RuleName = Text

-- | Creates a terminal production rule.
terminal

  :: RuleName

  -> Intervals t
  -- ^ Valid terminal symbols

  -> Pinchot t (Rule t)

terminal name ivls = do
  newRule name (RTerminal ivls)
  return $ Rule name (RTerminal ivls)

splitNonTerminal
  :: Text
  -> Seq (Text, Seq (Rule t))
  -> Pinchot t ((Text, Seq (Rule t)), Seq (Text, Seq (Rule t)))
splitNonTerminal n sq = Pinchot $ case viewl sq of
  EmptyL -> throwE $ EmptyNonTerminal n
  x :< xs -> return (x, xs)

-- | Creates a production for a sequence of terminals.  Useful for
-- parsing specific words.
terminalSeq

  :: RuleName

  -> Seq t
  -- ^ Sequence of terminal symbols to recognize

  -> Pinchot t (Rule t)

terminalSeq name sq = do
  newRule name (RSeqTerm sq)
  return $ Rule name (RSeqTerm sq)

-- | Creates a new non-terminal production rule.
nonTerminal

  :: RuleName

  -> Seq (Text, Seq (Rule t))
  -- ^ Alternatives.  There must be at least one alternative;
  -- otherwise, an error will result.  In each pair @(a, b)@, @a@ will
  -- be the data constructor, so this must be a valid Haskell data
  -- constructor name.  @b@ is the sequence of production rules, which
  -- can be empty (this is how to create an epsilon production).

  -> Pinchot t (Rule t)

nonTerminal name sq = do
  mapM (uncurry newBranch) sq
  (b1, bs) <- splitNonTerminal name sq
  let branches = RBranch (uncurry Branch b1, fmap (uncurry Branch) bs)
  newRule name branches
  return $ Rule name branches

-- | Creates a rule for the production of a sequence of other rules.
list
  :: RuleName

  -> Rule t
  -- ^ The resulting 'Rule' is a sequence of productions of this
  -- 'Rule'; that is, this 'Rule' may appear zero or more times.

  -> Pinchot t (Rule t)
list name r = do
  newRule name (RMany r)
  return $ Rule name (RMany r)

-- | Creates a rule for a production that appears at least once.
list1
  :: RuleName
  -> Rule t
  -- ^ The resulting 'Rule' produces this 'Rule' at least once.
  -> Pinchot t (Rule t)
list1 name r = do
  newRule name (RMany1 r)
  return $ Rule name (RMany1 r)

-- | Creates a rule for a production that optionally produces another
-- rule.
option
  :: RuleName
  -> Rule t
  -- ^ The resulting 'Rule' optionally produces this 'Rule'; that is,
  -- this 'Rule' may appear once or not at all.

  -> Pinchot t (Rule t)
option name r = do
  newRule name (ROptional r)
  return $ Rule name (ROptional r)

derivers :: Text
derivers = "  deriving (Eq, Ord, Show)"

printTerminal
  :: Show t
  => Text
  -- ^ Type constructor name of terminal
  -> Text
  -- ^ Terminal name
  -> Intervals t
  -> Text
printTerminal tyName name ivls
  = X.unlines [ openCom, com, closeCom, nt, derivers ]
  where
    openCom = "{- Terminal from"
    com = "   " <> X.pack (show ivls)
    closeCom = "-}"
    nt = X.unwords ["newtype", name, "=", name, tyName]

printBranch
  :: Bool
  -- ^ True if this is the first branch
  -> Branch t
  -> Text
printBranch first (Branch name rules) = X.unwords (leader : name : rest) <> "\n"
  where
    leader = "  " <> if first then "=" else "|"
    rest = toList . fmap (\(Rule x _) -> x) $ rules

printNonTerminal
  :: Text
  -> (Branch t, Seq (Branch t))
  -> Text
printNonTerminal name (b1, bs) = X.concat (line1 : linesRest)
  where
    line1 = "data " <> name <> "\n"
    linesRest = printBranch True b1 : toList (fmap (printBranch False) bs)
      <> [derivers <> "\n"]

printSeqTerm
  :: Show t
  => Text
  -- ^ Type constructor name of terminal
  -> Text
  -- ^ Type constructor name of rule
  -> Seq t
  -> Text
printSeqTerm tyName name sq
  = X.unlines [opCom, showSeq, closeCom, definition, derivers]
  where
    opCom = "{- Sequence of terminals"
    closeCom = "-}"
    showSeq = "  " <> X.pack (show sq)
    definition = X.unwords ["newtype", name, "=", name,
                            "(Seq " <> tyName <> ")" ]

printOptional
  :: Text
  -- ^ Type constructor name
  -> Rule t
  -> Text
printOptional name (Rule inner _)
  = X.unlines [definition, derivers]
  where
    definition = X.unwords ["newtype", name, "=", name,
      "(Maybe " <> inner <> ")" ]

printMany
  :: Text
  -- ^ Type constructor name
  -> Rule t
  -> Text
printMany name (Rule inner _)
  = X.unlines [definition, derivers]
  where
    definition = X.unwords ["newtype", name, "=", name,
      "(Seq " <> inner <> ")" ]

printMany1
  :: Text
  -- ^ Type constructor name
  -> Rule t
  -> Text
printMany1 name (Rule inner _)
  = X.unlines [definition, derivers]
  where
    definition = X.unwords ["data", name, "=", name,
      inner, "(Seq " <> inner <> ")" ]

printRule
  :: Show t
  => Text
  -- ^ Terminal type constructor name
  -> Rule t
  -> Text
printRule tyName (Rule name ei) = case ei of
  RTerminal term -> printTerminal tyName name term
  RBranch nonTerm -> printNonTerminal name nonTerm
  RSeqTerm sq -> printSeqTerm tyName name sq
  ROptional r -> printOptional name r
  RMany r -> printMany name r
  RMany1 r -> printMany1 name r

printAllRules
  :: Show t
  => Text
  -- ^ Terminal type constructor name
  -> Map Int (Rule t)
  -> Text
printAllRules tyName = X.concat . intersperse "\n"
  . fmap (printRule tyName)
  . fmap snd
  . M.toAscList

-- | Creates code for the AST for all 'Rule's created in the
-- 'Pinchot'.  The 'Rule's are shown in the order in which they were
-- created.
allPinchotRules
  :: Show t
  => Text
  -- ^ Terminal type constructor name
  -> Pinchot t a
  -> Either Error Text
allPinchotRules tyName (Pinchot exc) = case eiErr of
  Left err -> Left err
  Right _ -> Right $ printAllRules tyName (allRules st')
  where
    (eiErr, st') = flip runState (Names M.empty M.empty 0 M.empty)
      . runExceptT $ exc

handleError
  :: Either Error Text
  -> IO ()
handleError e = case e of
  Left e -> throwIO e
  Right g -> XIO.putStr g

-- | Like 'allPinchotRules' but prints the results to standard output.
-- Throws an 'Error' if any errors occurred.
allPinchotRulesToStdout
  :: Show t
  => Text
  -- ^ Terminal type constructor name
  -> Pinchot t a
  -> IO ()
allPinchotRulesToStdout tyName p = handleError $ allPinchotRules tyName p

-- | Gets all ancestor 'Rule's.  Skips duplicates.
getAncestors
  :: Rule t
  -> State (Set Text) (Seq (Rule t))
getAncestors r@(Rule name ei) = do
  set <- get
  if Set.member name set
    then return Seq.empty
    else do
      put (Set.insert name set)
      case ei of
        RTerminal _ -> return (Seq.singleton r)
        RBranch (b1, bs) -> do
          as1 <- branchAncestors b1
          ass <- fmap join . mapM branchAncestors $ bs
          return $ r <| as1 <> ass
        RSeqTerm _ -> return (Seq.singleton r)
        ROptional r -> do
          cs <- getAncestors r
          return $ r <| cs
        RMany r -> do
          cs <- getAncestors r
          return $ r <| cs
        RMany1 r -> do
          cs <- getAncestors r
          return $ r <| cs
  where
    branchAncestors (Branch _ rs) = fmap join . mapM getAncestors $ rs

-- | Shows only the returned 'Rule' and its ancestors.  Rules are
-- printed in the order in which they are encountered as the tree of
-- 'Rule's is traversed.
ancestors
  :: Show t
  => Text
  -- ^ Terminal type constructor name
  -> Pinchot t (Rule t)
  -> Either Error Text
ancestors tyName (Pinchot exc) = case eiErr of
  Left err -> Left err
  Right rule ->
    Right
    . X.concat
    . intersperse "\n"
    . fmap (printRule tyName)
    . toList
    . ancies
    $ rule
  where
    (eiErr, _) = flip runState (Names M.empty M.empty 0 M.empty)
      . runExceptT $ exc
    ancies rule = fst $ runState (getAncestors rule) Set.empty


-- | Like 'ancestors' but prints results to standard output.  Throws
-- an 'Error' if any errors occurred.

ancestorsToStdout
  :: Show t
  => Text
  -- ^ Terminal type constructor name
  -> Pinchot t (Rule t)
  -> IO ()
ancestorsToStdout tyName p = handleError $ ancestors tyName p
