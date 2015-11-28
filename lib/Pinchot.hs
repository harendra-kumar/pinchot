{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
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
  , AlternativeName
  , Rule
  , terminal
  , terminalSeq
  , nonTerminal

  -- * Rules that modify other rules
  , list
  , list1
  , option
  , wrap

  -- * Sequence type constructor
  , Seq

  -- * Transforming an AST to code
  , earleyParser
  , allRulesToCode
  , ruleTreeToCode
  , Error(..)
  ) where

import Pinchot.Intervals

import Control.Exception (Exception)
import Control.Monad (join, when)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.State (State, runState, get, put)
import Data.Char (isUpper)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Sequence (Seq, viewl, ViewL(EmptyL, (:<)), (<|))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import qualified Data.Text as X
import Data.Typeable (Typeable)
import Language.Haskell.TH
  (ExpQ, ConQ, normalC, mkName, strictType, notStrict, DecQ, newtypeD,
   cxt, conT, Name, dataD, appT, DecsQ)

data RuleType t
  = RTerminal (Intervals t)
  | RBranch (Branch t, Seq (Branch t))
  | RSeqTerm (Seq t)
  | ROptional (Rule t)
  | RMany (Rule t)
  | RMany1 (Rule t)
  | RWrap (Rule t)
  deriving (Eq, Ord, Show)

-- | A single production rule.  It may be a terminal or a non-terminal.
data Rule t = Rule Text (RuleType t)
  deriving (Eq, Ord, Show)

data Branch t = Branch Text (Seq (Rule t))
  deriving (Eq, Ord, Show)

data Names t = Names
  { tyConNames :: Set RuleName
  , dataConNames :: Set Text
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
newtype Pinchot t a
  = Pinchot { runPinchot :: (ExceptT Error (State (Names t)) a) }
  deriving (Functor, Applicative, Monad, MonadFix)

addRuleName
  :: RuleName
  -> Pinchot t ()
addRuleName name = Pinchot $ do
  old@(Names tyNames _ _ _) <- lift get
  case X.uncons name of
    Nothing -> throw
    Just (x, _) -> do
      when (not (isUpper x)) throw
      when (Set.member name tyNames) throw
      lift $ put (old { tyConNames = Set.insert name tyNames })
  where
    throw = throwE $ InvalidName name

addDataConName
  :: Text
  -> Pinchot t ()
addDataConName name = Pinchot $ do
  old@(Names _ dataConNames _ _) <- lift get
  case X.uncons name of
    Nothing -> throw
    Just (x, _) -> do
      when (not (isUpper x)) throw
      when (Set.member name dataConNames) throw
      lift $ put (old { dataConNames = Set.insert name dataConNames })
  where
    throw = throwE $ InvalidName name

newRule
  :: RuleName
  -> RuleType t
  -> Pinchot t ()
newRule name ei = Pinchot $ do
  runPinchot (addRuleName name)
  st <- lift get
  let newSt = st { nextIndex = succ (nextIndex st)
                 , allRules = M.insert (nextIndex st) (Rule name ei)
                            (allRules st)
                 }
  lift (put newSt)

-- | Type synonym for the name of a production rule.  
-- This will be the name of the type constructor for the corresponding
-- type in the AST, so this must be a valid Haskell type constructor
-- name.
--
-- If you are creating a 'terminal', 'option', 'list', 'list1', or
-- 'wrap', the 'RuleName' will also be used for the name of the single
-- data construtor.  If you are creating a 'nonTerminal', you will
-- specify the name of each data constructor with 'AlternativeName'.
type RuleName = Text

-- | Type synonym the the name of an alternative in a 'nonTerminal'.
-- This name must not conflict with any other data constructor, either
-- one specified as an 'AlternativeName' or one that was created using
-- 'terminal', 'option', 'list', or 'list1'.
type AlternativeName = Text

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
  mapM addDataConName . fmap fst $ sq
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

-- | Creates a newtype wrapper.

wrap
  :: RuleName
  -> Rule t
  -- ^ The resulting 'Rule' simply wraps this 'Rule'.
  -> Pinchot t (Rule t)
wrap name r = do
  newRule name (RWrap r)
  return $ Rule name (RWrap r)

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
        ROptional c -> do
          cs <- getAncestors c
          return $ r <| cs
        RMany c -> do
          cs <- getAncestors c
          return $ r <| cs
        RMany1 c -> do
          cs <- getAncestors c
          return $ r <| cs
        RWrap c -> do
          cs <- getAncestors c
          return $ r <| cs
  where
    branchAncestors (Branch _ rs) = fmap join . mapM getAncestors $ rs


-- | Generates a parser for use with the @Earley@ package.
earleyParser
  :: Pinchot t (Rule t)
  -> ExpQ
earleyParser = undefined

thBranch :: Branch t -> ConQ
thBranch (Branch nm rules) = normalC name fields
  where
    name = mkName (unpack nm)
    mkField (Rule n _) = strictType notStrict (conT (mkName (unpack n)))
    fields = toList . fmap mkField $ rules


thRule :: Name -> Rule t -> DecQ
thRule typeName (Rule nm ruleType) = case ruleType of

  RTerminal _ -> newtypeD (cxt []) name [] newtypeCon derives
    where
      newtypeCon = normalC name
        [strictType notStrict (conT typeName)]

  RBranch (b1, bs) -> dataD (cxt []) name [] cons derives
    where
      cons = thBranch b1 : toList (fmap thBranch bs)

  RSeqTerm _ -> dataD (cxt []) name [] cons derives
    where
      cons = [normalC name
        [strictType notStrict (appT [t| Seq |]
                                    (conT typeName))]]

  ROptional (Rule inner _) -> newtypeD (cxt []) name [] newtypeCon derives
    where
      newtypeCon = normalC name
        [strictType notStrict (appT [t| Maybe |]
                                    (conT (mkName (unpack inner))))]

  RMany (Rule inner _) -> newtypeD (cxt []) name [] newtypeCon derives
    where
      newtypeCon = normalC name
        [strictType notStrict (appT [t| Seq |]
                                    (conT (mkName (unpack inner))))]

  RMany1 (Rule inner _) -> dataD (cxt []) name [] [cons] derives
    where
      cons = normalC name
        [ strictType notStrict (conT (mkName (unpack inner)))
        , strictType notStrict (appT [t| Seq |]
                                     (conT (mkName (unpack inner))))]

  RWrap (Rule inner _) -> newtypeD (cxt []) name [] newtypeCon derives
    where
      newtypeCon = normalC name
        [ strictType notStrict (conT (mkName (unpack inner))) ]


  where
    name = mkName (unpack nm)
    derives = map mkName ["Eq", "Ord", "Show"]

thAllRules
  :: Name
  -- ^ Terminal type constructor name
  -> Map Int (Rule t)
  -> DecsQ
thAllRules typeName
  = sequence
  . fmap (thRule typeName)
  . fmap snd
  . M.toAscList


-- | Creates code for every 'Rule' created in the 'Pinchot'.  The data
-- types are created in the same order in which they were created in
-- the 'Pinchot'.
allRulesToCode
  :: Name
  -- ^ Terminal type constructor name
  -> Pinchot t a
  -> DecsQ
allRulesToCode typeName pinchot = case ei of
  Left err -> fail $ "pinchot: bad grammar: " ++ show err
  Right _ -> thAllRules typeName (allRules st')
  where
    (ei, st') = runState (runExceptT (runPinchot pinchot))
      (Names Set.empty Set.empty 0 M.empty)

-- | Creates code only for the 'Rule' returned from the 'Pinchot', and
-- for its ancestors.
ruleTreeToCode
  :: Name
  -- ^ Terminal type constructor name
  -> Pinchot t (Rule t)
  -> DecsQ
ruleTreeToCode typeName pinchot = case ei of
  Left err -> fail $ "pinchot: bad grammar: " ++ show err
  Right rule -> sequence . toList . fmap (thRule typeName)
    . runCalc . getAncestors $ rule
  where
    runCalc stateCalc = fst $ runState stateCalc (Set.empty)
    (ei, _) = runState (runExceptT (runPinchot pinchot))
      (Names Set.empty Set.empty 0 M.empty)
