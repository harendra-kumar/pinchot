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
  , ruleParser
  , Error(..)
  ) where

import Pinchot.Intervals

import Control.Applicative ((<|>), empty, liftA2)
import Control.Exception (Exception)
import Control.Monad (join, when)
import Control.Monad.Fix (MonadFix, mfix)
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
   cxt, conT, Name, dataD, appT, DecsQ, appE, Q, Stmt(NoBindS), uInfixE, bindS,
   varE, varP, conE, Pat, tupE, Exp(AppE, DoE), lamE)
import qualified Language.Haskell.TH.Syntax as Syntax
import Text.Earley (satisfy, rule, symbol)
import qualified Text.Earley ((<?>))

data RuleType t
  = RTerminal (Intervals t)
  | RBranch (Branch t, Seq (Branch t))
  | RSeqTerm (Seq t)
  | ROptional (Rule t)
  | RMany (Rule t)
  | RMany1 (Rule t)
  | RWrap (Rule t)
  deriving (Eq, Ord, Show)

-- Rule n d t, where
--
-- n is the name of the rule.  This is used as the name of the
-- corresponding data type.
--
-- d is the description of the rule.  This is optional and is used for
-- the parser's error messages.  If there is no description, the name
-- is used for error messages.
--
-- t is the type of rule (terminal, branch, etc.)

-- | A single production rule.  It may be a terminal or a non-terminal.
data Rule t = Rule Text (Maybe Text) (RuleType t)
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
  -> Pinchot t (Rule t)
newRule name ty = Pinchot $ do
  runPinchot (addRuleName name)
  st <- lift get
  let r = Rule name Nothing ty
      newSt = st { nextIndex = succ (nextIndex st)
                 , allRules = M.insert (nextIndex st) r
                            (allRules st)
                 }
  lift (put newSt)
  return r

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

terminal name ivls = newRule name (RTerminal ivls)

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

terminalSeq name sq = newRule name (RSeqTerm sq)

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

-- | Creates a rule for the production of a sequence of other rules.
list
  :: RuleName

  -> Rule t
  -- ^ The resulting 'Rule' is a sequence of productions of this
  -- 'Rule'; that is, this 'Rule' may appear zero or more times.

  -> Pinchot t (Rule t)
list name r = newRule name (RMany r)

-- | Creates a rule for a production that appears at least once.
list1
  :: RuleName
  -> Rule t
  -- ^ The resulting 'Rule' produces this 'Rule' at least once.
  -> Pinchot t (Rule t)
list1 name r = newRule name (RMany1 r)

-- | Creates a rule for a production that optionally produces another
-- rule.
option
  :: RuleName
  -> Rule t
  -- ^ The resulting 'Rule' optionally produces this 'Rule'; that is,
  -- this 'Rule' may appear once or not at all.

  -> Pinchot t (Rule t)
option name r = newRule name (ROptional r)

-- | Creates a newtype wrapper.

wrap
  :: RuleName
  -> Rule t
  -- ^ The resulting 'Rule' simply wraps this 'Rule'.
  -> Pinchot t (Rule t)
wrap name r = newRule name (RWrap r)

-- | Gets all ancestor 'Rule's.  Skips duplicates.
getAncestors
  :: Rule t
  -> State (Set Text) (Seq (Rule t))
getAncestors r@(Rule name _ ei) = do
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
    mkField (Rule n _ _) = strictType notStrict (conT (mkName (unpack n)))
    fields = toList . fmap mkField $ rules


thRule :: Name -> Rule t -> DecQ
thRule typeName (Rule nm _ ruleType) = case ruleType of

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

  ROptional (Rule inner _ _) -> newtypeD (cxt []) name [] newtypeCon derives
    where
      newtypeCon = normalC name
        [strictType notStrict (appT [t| Maybe |]
                                    (conT (mkName (unpack inner))))]

  RMany (Rule inner _ _) -> newtypeD (cxt []) name [] newtypeCon derives
    where
      newtypeCon = normalC name
        [strictType notStrict (appT [t| Seq |]
                                    (conT (mkName (unpack inner))))]

  RMany1 (Rule inner _ _) -> dataD (cxt []) name [] [cons] derives
    where
      cons = normalC name
        [ strictType notStrict (conT (mkName (unpack inner)))
        , strictType notStrict (appT [t| Seq |]
                                     (conT (mkName (unpack inner))))]

  RWrap (Rule inner _ _) -> newtypeD (cxt []) name [] newtypeCon derives
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



ruleToParser
  :: Syntax.Lift t
  => Rule t
  -> Q [Stmt]
ruleToParser (Rule nm mayDescription rt) = case rt of

  RTerminal ivls -> do
    topRule <- makeRule expression
    return [topRule]
    where
      expression = [| fmap $constructor (satisfy (inIntervals ivls)) |]

  RBranch (b1, bs) -> do
    topRule <- makeRule expression
    return [topRule]
    where
      expression = uInfixE (branchToParser b1)
        [|(<|>)|] (go bs)
        where
          go sq = case viewl sq of
            EmptyL -> [| empty |]
            x :< xs -> uInfixE (branchToParser x) [| (<|>) |]
              (go xs)

  RSeqTerm sq -> do
    let nestRule = bindS (varP helper) [|rule $(go sq)|]
          where
            go sqnce = case viewl sqnce of
              EmptyL -> [|pure Seq.empty|]
              x :< xs -> [|liftA2 (<|) (symbol x) $(go xs)|]
    nest <- nestRule
    topRule <- makeRule (wrapper helper)
    return [nest, topRule]

  ROptional (Rule innerNm _ _) -> fmap (:[]) (makeRule expression)
    where
      expression = [| fmap $constructor (pure Nothing <|> $(just)) |]
        where
          just = [| fmap Just $(varE (ruleName innerNm)) |]

  RMany (Rule innerNm _ _) -> do
    let nestRule = bindS (varP helper) ([|rule|] `appE` parseSeq)
          where
            parseSeq = uInfixE [|pure Seq.empty|] [|(<|>)|] pSeq
              where
                pSeq = [|liftA2|] `appE` [|(<|)|] `appE` (varE (ruleName innerNm))
                  `appE` varE helper
    nest <- nestRule
    top <- makeRule $ wrapper helper
    return [nest, top]

  RMany1 (Rule innerNm _ _) -> do
    let nestRule = bindS (varP helper) [|rule $(parseSeq)|]
          where
            parseSeq = [| pure Seq.empty <|> $pSeq |]
              where
                pSeq = [| (<|) <$> $(varE (ruleName innerNm))
                               <*> $(varE helper) |]
    nest <- nestRule
    let topExpn = [| $constructor <$> $(varE (ruleName innerNm))
                                  <*> $(varE helper) |]
    top <- makeRule topExpn
    return [nest, top]

  RWrap (Rule innerNm _ _) -> fmap (:[]) (makeRule expression)
    where
      expression = [|fmap $constructor $(varE (ruleName innerNm)) |]
    

  where
    makeRule expression = varP (ruleName nm) `bindS`
      [|rule ($expression Text.Earley.<?> $(textToExp desc))|]
    desc = maybe nm id mayDescription
    textToExp txt = [| X.pack $(Syntax.lift (unpack txt)) |]
    constructor = conE (mkName (unpack nm))
    wrapper wrapRule = [|fmap $constructor $(varE wrapRule) |]
    helper = helperName nm


ruleName :: Text -> Name
ruleName suffix = mkName ("r'" ++ unpack suffix)

helperName :: Text -> Name
helperName suffix = mkName ("h'" ++ unpack suffix)

branchToParser
  :: Syntax.Lift t
  => Branch t
  -> ExpQ
branchToParser (Branch name rules) = case viewl rules of
  EmptyL -> [| pure $constructor |]
  (Rule rule1 _ _) :< xs -> foldl f z xs
    where
      z = [| $constructor <$> $(varE (ruleName rule1)) |]
      f soFar (Rule rule2 _ _) = [| $soFar <*> $(varE (ruleName rule2)) |]
  where
    constructor = conE (mkName (unpack name))
    
-- | Creates a lazy pattern for all the given names.
lazyPattern
  :: [Name]
  -> Q Pat
lazyPattern ns = [p| ~(_, $(go ns)) |]
  where
    go es = case es of
      [] -> [p| () |]
      x:xs -> [p| ($(varP x), $(go xs)) |]

-- | Creates a tuple for all the given names.
bigTuple
  :: Name
  -> [Name]
  -> Q Exp
bigTuple n ns = go n ns
  where
    go e1 es = case es of
      [] -> tupE [varE e1, [e|()|]]
      x:xs -> tupE [varE e1, go x xs]

-- | Given a root Rule, returns all the Names required when creating a parser.
allRequiredNames
  :: Rule t
  -> Set Name
allRequiredNames = go Set.empty
  where
    go set (Rule n _ ty)
      = more (Set.insert (ruleName n) set)
      where
        more set = case ty of
          RTerminal _ -> set
          RBranch (Branch _ s1, bs) -> foldl goBranch (foldl go set s1) bs
            where
              goBranch set (Branch _ sqRule) = foldl go set sqRule
          RSeqTerm _ -> helper set
          ROptional r -> go set r
          RMany r -> helper (go set r)
          RMany1 r -> helper (go set r)
          RWrap r -> go set r
          where
            helper = Set.insert (mkName ("h'" ++ unpack n))

ruleParser
  :: Syntax.Lift t
  => Pinchot t (Rule t)
  -> Q Exp
ruleParser pinc = case ei of
  Left err -> fail $ "pinchot: bad grammar: " ++ show err
  Right rule@(Rule top _ _) -> do
    let lamb = lamE [lazyPattern otherNames] expression
        otherNames = toList . allRequiredNames $ rule
        expression = do
          let allRules = fst $ runState (getAncestors rule) Set.empty
          stmts <- fmap concat . mapM ruleToParser . toList $ allRules
          result <- bigTuple (ruleName top) otherNames
          rtn <- [|return|]
          let returner = rtn `AppE` result
          return $ DoE (stmts ++ [NoBindS returner])
    [| fmap fst (mfix $lamb) |]
  where
    (ei, _) = runState (runExceptT (runPinchot pinc))
      (Names Set.empty Set.empty 0 M.empty)
