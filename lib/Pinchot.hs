{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{- |

Pinchot provides a simple language that you use to write a Haskell
program that describes a context-free grammar.  When run, this program
creates a value that stores your context-free grammar.  You can then
use Template Haskell to take this value and generate a series of data
types that correspond to your context-free grammar.  You can also use
Template Haskell to create an Earley parser that will parse all
strings in the context-free language.

For examples, please consult "Pinchot.Examples".

Pinchot grows and harvests syntax trees, so it is named after
Gifford Pinchot, first chief of the United States Forest Service.

-}
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
  , (<?>)

  -- * Transforming an AST to code
  , earleyParser
  , allRulesToCode
  , ruleTreeToCode
  ) where

import Pinchot.Intervals

import Control.Applicative ((<|>), liftA2)
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
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Language.Haskell.TH
  (ExpQ, ConQ, normalC, mkName, strictType, notStrict, DecQ, newtypeD,
   cxt, conT, Name, dataD, appT, DecsQ, appE, Q, Stmt(NoBindS), uInfixE, bindS,
   varE, varP, conE, Pat, Exp(AppE, DoE), lamE)
import qualified Language.Haskell.TH.Syntax as Syntax
import Text.Earley (satisfy, rule, symbol)
import qualified Text.Earley ((<?>))

data RuleType t
  = RTerminal (Intervals t)
  | RBranch (Branch t, [(Branch t)])
  | RSeqTerm [t] 
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
data Rule t = Rule String (Maybe String) (RuleType t)
  deriving (Eq, Ord, Show)

-- | Name a 'Rule' for use in error messages.  If you do not name a
-- rule using this combinator, the rule's type name will be used in
-- error messages.
(<?>) :: Rule t -> String -> Rule t
(Rule n _ t) <?> newName = Rule n (Just newName) t

data Branch t = Branch String [(Rule t)]
  deriving (Eq, Ord, Show)

data Names t = Names
  { tyConNames :: Set RuleName
  , dataConNames :: Set String
  , nextIndex :: Int
  , allRules :: Map Int (Rule t)
  } deriving (Eq, Ord, Show)

-- | Errors that may arise when constructing an AST.
data Error
  = InvalidName String
  -- ^ A name was invalid.  The field is the invalid name.  The name
  -- might be invalid because it was already used, or because it does
  -- not begin with a capital letter.
  | EmptyNonTerminal String
  -- ^ A non-terminal must have at least one summand.  The field is
  -- the name of the empty non-terminal.
  deriving (Show, Typeable)

instance Exception Error

-- | Constructs new 'Rule's.  @t@ is the type of the token; often this
-- will be 'Char'.
--
-- 'Pinchot' is a 'Monad' and an 'Applicative' so you can combine
-- computations using the usual methods of those classes.  Also,
-- 'Pinchot' is a 'MonadFix'.  This allows you to construct a 'Rule'
-- that depends on itself, and to construct sets of 'Rule's that have
-- mutually recursive dependencies.  'MonadFix' also allows you to use
-- the GHC @RecursiveDo@ extension.  Put
--
-- @
-- {-# LANGUAGE RecursiveDo #-}
-- @
--
-- at the top of your module, then use @mdo@ instead of @do@.  Because
-- an @mdo@ block is recursive, you can use a binding before it is
-- defined, just as you can in a set of @let@ bindings.

newtype Pinchot t a
  = Pinchot { runPinchot :: (ExceptT Error (State (Names t)) a) }
  deriving (Functor, Applicative, Monad, MonadFix)

addRuleName
  :: RuleName
  -> Pinchot t ()
addRuleName name = Pinchot $ do
  old@(Names tyNames _ _ _) <- lift get
  case name of
    [] -> throw
    x:_ -> do
      when (not (isUpper x)) throw
      when (Set.member name tyNames) throw
      lift $ put (old { tyConNames = Set.insert name tyNames })
  where
    throw = throwE $ InvalidName name

addDataConName
  :: String
  -> Pinchot t ()
addDataConName name = Pinchot $ do
  old@(Names _ dataConNames _ _) <- lift get
  case name of
    [] -> throw
    x:_ -> do
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
type RuleName = String

-- | Type synonym the the name of an alternative in a 'nonTerminal'.
-- This name must not conflict with any other data constructor, either
-- one specified as an 'AlternativeName' or one that was created using
-- 'terminal', 'option', 'list', or 'list1'.
type AlternativeName = String

-- | Creates a terminal production rule.
terminal

  :: RuleName

  -> Intervals t
  -- ^ Valid terminal symbols

  -> Pinchot t (Rule t)

terminal name ivls = newRule name (RTerminal ivls)

splitNonTerminal
  :: String
  -> [(String, [(Rule t)])]
  -> Pinchot t ((String, [(Rule t)]), [(String, [Rule t])])
splitNonTerminal n sq = Pinchot $ case sq of
  [] -> throwE $ EmptyNonTerminal n
  x : xs -> return (x, xs)

-- | Creates a production for a sequence of terminals.  Useful for
-- parsing specific words.
terminalSeq

  :: RuleName

  -> [t]
  -- ^ Sequence of terminal symbols to recognize

  -> Pinchot t (Rule t)

terminalSeq name sq = newRule name (RSeqTerm sq)

-- | Creates a new non-terminal production rule.
nonTerminal

  :: RuleName

  -> [(AlternativeName, [Rule t])]
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
  -> State (Set String) [Rule t]
getAncestors r@(Rule name _ ei) = do
  set <- get
  if Set.member name set
    then return []
    else do
      put (Set.insert name set)
      case ei of
        RTerminal _ -> return [r]
        RBranch (b1, bs) -> do
          as1 <- branchAncestors b1
          ass <- fmap join . mapM branchAncestors $ bs
          return $ r : as1 <> ass
        RSeqTerm _ -> return [r]
        ROptional c -> do
          cs <- getAncestors c
          return $ r : cs
        RMany c -> do
          cs <- getAncestors c
          return $ r : cs
        RMany1 c -> do
          cs <- getAncestors c
          return $ r : cs
        RWrap c -> do
          cs <- getAncestors c
          return $ r : cs
  where
    branchAncestors (Branch _ rs) = fmap join . mapM getAncestors $ rs

-- | Returns both this 'Rule' and any 'Rule's that are ancestors.
ruleAndAncestors
  :: Rule t
  -> [Rule t]
ruleAndAncestors r = fst $ runState (getAncestors r) Set.empty

-- | Given a sequence of 'Rule', determine which rules are on a
-- right-hand side before they are defined.
rulesDemandedBeforeDefined :: Foldable f => f (Rule t) -> Set Name
rulesDemandedBeforeDefined = snd . foldl f (Set.empty, Set.empty)
  where
    f (lhsDefined, results) (Rule nm _ ty)
      = (Set.insert nm lhsDefined, results')
      where
        results' = case ty of
          RTerminal _ -> results
          RBranch (b1, bs) -> foldr checkBranch (checkBranch b1 results) bs
            where
              checkBranch (Branch _ rls) rslts = foldr checkRule rslts rls
          RSeqTerm _ -> results
          ROptional r -> checkRule r results
          RMany r -> addHelper $ checkRule r results
          RMany1 r -> addHelper $ checkRule r results
          RWrap r -> checkRule r results
        checkRule (Rule name _ _) rslts
          | Set.member name lhsDefined = rslts
          | otherwise = Set.insert (ruleName name) rslts
        addHelper = Set.insert (helperName nm)
  

thBranch :: Branch t -> ConQ
thBranch (Branch nm rules) = normalC name fields
  where
    name = mkName nm
    mkField (Rule n _ _) = strictType notStrict (conT (mkName n))
    fields = toList . fmap mkField $ rules


thRule
  :: Name
  -- ^ Name of terminal type
  -> [Name]
  -- ^ What to derive
  -> Rule t
  -> DecQ
thRule typeName derives (Rule nm _ ruleType) = case ruleType of

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
        [strictType notStrict (appT [t| [] |]
                                    (conT typeName))]]

  ROptional (Rule inner _ _) -> newtypeD (cxt []) name [] newtypeCon derives
    where
      newtypeCon = normalC name
        [strictType notStrict (appT [t| Maybe |]
                                    (conT (mkName inner)))]

  RMany (Rule inner _ _) -> newtypeD (cxt []) name [] newtypeCon derives
    where
      newtypeCon = normalC name
        [strictType notStrict (appT [t| [] |]
                                    (conT (mkName inner)))]

  RMany1 (Rule inner _ _) -> dataD (cxt []) name [] [cons] derives
    where
      cons = normalC name
        [ strictType notStrict (conT (mkName inner))
        , strictType notStrict (appT [t| [] |]
                                     (conT (mkName inner)))]

  RWrap (Rule inner _ _) -> newtypeD (cxt []) name [] newtypeCon derives
    where
      newtypeCon = normalC name
        [ strictType notStrict (conT (mkName inner)) ]


  where
    name = mkName nm

thAllRules
  :: Name
  -- ^ Terminal type constructor name
  -> [Name]
  -- ^ What to derive
  -> Map Int (Rule t)
  -> DecsQ
thAllRules typeName derives
  = sequence
  . fmap (thRule typeName derives)
  . fmap snd
  . M.toAscList


-- | Creates code for every 'Rule' created in the 'Pinchot'.  The data
-- types are created in the same order in which they were created in
-- the 'Pinchot'.  When spliced, the 'DecsQ' is a list of
-- declarations, each of which is an appropriate @data@ or @newtype@.
-- For an example use of 'allRulesToCode', see
-- "Pinchot.Examples.PostalAstAllRules".

allRulesToCode

  :: Name
  -- ^ Terminal type constructor name.  Typically you will use the
  -- Template Haskell quoting mechanism to get this.

  -> [Name]
  -- ^ What to derive.  For instance, you might use @Eq@, @Ord@, and
  -- @Show@ here.  Each created data type will derive these instances.

  -> Pinchot t a
  -- ^ The return value from the 'Pinchot' is ignored.

  -> DecsQ
allRulesToCode typeName derives pinchot = case ei of
  Left err -> fail $ "pinchot: bad grammar: " ++ show err
  Right _ -> thAllRules typeName derives (allRules st')
  where
    (ei, st') = runState (runExceptT (runPinchot pinchot))
      (Names Set.empty Set.empty 0 M.empty)

-- | Creates code only for the 'Rule' returned from the 'Pinchot', and
-- for its ancestors.
ruleTreeToCode
  :: Name
  -- ^ Terminal type constructor name.  Typically you will use the
  -- Template Haskell quoting mechanism to get this.

  -> [Name]
  -- ^ What to derive.  For instance, you might use @Eq@, @Ord@, and
  -- @Show@ here.  Each created data type will derive these instances.

  -> Pinchot t (Rule t)
  -- ^ A data type is created for the 'Rule' that the 'Pinchot'
  -- returns, and for the ancestors of the 'Rule'.
  -> DecsQ
ruleTreeToCode typeName derives pinchot = case ei of
  Left err -> fail $ "pinchot: bad grammar: " ++ show err
  Right rule -> sequence . toList . fmap (thRule typeName derives)
    . runCalc . getAncestors $ rule
  where
    runCalc stateCalc = fst $ runState stateCalc (Set.empty)
    (ei, _) = runState (runExceptT (runPinchot pinchot))
      (Names Set.empty Set.empty 0 M.empty)



ruleToParser
  :: Syntax.Lift t
  => String
  -- ^ Module prefix
  -> Rule t
  -> Q [Stmt]
ruleToParser prefix (Rule nm mayDescription rt) = case rt of

  RTerminal ivls -> do
    topRule <- makeRule expression
    return [topRule]
    where
      expression = [| fmap $constructor (satisfy (inIntervals ivls)) |]

  RBranch (b1, bs) -> do
    topRule <- makeRule expression
    return [topRule]
    where
      expression = foldl addBranch (branchToParser prefix b1) bs
        where
          addBranch tree branch =
            [| $tree <|> $(branchToParser prefix branch) |]

  RSeqTerm sq -> do
    let nestRule = bindS (varP helper) [|rule $(go sq)|]
          where
            go sqnce = case sqnce of
              [] -> [|pure []|]
              x : xs -> [|liftA2 (:) (symbol x) $(go xs)|]
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
            parseSeq = uInfixE [|pure []|] [|(<|>)|] pSeq
              where
                pSeq = [|liftA2 (:) $(varE (ruleName innerNm)) $(varE helper) |]
    nest <- nestRule
    top <- makeRule $ wrapper helper
    return [nest, top]

  RMany1 (Rule innerNm _ _) -> do
    let nestRule = bindS (varP helper) [|rule $(parseSeq)|]
          where
            parseSeq = [| pure [] <|> $pSeq |]
              where
                pSeq = [| (:) <$> $(varE (ruleName innerNm))
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
    textToExp txt = [| $(Syntax.lift txt) |]
    constructor = constructorName prefix nm
    wrapper wrapRule = [|fmap $constructor $(varE wrapRule) |]
    helper = helperName nm


constructorName
  :: String
  -- ^ Module prefix
  -> String
  -- ^ Name of constructor
  -> ExpQ
constructorName pfx nm = conE (mkName name)
  where
    name = pfx' ++ nm
    pfx'
      | null pfx = ""
      | otherwise = pfx ++ "."

ruleName :: String -> Name
ruleName suffix = mkName ("r'" ++ suffix)

helperName :: String -> Name
helperName suffix = mkName ("h'" ++ suffix)

branchToParser
  :: Syntax.Lift t
  => String
  -- ^ Module prefix
  -> Branch t
  -> ExpQ
branchToParser prefix (Branch name rules) = case rules of
  [] -> [| pure $constructor |]
  (Rule rule1 _ _) : xs -> foldl f z xs
    where
      z = [| $constructor <$> $(varE (ruleName rule1)) |]
      f soFar (Rule rule2 _ _) = [| $soFar <*> $(varE (ruleName rule2)) |]
  where
    constructor = constructorName prefix name
    
-- | Creates a lazy pattern for all the given names.  Adds an empty
-- pattern onto the front.
lazyPattern
  :: Foldable c
  => c Name
  -> Q Pat
lazyPattern = finish . foldr gen [p| () |]
  where
    gen name rest = [p| ($(varP name), $rest) |]
    finish pat = [p| ~(_, $pat) |]

bigTuple
  :: Foldable c
  => Name
  -> c Name
  -> ExpQ
bigTuple top = finish . foldr f [| () |]
  where
    f n rest = [| ( $(varE n), $rest) |]
    finish tup = [| ($(varE top), $tup) |]

-- | Creates an Earley parser for a given 'Rule'.  For examples of how
-- to use this, see the source code for
-- "Pinchot.Examples.PostalAstRuleTree" and for
-- "Pinchot.Examples.PostalAstAllRules".

earleyParser
  :: Syntax.Lift t

  => String
  -- ^ Module prefix.  You have to make sure that the data types you
  -- created with 'ruleTreeToCode' or with 'allRulesToCode' are
  -- @import@ed into scope.  The spliced Template Haskell code has to
  -- know where to look for these data types.  If you did an
  -- unqualified @import@, just pass the empty string here.  If you
  -- did a qualified import, pass the appropriate namespace here.
  --
  -- For example, if you used @import qualified MyAst@, pass
  -- @\"MyAst\"@ here.  If you used @import qualified
  -- Data.MyLibrary.MyAst as MyLibrary.MyAst@, pass
  -- @\"MyLibrary.MyAst\"@ here.
  --
  -- For an example using an unqualified import, see
  -- "Pinchot.Examples.PostalAstRuleTree" or
  -- "Pinchot.Examples.PostalAstAllRules".
  --
  -- For an example using a qualified import, see
  -- "Pinchot.Examples.QualifiedImport".

  -> Pinchot t (Rule t)
  -- ^ Creates an Earley parser for the 'Rule' that the 'Pinchot'
  -- returns.
  -> Q Exp
earleyParser prefix pinc = case ei of
  Left err -> fail $ "pinchot: bad grammar: " ++ show err
  Right rule@(Rule top _ _) -> do
    let neededRules = ruleAndAncestors rule
        otherNames = rulesDemandedBeforeDefined neededRules
        lamb = lamE [lazyPattern otherNames] expression
        expression = do
          stmts <- fmap concat . mapM (ruleToParser prefix)
            . toList $ neededRules
          result <- bigTuple (ruleName top) otherNames
          rtn <- [|return|]
          let returner = rtn `AppE` result
          return $ DoE (stmts ++ [NoBindS returner])
    [| fmap fst (mfix $lamb) |]
  where
    (ei, _) = runState (runExceptT (runPinchot pinc))
      (Names Set.empty Set.empty 0 M.empty)

