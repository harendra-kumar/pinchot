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

  -- * Rendering rules to source code text
  , allPinchotRules
  , allPinchotRulesToStdout
  , ancestors
  , ancestorsToStdout
  , earleyParser
  , makeAst
  , Error(..)
  ) where

import Pinchot.Intervals

import Control.Exception (Exception, throwIO)
import Control.Monad (join, when)
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
import Data.Text (Text, unpack)
import qualified Data.Text as X
import qualified Data.Text.IO as XIO
import Data.Typeable (Typeable)

import Language.Haskell.TH

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

derivers :: Text
derivers = "  deriving (Eq, Ord, Show, Typeable)"

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

printWrap
  :: Text
  -- ^ Type constructor name
  -> Rule t
  -> Text
printWrap name (Rule inner _)
  = X.unlines [definition, derivers]
  where
    definition = X.unwords ["newtype", name, "=", name, inner]

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
  RWrap r -> printWrap name r

astHeader
  :: Text
  -- ^ Module name
  -> Maybe (Text, Text)
  -- ^ Name of module to import the terminal type from, and the name
  -- of the terminal type itself.  Use 'Nothing' if your terminal type
  -- is in the Prelude.
  -> Text
astHeader name mayImportType = X.unlines $
  [ "{-# LANGUAGE DeriveDataTypeable #-}"
  , ""
  , X.unwords ["module", name, "where"]
  , ""
  , "import Data.Sequence (Seq)"
  , "import Data.Typeable (Typeable)"
  ] ++ case mayImportType of
          Nothing -> []
          Just (mod, tyName) ->
            [X.unwords ["import", mod, "(" <> tyName <> ")"]]

printAllRules
  :: Show t
  => Text
  -- ^ Module name
  -> Text
  -- ^ Terminal type constructor name
  -> Maybe Text
  -- ^ Where to import the terminal type from
  -> Map Int (Rule t)
  -> Text
printAllRules modName tyName mayTermMod
  = X.concat
  . intersperse "\n"
  . (astHeader modName impTerminal :)
  . fmap (printRule tyName)
  . fmap snd
  . M.toAscList
  where
    impTerminal = case mayTermMod of
      Nothing -> Nothing
      Just termMod -> Just (termMod, tyName)

-- | Creates code for the AST for all 'Rule's created in the
-- 'Pinchot'.  The 'Rule's are shown in the order in which they were
-- created.
allPinchotRules
  :: Show t
  => Text
  -- ^ Module name
  -> Text
  -- ^ Terminal type constructor name
  -> Maybe Text
  -- ^ Where to import the terminal type from
  -> Pinchot t a
  -> Either Error Text
allPinchotRules modName tyName mayTermMod (Pinchot exc) = case eiErr of
  Left err -> Left err
  Right _ -> Right $ printAllRules modName tyName mayTermMod (allRules st')
  where
    (eiErr, st') = flip runState (Names Set.empty Set.empty 0 M.empty)
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
  -- ^ Module name
  -> Text
  -- ^ Terminal type constructor name
  -> Maybe Text
  -- ^ Where to import the terminal type from
  -> Pinchot t a
  -> IO ()
allPinchotRulesToStdout modName tyName mayTermMod p = handleError
  $ allPinchotRules modName tyName mayTermMod p

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
        RWrap r -> do
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
  -- ^ Module name
  -> Text
  -- ^ Terminal type constructor name
  -> Maybe Text
  -- ^ Where to import the terminal type from
  -> Pinchot t (Rule t)
  -> Either Error Text
ancestors modName tyName mayTermMod (Pinchot exc) = case eiErr of
  Left err -> Left err
  Right rule ->
    Right
    . X.concat
    . intersperse "\n"
    . (astHeader modName impTerminal :)
    . fmap (printRule tyName)
    . toList
    . ancies
    $ rule
  where
    (eiErr, _) = flip runState (Names Set.empty Set.empty 0 M.empty)
      . runExceptT $ exc
    ancies rule = fst $ runState (getAncestors rule) Set.empty
    impTerminal = case mayTermMod of
      Nothing -> Nothing
      Just termMod -> Just (termMod, tyName)


-- | Like 'ancestors' but prints results to standard output.  Throws
-- an 'Error' if any errors occurred.

ancestorsToStdout
  :: Show t
  => Text
  -- ^ Module name
  -> Text
  -- ^ Terminal type constructor name
  -> Maybe Text
  -- ^ Where to import the terminal type from
  -> Pinchot t (Rule t)
  -> IO ()
ancestorsToStdout modName tyName mayTermMod p
  = handleError $ ancestors modName tyName mayTermMod p

-- | Generates a parser for use with the @Earley@ package.
earleyParser
  :: Text
  -- ^ Module name for the resulting parser
  -> Text
  -- ^ Terminal type constructor name.  Often this will be 'Char'.
  -> Maybe Text
  -- ^ Where to import the terminal type from.  Use 'Nothing' if your
  -- terminal type is already in the Prelude.
  -> Text
  -- ^ Where to import the AST from
  -> Pinchot t (Rule t)
  -> Either Error Text
earleyParser = undefined

--
-- # Template Haskell - experimental
--

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
        [strictType notStrict (appT (conT (mkName "Seq"))
                                    (conT typeName))]]

  ROptional (Rule inner _) -> newtypeD (cxt []) name [] newtypeCon derives
    where
      newtypeCon = normalC name
        [strictType notStrict (appT (conT (mkName "Maybe"))
                                    (conT (mkName (unpack inner))))]

  RMany (Rule inner _) -> newtypeD (cxt []) name [] newtypeCon derives
    where
      newtypeCon = normalC name
        [strictType notStrict (appT (conT (mkName "Seq"))
                                    (conT (mkName (unpack inner))))]

  RMany1 (Rule inner _) -> dataD (cxt []) name [] [cons] derives
    where
      cons = normalC name
        [ strictType notStrict (conT (mkName (unpack inner)))
        , strictType notStrict (appT (conT (mkName "Seq"))
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

makeAst
  :: Name
  -- ^ Terminal type constructor name
  -> Pinchot t a
  -> DecsQ
makeAst typeName pinchot = case ei of
  Left err -> fail $ "pinchot: bad grammar: " ++ show err
  Right _ -> thAllRules typeName (allRules st')
  where
    (ei, st') = runState (runExceptT (runPinchot pinchot))
      (Names Set.empty Set.empty 0 M.empty)
