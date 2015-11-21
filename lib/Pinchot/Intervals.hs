-- | Helpers for dealing with intervals.
module Pinchot.Intervals
  ( -- * Single interval
    Interval(..)
  , singleton
  , range
  , endLeft
  , endRight
  , members
  , uniqueMembers
  , inInterval
  , sortIntervals
  , standardize
  , flatten
  , removeExcludes
  , intervalToTuple

  -- * Collection of intervals
  , Intervals(..)
  , emptyIntervals
  , included
  , alone
  , addIncluded
  , addExcluded
  , remove
  , splitIntervals
  , inIntervals
  , intervalsToTuples
  ) where

import Data.Ord
import Data.Monoid
import Data.List (sortBy, nub, sort)
import Pinchot.Intervals.Interval

-- | A collection of 'Interval'.
data Intervals a = Intervals
  { includedIntervals :: [Interval a]
  -- ^ Every member of each of these 'Interval' is a member of the
  -- 'Intervals', except for the 'excludedIntervals'.
  , excludedIntervals :: [Interval a]
  -- ^ Every member of these 'Interval' is not a member of the
  -- 'Intervals', even if they are members of 'includedIntervals'.
  } deriving (Eq, Ord, Show)

-- | Initialize an 'Intervals' with an empty 'excludedIntervals' list
-- and a list of these 'includedIntervals'.
included :: [Interval a] -> Intervals a
included ls = Intervals ls []

-- | Initialize an 'Intervals' with a single member and no excludes.
alone :: a -> Intervals a
alone a = Intervals [singleton a] []

instance Monoid (Intervals a) where
  mempty = Intervals [] []
  mappend (Intervals x1 y1) (Intervals x2 y2)
    = Intervals (x1 <> x2) (y1 <> y2)

instance Functor Intervals where
  fmap f (Intervals i e) = Intervals (fmap (fmap f) i)
                                     (fmap (fmap f) e)

-- | An 'Intervals' with no members.
emptyIntervals :: Intervals a
emptyIntervals = Intervals [] []

-- | Adds an 'Interval' to the 'includedIntervals'.
addIncluded :: Interval a -> Intervals a -> Intervals a
addIncluded i (Intervals is es) = Intervals (i:is) es

-- | Adds an 'Interval' to the 'excludedIntervals'.
addExcluded :: Interval a -> Intervals a -> Intervals a
addExcluded i (Intervals is es) = Intervals is (i:es)

-- | @remove a b@ produces a new 'Intervals' which includes the
-- include list of @a@, with any members of @b@ added to the exclude
-- list.  Use this infix.
remove :: (Enum a, Ord a) => Intervals a -> Intervals a -> Intervals a
remove (Intervals inc exc) toRemove = Intervals inc (exc ++ newRemoves)
  where
    newRemoves = splitIntervals toRemove

-- | True if this point a member of the 'Interval'.
inInterval :: Ord a => a -> Interval a -> Bool
inInterval x i = (x >= endLeft i) && (x <= endRight i)

-- | All members of an 'Interval'.
members :: (Ord a, Enum a) => Interval a -> [a]
members i = [endLeft i .. endRight i]

-- | All members of a list of 'Interval'; sorted with duplicates
-- removed.
uniqueMembers :: (Ord a, Enum a) => [Interval a] -> [a]
uniqueMembers = nub . sort . concatMap members

-- | Sorts intervals, first by the left endpoint and then by the right
-- endpoint if the left endpoint is equal.
sortIntervals :: Ord a => [Interval a] -> [Interval a]
sortIntervals = sortBy (comparing endLeft <> comparing endRight)

-- | Sorts the intervals using 'sortIntervals' and presents them in a
-- regular order using 'flatten'.  The function @standardize a@ has
-- the following properties, where @b@ is the result:
--
-- @
-- 'uniqueMembers' a == 'uniqueMembers' b
--
-- let go [] = True
--     go (_:[]) = True
--     go (x:y:xs)
--          | 'endRight' x < 'endLeft' y
--              && 'endRight' x < pred ('endLeft' x)
--              = go (y:xs)
--          | otherwise = False
-- in go b
-- @
--
-- The second property means that adjacent intervals in the list must
-- be separated by at least one point on the number line.

standardize :: (Ord a, Enum a) => [Interval a] -> [Interval a]
standardize = flatten . sortIntervals

-- | Presents the intervals in a standard order, as described in
-- 'standardize'.  If the input has already been sorted with
-- 'sortIntervals', the same properties for 'standardize' hold for
-- this function.  Otherwise, its properties are undefined.
flatten :: (Ord a, Enum a) => [Interval a] -> [Interval a]
flatten = go Nothing
  where
    go Nothing [] = []
    go (Just i) [] = [i]
    go Nothing (x:xs) = go (Just x) xs
    go (Just curr) (x:xs)
      | endRight curr < endLeft x
          && endRight curr < pred (endLeft x) = curr : go (Just x) xs
      | otherwise = go (Just (range (endLeft curr)
          (max (endRight curr) (endRight x)))) xs

{- |
Removes excluded members from a list of 'Interval'.  The
following properties hold:

@

removeProperties
  :: (Ord a, Enum a)
  => [Interval a]
  -> [Interval a]
  -> [Bool]
removeProperties inc exc =

 let r = removeExcludes inc exc
     allExcluded = concatMap members exc
     allIncluded = concatMap members inc
     allResults = concatMap members r
 in [
   -- intervals remain in original order
   allResults == filter (not . (\`elem\` allExcluded)) allIncluded

 -- Every resulting member was a member of the original include list
 , all (\`elem\` allIncluded) allResults

 -- No resulting member is in the exclude list
 , all (not . (\`elem\` allExcluded)) allResults

 -- Every included member that is not in the exclude list is
 -- in the result
 , all (\x -> x \`elem\` allExcluded || x \`elem\` allResults)
       allIncluded

 ]
@

-}
removeExcludes
  :: (Ord a, Enum a)
  => [Interval a]
  -- ^ Included intervals (not necessarily sorted)
  -> [Interval a]
  -- ^ Excluded intervals (not necessarily sorted)
  -> [Interval a]
removeExcludes inc = foldr remover inc
  where
    remover ivl ivls = concatMap rmve ivls
      where
        rmve oldIvl = concat [onLeft, onRight]
          where
            onLeft
              | endLeft ivl > endLeft oldIvl =
                  [range (endLeft oldIvl)
                            (min (pred (endLeft ivl)) (endRight oldIvl))]
              | otherwise = []
            onRight
              | endRight ivl < endRight oldIvl =
                  [range (max (succ (endRight ivl)) (endLeft oldIvl))
                            (endRight oldIvl)]
              | otherwise = []

-- | Sorts the intervals using 'standardize', and then removes the
-- excludes with 'removeExcludes'.
splitIntervals
  :: (Ord a, Enum a)
  => Intervals a
  -> [Interval a]
splitIntervals (Intervals is es) = removeExcludes (standardize is) es

-- | 'True' if the given element is a member of the 'Intervals'.
inIntervals :: (Enum a, Ord a) => a -> Intervals a -> Bool
inIntervals a = any (inInterval a) . splitIntervals

{- |

Converts an interval to a tuple, with the bottom of the range being
the first element and the top being the second element.  That is, the
following property holds:

@
\x -> let (a, b) = intervalToTuple x in a <= b
@
-}
intervalToTuple :: Ord a => Interval a -> (a, a)
intervalToTuple i = (endLeft i, endRight i)

-- | Same as
--
-- @
-- map 'intervalToTuple' . 'splitIntervals'
-- @
intervalsToTuples :: (Ord a, Enum a) => Intervals a -> [(a, a)]
intervalsToTuples = map intervalToTuple . splitIntervals
