{-# LANGUAGE OverloadedLists #-}
module Pinchot.Intervals' where

import Control.Monad (join)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Sequence (Seq, ViewL(EmptyL, (:<)), viewl, (<|))
import qualified Data.Sequence as Seq

data Intervals a = Intervals
  { included :: Seq (a, a)
  , excluded :: Seq (a, a)
  } deriving (Eq, Ord, Show)

instance Functor Intervals where
  fmap f (Intervals a b) = Intervals (fmap g a) (fmap g b)
    where
      g (x, y) = (f x, f y)

instance Monoid (Intervals a) where
  mempty = Intervals mempty mempty
  (Intervals x1 y1) `mappend` (Intervals x2 y2)
    = Intervals (x1 <> x2) (y1 <> y2)

include :: a -> a -> Intervals a
include l h = Intervals [(l, h)] []

exclude :: a -> a -> Intervals a
exclude l h = Intervals [] [(l, h)]

solo :: a -> Intervals a
solo x = Intervals [(x, x)] []

pariah :: a -> Intervals a
pariah x = Intervals [] [(x, x)]

endLeft :: Ord a => (a, a) -> a
endLeft (a, b) = min a b

endRight :: Ord a => (a, a) -> a
endRight (a, b) = max a b

inInterval :: Ord a => a -> (a, a) -> Bool
inInterval x i = x >= endLeft i && x <= endRight i

members :: (Ord a, Enum a) => (a, a) -> Seq a
members i = Seq.fromList [endLeft i .. endRight i]

sortIntervalSeq :: Ord a => Seq (a, a) -> Seq (a, a)
sortIntervalSeq = Seq.sortBy (comparing endLeft <> comparing endRight)

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

standardizeIntervalSeq :: (Ord a, Enum a) => Seq (a, a) -> Seq (a, a)
standardizeIntervalSeq = flattenIntervalSeq . sortIntervalSeq

-- | Presents the intervals in a standard order, as described in
-- 'standardize'.  If the input has already been sorted with
-- 'sortIntervals', the same properties for 'standardize' hold for
-- this function.  Otherwise, its properties are undefined.
flattenIntervalSeq :: (Ord a, Enum a) => Seq (a, a) -> Seq (a, a)
flattenIntervalSeq = go Nothing
  where
    go mayCurr sq = case (mayCurr, viewl sq) of
      (Nothing, EmptyL) -> []
      (Just i, EmptyL) -> [i]
      (Nothing, x :< xs) -> go (Just x) xs
      (Just curr, x :< xs)
        | endRight curr < endLeft x
            && endRight curr < pred (endLeft x) -> curr <| go (Just x) xs
        | otherwise -> go (Just (endLeft curr,
            max (endRight curr) (endRight x))) xs


{- |
Removes excluded members from a list of 'Interval'.  The
following properties hold:

@

removeProperties
  :: (Ord a, Enum a)
  => Seq (a, a)
  -> Seq (a, a)
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
  => Seq (a, a)
  -- ^ Included intervals (not necessarily sorted)
  -> Seq (a, a)
  -- ^ Excluded intervals (not necessarily sorted)
  -> Seq (a, a)
removeExcludes inc = foldr remover inc

remover
  :: (Ord a, Enum a)
  => (a, a)
  -- ^ Remove this interval
  -> Seq (a, a)
  -- ^ From this sequence of intervals
  -> Seq (a, a)
remover ivl = join . fmap (removeInterval ivl)

-- | Removes a single interval from a single other interval.  Returns
-- a sequence of intervals, which always
removeInterval
  :: (Ord a, Enum a)
  => (a, a)
  -- ^ Remove this interval
  -> (a, a)
  -- ^ From this interval
  -> Seq (a, a)
removeInterval ivl oldIvl = onLeft <> onRight
  where
    onLeft
      | endLeft ivl > endLeft oldIvl =
          Seq.singleton ( endLeft oldIvl
            , min (pred (endLeft ivl)) (endRight oldIvl))
      | otherwise = Seq.empty
    onRight
      | endRight ivl < endRight oldIvl =
          Seq.singleton ( max (succ (endRight ivl)) (endLeft oldIvl)
                        , endRight oldIvl)
      | otherwise = Seq.empty
