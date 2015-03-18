{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Indexed.Map.Strict
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
--                (c) SkedgeMe LLC 2015
-- License     :  BSD-style
-- Maintainer  :  daniel.haraj@skedge.me
-- Stability   :  provisional
-- Portability :  portable
--
-- An efficient implementation of ordered maps from keys to values
-- (dictionaries).
--
-- API of this module is strict in both the keys and the values.
-- If you need value-lazy maps, use "Data.Map.Lazy" instead.
-- The 'Map' type is shared between the lazy and strict modules,
-- meaning that the same 'Map' value can be passed to functions in
-- both modules (although that is rarely needed).
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import qualified Data.Map.Strict as Map
--
-- The implementation of 'Map' is based on /size balanced/ binary trees (or
-- trees of /bounded balance/) as described by:
--
--    * Stephen Adams, \"/Efficient sets: a balancing act/\",
--     Journal of Functional Programming 3(4):553-562, October 1993,
--     <http://www.swiss.ai.mit.edu/~adams/BB/>.
--
--    * J. Nievergelt and E.M. Reingold,
--      \"/Binary search trees of bounded balance/\",
--      SIAM journal of computing 2(1), March 1973.
--
-- Note that the implementation is /left-biased/ -- the elements of a
-- first argument are always preferred to the second, for example in
-- 'union' or 'insert'.
--
-- /Warning/: The size of the map must not exceed @maxBound::Int@. Violation of
-- this condition is not detected and if the size limit is exceeded, its
-- behaviour is undefined.
--
-- Operation comments contain the operation time complexity in
-- the Big-O notation (<http://en.wikipedia.org/wiki/Big_O_notation>).
--
-- Be aware that the 'Functor', 'Traversable' and 'Data' instances
-- are the same as for the "Data.Map.Lazy" module, so if they are used
-- on strict maps, the resulting maps will be lazy.
-----------------------------------------------------------------------------

-- See the notes at the beginning of Data.Dependent.Map.Base.

module Indexed.Map.Strict
    (
    -- * Strictness properties
    -- $strictness

    -- * Map type
    Map              -- instance Eq,Show,Read
    -- * Operators
    , (!), (\\)

    -- * Query
    , null
    , size
    , member
    , notMember
    , lookup
    , findWithDefault
    , lookupLT
    , lookupGT
    , lookupLE
    , lookupGE

    -- * Construction
    , empty
    , singleton

    -- ** Insertion
    , insert
    , insertWith
    , insertWithKey
    , insertLookupWithKey

    -- ** Delete\/Update
    , delete
    , adjust
    , adjustWithKey
    , update
    , updateWithKey
    , updateLookupWithKey
    , alter

    -- * Combine

    -- ** Union
    , union
    , unionWith
    , unionWithKey
    , unions
    , unionsWith

    -- ** Difference
    , difference
    , differenceWith
    , differenceWithKey

    -- ** Intersection
    , intersection
    , intersectionWith
    , intersectionWithKey

    -- ** Universal combining function
    , mergeWithKey

    -- * Traversal
    -- ** Map
    , map
    , mapWithKey
    , traverseWithKey
    , mapAccum
    , mapAccumWithKey
    , mapAccumRWithKey
    , mapKeys
    , mapKeysWith
    , mapKeysMonotonic

    -- * Folds
    , foldr
    , foldl
    , foldrWithKey
    , foldlWithKey
    , foldMapWithKey

    -- ** Strict folds
    , foldr'
    , foldl'
    , foldrWithKey'
    , foldlWithKey'

    -- * Conversion
    , elems
    , keys
    , assocs
    , keysSet
    , fromSet

    -- ** Lists
    , toList
    , fromList
    , fromListWith
    , fromListWithKey

    -- ** Ordered lists
    , toAscList
    , toDescList
    , fromAscList
    , fromAscListWith
    , fromAscListWithKey
    , fromDistinctAscList

    -- * Filter
    , filter
    , filterWithKey
    , partition
    , partitionWithKey

    , mapMaybe
    , mapMaybeWithKey
    , mapEither
    , mapEitherWithKey

    , split
    , splitLookup
    , splitRoot

    -- * Submap
    , isSubmapOf, isSubmapOfBy
    , isProperSubmapOf, isProperSubmapOfBy

    -- * Indexed
    , lookupIndex
    , findIndex
    , elemAt
    , updateAt
    , deleteAt

    -- * Min\/Max
    , findMin
    , findMax
    , deleteMin
    , deleteMax
    , deleteFindMin
    , deleteFindMax
    , updateMin
    , updateMax
    , updateMinWithKey
    , updateMaxWithKey
    , minView
    , maxView
    , minViewWithKey
    , maxViewWithKey

    -- * Debugging
    , showTree
    , showTreeWith
    , valid
    ) where

import Prelude hiding (lookup,map,filter,foldr,foldl,null)

import Indexed.Map.Base hiding
    ( findWithDefault
    , singleton
    , insert
    , insertWith
    , insertWithKey
    , insertLookupWithKey
    , adjust
    , adjustWithKey
    , update
    , updateWithKey
    , updateLookupWithKey
    , alter
    , unionWith
    , unionWithKey
    , unionsWith
    , differenceWith
    , differenceWithKey
    , intersectionWith
    , intersectionWithKey
    , mergeWithKey
    , map
    , mapWithKey
    , mapAccum
    , mapAccumWithKey
    , mapAccumRWithKey
    , mapKeysWith
    , fromSet
    , fromList
    , fromListWith
    , fromListWithKey
    , fromAscList
    , fromAscListWith
    , fromAscListWithKey
    , fromDistinctAscList
    , mapMaybe
    , mapMaybeWithKey
    , mapEither
    , mapEitherWithKey
    , updateAt
    , updateMin
    , updateMax
    , updateMinWithKey
    , updateMaxWithKey
    )
import Indexed.Class
import qualified Indexed.Set.Base as Set
import Indexed.Some
import Indexed.Utils.StrictFold
import Indexed.Utils.StrictPair

import Data.Bits (shiftL, shiftR)


-- $strictness
--
-- This module satisfies the following strictness properties:
--
-- 1. Key arguments are evaluated to WHNF;
--
-- 2. Keys and values are evaluated to WHNF before they are stored in
--    the map.
--
-- Here's an example illustrating the first property:
--
-- > delete undefined m  ==  undefined
--
-- Here are some examples that illustrate the second property:
--
-- > map (\ v -> undefined) m  ==  undefined      -- m is not empty
-- > mapKeys (\ k -> undefined) m  ==  undefined  -- m is not empty

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.
--
-- > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
-- > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'

-- See Map.Base.Note: Local 'go' functions and capturing
findWithDefault :: IOrd k => a x -> k x -> Map k a -> a x
findWithDefault def k = k `seq` go
  where
    go Tip = def
    go (Bin _ kx x l r) = case icompare k kx of
      ILT -> go l
      IGT -> go r
      IEQ -> x
{-# INLINABLE findWithDefault #-}

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}

-- | /O(1)/. A map with a single element.
--
-- > singleton 1 'a'        == fromList [(1, 'a')]
-- > size (singleton 1 'a') == 1

singleton :: k x -> a x -> Map k a
singleton k x = x `seq` Bin 1 k x Tip Tip
{-# INLINE singleton #-}

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}
-- | /O(log n)/. Insert a new key and value in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. 'insert' is equivalent to
-- @'insertWith' 'const'@.
--
-- > insert 5 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'x')]
-- > insert 7 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'a'), (7, 'x')]
-- > insert 5 'x' empty                         == singleton 5 'x'

-- See Map.Base.Note: Type of local 'go' function
insert :: forall k a x. IOrd k => k x -> a x -> Map k a -> Map k a
insert = go
  where
    go :: k x -> a x -> Map k a -> Map k a
    go !kx !x Tip = singleton kx x
    go !kx !x (Bin sz ky y l r) =
        case icompare kx ky of
            ILT -> balanceL ky y (go kx x l) r
            IGT -> balanceR ky y l (go kx x r)
            IEQ -> Bin sz kx x l r
{-# INLINABLE insert #-}

-- | /O(log n)/. Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key, f new_value old_value)@.
--
-- > insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "xxxa")]
-- > insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWith (++) 5 "xxx" empty                         == singleton 5 "xxx"

insertWith :: IOrd k => (a x -> a x -> a x) -> k x -> a x -> Map k a -> Map k a
insertWith f = insertWithKey (\_ x' y' -> f x' y')
{-# INLINABLE insertWith #-}

-- | /O(log n)/. Insert with a function, combining key, new value and old value.
-- @'insertWithKey' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key,f key new_value old_value)@.
-- Note that the key passed to f is the same key passed to 'insertWithKey'.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:xxx|a")]
-- > insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWithKey f 5 "xxx" empty                         == singleton 5 "xxx"

-- See Map.Base.Note: Type of local 'go' function
insertWithKey :: forall k a x. IOrd k => (k x -> a x -> a x -> a x) -> k x -> a x -> Map k a -> Map k a
insertWithKey = go
  where
    go :: (k x -> a x -> a x -> a x) -> k x -> a x -> Map k a -> Map k a
    go _ !kx x Tip = singleton kx x
    go f !kx x (Bin sy ky y l r) =
        case icompare kx ky of
            ILT -> balanceL ky y (go f kx x l) r
            IGT -> balanceR ky y l (go f kx x r)
            IEQ -> let x' = f kx x y
                   in x' `seq` Bin sy kx x' l r
{-# INLINABLE insertWithKey #-}

-- | /O(log n)/. Combines insert operation with old value retrieval.
-- The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWithKey' f k x map@).
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertLookupWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:xxx|a")])
-- > insertLookupWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "xxx")])
-- > insertLookupWithKey f 5 "xxx" empty                         == (Nothing,  singleton 5 "xxx")
--
-- This is how to define @insertLookup@ using @insertLookupWithKey@:
--
-- > let insertLookup kx x t = insertLookupWithKey (\_ a _ -> a) kx x t
-- > insertLookup 5 "x" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "x")])
-- > insertLookup 7 "x" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "x")])

-- See Map.Base.Note: Type of local 'go' function
insertLookupWithKey :: forall k a x. IOrd k => (k x -> a x -> a x -> a x) -> k x -> a x -> Map k a
                    -> (Maybe (a x), Map k a)
insertLookupWithKey f0 kx0 x0 t0 = toPair $ go f0 kx0 x0 t0
  where
    go :: (k x -> a x -> a x -> a x) -> k x -> a x -> Map k a -> StrictPair (Maybe (a x)) (Map k a)
    go _ !kx x Tip = Nothing :*: singleton kx x
    go f !kx x (Bin sy ky y l r) =
        case icompare kx ky of
            ILT -> let (found :*: l') = go f kx x l
                   in found :*: balanceL ky y l' r
            IGT -> let (found :*: r') = go f kx x r
                   in found :*: balanceR ky y l r'
            IEQ -> let x' = f kx x y
                   in x' `seq` (Just y :*: Bin sy kx x' l r)
{-# INLINABLE insertLookupWithKey #-}

{--------------------------------------------------------------------
  Deletion
--------------------------------------------------------------------}

-- | /O(log n)/. Update a value at a specific key with the result of the provided function.
-- When the key is not
-- a member of the map, the original map is returned.
--
-- > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjust ("new " ++) 7 empty                         == empty

adjust :: IOrd k => (a x -> a x) -> k x -> Map k a -> Map k a
adjust f = adjustWithKey (\_ x -> f x)
{-# INLINABLE adjust #-}

-- | /O(log n)/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > let f key x = (show key) ++ ":new " ++ x
-- > adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjustWithKey f 7 empty                         == empty

adjustWithKey :: IOrd k => (k x -> a x -> a x) -> k x -> Map k a -> Map k a
adjustWithKey f = updateWithKey (\k' x' -> Just (f k' x'))
{-# INLINABLE adjustWithKey #-}

-- | /O(log n)/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

update :: IOrd k => (a x -> Maybe (a x)) -> k x -> Map k a -> Map k a
update f = updateWithKey (\_ x -> f x)
{-# INLINABLE update #-}

-- | /O(log n)/. The expression (@'updateWithKey' f k map@) updates the
-- value @x@ at @k@ (if it is in the map). If (@f k x@) is 'Nothing',
-- the element is deleted. If it is (@'Just' y@), the key @k@ is bound
-- to the new value @y@.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

-- See Map.Base.Note: Type of local 'go' function
updateWithKey :: forall k a x. IOrd k => (k x -> a x -> Maybe (a x)) -> k x -> Map k a -> Map k a
updateWithKey = go
  where
    go :: (k x -> a x -> Maybe (a x)) -> k x -> Map k a -> Map k a
    go _ !_ Tip = Tip
    go f !k (Bin sx kx x l r) =
        case icompare k kx of
           ILT -> balanceR kx x (go f k l) r
           IGT -> balanceL kx x l (go f k r)
           IEQ -> case f kx x of
                    Just x' -> x' `seq` Bin sx kx x' l r
                    Nothing -> glue l r
{-# INLINABLE updateWithKey #-}

-- | /O(log n)/. Lookup and update. See also 'updateWithKey'.
-- The function returns changed value, if it is updated.
-- Returns the original key value if the map entry is deleted.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) == (Just "5:new a", fromList [(3, "b"), (5, "5:new a")])
-- > updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a")])
-- > updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) == (Just "b", singleton 5 "a")

-- See Map.Base.Note: Type of local 'go' function
updateLookupWithKey :: forall k a x. IOrd k => (k x -> a x -> Maybe (a x)) -> k x -> Map k a -> (Maybe (a x), Map k a)
updateLookupWithKey f0 k0 t0 = toPair $ go f0 k0 t0
 where
   go :: (k x -> a x -> Maybe (a x)) -> k x -> Map k a -> StrictPair (Maybe (a x)) (Map k a)
   go _ !_ Tip = (Nothing :*: Tip)
   go f !k (Bin sx kx x l r) =
          case icompare k kx of
               ILT -> let (found :*: l') = go f k l
                      in found :*: balanceR kx x l' r
               IGT -> let (found :*: r') = go f k r
                      in found :*: balanceL kx x l r'
               IEQ -> case f kx x of
                        Just x' -> x' `seq` (Just x' :*: Bin sx kx x' l r)
                        Nothing -> (Just x :*: glue l r)
{-# INLINABLE updateLookupWithKey #-}

-- | /O(log n)/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in a 'Map'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
--
-- > let f _ = Nothing
-- > alter f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > alter f 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- >
-- > let f _ = Just "c"
-- > alter f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "c")]
-- > alter f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "c")]

-- See Map.Base.Note: Type of local 'go' function
alter :: forall k a x. IOrd k => (Maybe (a x) -> Maybe (a x)) -> k x -> Map k a -> Map k a
alter = go
  where
    go :: (Maybe (a x) -> Maybe (a x)) -> k x -> Map k a -> Map k a
    go f !k Tip = case f Nothing of
                   Nothing -> Tip
                   Just x  -> singleton k x

    go f !k (Bin sx kx x l r) = case icompare k kx of
               ILT -> balance kx x (go f k l) r
               IGT -> balance kx x l (go f k r)
               IEQ -> case f (Just x) of
                        Just x' -> x' `seq` Bin sx kx x' l r
                        Nothing -> glue l r
{-# INLINABLE alter #-}

{--------------------------------------------------------------------
  Indexing
--------------------------------------------------------------------}

-- | /O(log n)/. Update the element at /index/. Calls 'error' when an
-- invalid index is used.
--
-- > updateAt (\ _ _ -> Just "x") 0    (fromList [(5,"a"), (3,"b")]) == fromList [(3, "x"), (5, "a")]
-- > updateAt (\ _ _ -> Just "x") 1    (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "x")]
-- > updateAt (\ _ _ -> Just "x") 2    (fromList [(5,"a"), (3,"b")])    Error: index out of range
-- > updateAt (\ _ _ -> Just "x") (-1) (fromList [(5,"a"), (3,"b")])    Error: index out of range
-- > updateAt (\_ _  -> Nothing)  0    (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
-- > updateAt (\_ _  -> Nothing)  1    (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > updateAt (\_ _  -> Nothing)  2    (fromList [(5,"a"), (3,"b")])    Error: index out of range
-- > updateAt (\_ _  -> Nothing)  (-1) (fromList [(5,"a"), (3,"b")])    Error: index out of range

updateAt :: (forall x. k x -> a x -> Maybe (a x)) -> Int -> Map k a -> Map k a
updateAt f i t = i `seq`
  case t of
    Tip -> error "Map.updateAt: index out of range"
    Bin sx kx x l r -> case compare i sizeL of
      LT -> balanceR kx x (updateAt f i l) r
      GT -> balanceL kx x l (updateAt f (i-sizeL-1) r)
      EQ -> case f kx x of
              Just x' -> x' `seq` Bin sx kx x' l r
              Nothing -> glue l r
      where
        sizeL = size l

{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}

-- | /O(log n)/. Update the value at the minimal key.
--
-- > updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "Xb"), (5, "a")]
-- > updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMin :: (forall x. a x -> Maybe (a x)) -> Map k a -> Map k a
updateMin f m
  = updateMinWithKey (\_ x -> f x) m

-- | /O(log n)/. Update the value at the maximal key.
--
-- > updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "Xa")]
-- > updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMax :: (forall x. a x -> Maybe (a x)) -> Map k a -> Map k a
updateMax f m
  = updateMaxWithKey (\_ x -> f x) m


-- | /O(log n)/. Update the value at the minimal key.
--
-- > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"3:b"), (5,"a")]
-- > updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMinWithKey :: (forall x. k x -> a x -> Maybe (a x)) -> Map k a -> Map k a
updateMinWithKey _ Tip                 = Tip
updateMinWithKey f (Bin sx kx x Tip r) = case f kx x of
                                           Nothing -> r
                                           Just x' -> x' `seq` Bin sx kx x' Tip r
updateMinWithKey f (Bin _ kx x l r)    = balanceR kx x (updateMinWithKey f l) r

-- | /O(log n)/. Update the value at the maximal key.
--
-- > updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"b"), (5,"5:a")]
-- > updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMaxWithKey :: (forall x. k x -> a x -> Maybe (a x)) -> Map k a -> Map k a
updateMaxWithKey _ Tip                 = Tip
updateMaxWithKey f (Bin sx kx x l Tip) = case f kx x of
                                           Nothing -> l
                                           Just x' -> x' `seq` Bin sx kx x' l Tip
updateMaxWithKey f (Bin _ kx x l r)    = balanceL kx x l (updateMaxWithKey f r)

{--------------------------------------------------------------------
  Union.
--------------------------------------------------------------------}

-- | The union of a list of maps, with a combining operation:
--   (@'unionsWith' f == 'Prelude.foldl' ('unionWith' f) 'empty'@).
--
-- > unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]

unionsWith :: IOrd k => (forall x. a x -> a x -> a x) -> [Map k a] -> Map k a
unionsWith f ts
  = foldlStrict (unionWith f) empty ts
{-# INLINABLE unionsWith #-}

{--------------------------------------------------------------------
  Union with a combining function
--------------------------------------------------------------------}
-- | /O(n+m)/. Union with a combining function. The implementation uses the efficient /hedge-union/ algorithm.
--
-- > unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]

unionWith :: IOrd k => (forall x. a x -> a x -> a x) -> Map k a -> Map k a -> Map k a
unionWith f m1 m2
  = unionWithKey (\_ x y -> f x y) m1 m2
{-# INLINABLE unionWith #-}

-- | /O(n+m)/.
-- Union with a combining function. The implementation uses the efficient /hedge-union/ algorithm.
--
-- > let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
-- > unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]

unionWithKey :: IOrd k => (forall x. k x -> a x -> a x -> a x) -> Map k a -> Map k a -> Map k a
unionWithKey f t1 t2 = mergeWithKey (\k x1 x2 -> Just $ f k x1 x2) id id t1 t2
{-# INLINABLE unionWithKey #-}

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}

-- | /O(n+m)/. Difference with a combining function.
-- When two equal keys are
-- encountered, the combining function is applied to the values of these keys.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
-- The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
--
-- > let f al ar = if al == "b" then Just (al ++ ":" ++ ar) else Nothing
-- > differenceWith f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (7, "C")])
-- >     == singleton 3 "b:B"

differenceWith :: IOrd k => (forall x. a x -> b x -> Maybe (a x)) -> Map k a -> Map k b -> Map k a
differenceWith f m1 m2
  = differenceWithKey (\_ x y -> f x y) m1 m2
{-# INLINABLE differenceWith #-}

-- | /O(n+m)/. Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
-- The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
--
-- > let f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing
-- > differenceWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (10, "C")])
-- >     == singleton 3 "3:b|B"

differenceWithKey :: IOrd k => (forall x. k x -> a x -> b x -> Maybe (a x)) -> Map k a -> Map k b -> Map k a
differenceWithKey f t1 t2 = mergeWithKey f id (const Tip) t1 t2
{-# INLINABLE differenceWithKey #-}


{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}

-- | /O(n+m)/. Intersection with a combining function.  The implementation uses
-- an efficient /hedge/ algorithm comparable with /hedge-union/.
--
-- > intersectionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "aA"

intersectionWith :: IOrd k => (forall x. a x -> b x -> c x) -> Map k a -> Map k b -> Map k c
intersectionWith f m1 m2
  = intersectionWithKey (\_ x y -> f x y) m1 m2
{-# INLINABLE intersectionWith #-}

-- | /O(n+m)/. Intersection with a combining function.  The implementation uses
-- an efficient /hedge/ algorithm comparable with /hedge-union/.
--
-- > let f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar
-- > intersectionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "5:a|A"


intersectionWithKey :: IOrd k => (forall x. k x -> a x -> b x -> c x) -> Map k a -> Map k b -> Map k c
intersectionWithKey f t1 t2 = mergeWithKey (\k x1 x2 -> Just $ f k x1 x2) (const Tip) (const Tip) t1 t2
{-# INLINABLE intersectionWithKey #-}


{--------------------------------------------------------------------
  MergeWithKey
--------------------------------------------------------------------}

-- | /O(n+m)/. A high-performance universal combining function. This function
-- is used to define 'unionWith', 'unionWithKey', 'differenceWith',
-- 'differenceWithKey', 'intersectionWith', 'intersectionWithKey' and can be
-- used to define other custom combine functions.
--
-- Please make sure you know what is going on when using 'mergeWithKey',
-- otherwise you can be surprised by unexpected code growth or even
-- corruption of the data structure.
--
-- When 'mergeWithKey' is given three arguments, it is inlined to the call
-- site. You should therefore use 'mergeWithKey' only to define your custom
-- combining functions. For example, you could define 'unionWithKey',
-- 'differenceWithKey' and 'intersectionWithKey' as
--
-- > myUnionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) id id m1 m2
-- > myDifferenceWithKey f m1 m2 = mergeWithKey f id (const empty) m1 m2
-- > myIntersectionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) (const empty) (const empty) m1 m2
--
-- When calling @'mergeWithKey' combine only1 only2@, a function combining two
-- 'IntMap's is created, such that
--
-- * if a key is present in both maps, it is passed with both corresponding
--   values to the @combine@ function. Depending on the result, the key is either
--   present in the result with specified value, or is left out;
--
-- * a nonempty subtree present only in the first map is passed to @only1@ and
--   the output is added to the result;
--
-- * a nonempty subtree present only in the second map is passed to @only2@ and
--   the output is added to the result.
--
-- The @only1@ and @only2@ methods /must return a map with a subset (possibly empty) of the keys of the given map/.
-- The values can be modified arbitrarily. Most common variants of @only1@ and
-- @only2@ are 'id' and @'const' 'empty'@, but for example @'map' f@ or
-- @'filterWithKey' f@ could be used for any @f@.

mergeWithKey :: IOrd k => (forall x. k x -> a x -> b x -> Maybe (c x)) -> (Map k a -> Map k c) -> (Map k b -> Map k c)
             -> Map k a -> Map k b -> Map k c
mergeWithKey f g1 g2 = go
  where
    go Tip t2 = g2 t2
    go t1 Tip = g1 t1
    go t1 t2 = hedgeMerge NothingS NothingS t1 t2

    hedgeMerge _   _   t1  Tip = g1 t1
    hedgeMerge blo bhi Tip (Bin _ kx x l r) = g2 $ link kx x (filterGt blo l) (filterLt bhi r)
    hedgeMerge blo bhi (Bin _ kx x l r) t2 = let l' = hedgeMerge blo bmi l (trim blo bmi t2)
                                                 (found, trim_t2) = trimLookupLo kx bhi t2
                                                 r' = hedgeMerge bmi bhi r trim_t2
                                             in case found of
                                                  Nothing -> case g1 (singleton kx x) of
                                                               Tip -> merge l' r'
                                                               (Bin _ k' x' Tip Tip) -> case ieq k' kx of
                                                                   ITrue -> link kx x' l' r'
                                                                   _     -> error "mergeWithKey: Given function only1 does not fulfil required conditions (see documentation)"
                                                               _ -> error "mergeWithKey: Given function only1 does not fulfil required conditions (see documentation)"
                                                  Just x2 -> case f kx x x2 of
                                                               Nothing -> merge l' r'
                                                               Just x' -> x' `seq` link kx x' l' r'
      where bmi = JustS (Some1 kx)
{-# INLINE mergeWithKey #-}

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}

-- | /O(n)/. Map values and collect the 'Just' results.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > mapMaybe f (fromList [(5,"a"), (3,"b")]) == singleton 5 "new a"

mapMaybe :: (forall x. a x -> Maybe (b x)) -> Map k a -> Map k b
mapMaybe f = mapMaybeWithKey (\_ x -> f x)

-- | /O(n)/. Map keys\/values and collect the 'Just' results.
--
-- > let f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing
-- > mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) == singleton 3 "key : 3"

mapMaybeWithKey :: (forall x. k x -> a x -> Maybe (b x)) -> Map k a -> Map k b
mapMaybeWithKey _ Tip = Tip
mapMaybeWithKey f (Bin _ kx x l r) = case f kx x of
  Just y  -> y `seq` link kx y (mapMaybeWithKey f l) (mapMaybeWithKey f r)
  Nothing -> merge (mapMaybeWithKey f l) (mapMaybeWithKey f r)

-- | /O(n)/. Map values and separate the 'Left' and 'Right' results.
--
-- > let f a = if a < "c" then Left a else Right a
-- > mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
-- >
-- > mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])

mapEither :: (forall x. a x -> Either (b x) (c x)) -> Map k a -> (Map k b, Map k c)
mapEither f m
  = mapEitherWithKey (\_ x -> f x) m

-- | /O(n)/. Map keys\/values and separate the 'Left' and 'Right' results.
--
-- > let f k a = if k < 5 then Left (k * 2) else Right (a ++ a)
-- > mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
-- >
-- > mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])

mapEitherWithKey :: (forall x. k x -> a x -> Either (b x) (c x)) -> Map k a -> (Map k b, Map k c)
mapEitherWithKey f0 t0 = toPair $ go f0 t0
  where
    go :: (forall x. k x -> a x -> Either (b x) (c x)) -> Map k a -> StrictPair (Map k b) (Map k c)
    go _ Tip = (Tip :*: Tip)
    go f (Bin _ kx x l r) = case f kx x of
      Left y  -> y `seq` (link kx y l1 r1 :*: merge l2 r2)
      Right z -> z `seq` (merge l1 r1 :*: link kx z l2 r2)
     where
        (l1 :*: l2) = go f l
        (r1 :*: r2) = go f r

{--------------------------------------------------------------------
  Mapping
--------------------------------------------------------------------}
-- | /O(n)/. Map a function over all values in the map.
--
-- > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]

map :: (forall x. a x -> b x) -> Map k a -> Map k b
map _ Tip = Tip
map f (Bin sx kx x l r) = let x' = f x in x' `seq` Bin sx kx x' (map f l) (map f r)

-- | /O(n)/. Map a function over all values in the map.
--
-- > let f key x = (show key) ++ ":" ++ x
-- > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]

mapWithKey :: (forall x. k x -> a x -> b x) -> Map k a -> Map k b
mapWithKey _ Tip = Tip
mapWithKey f (Bin sx kx x l r) =
  let x' = f kx x
  in x' `seq` Bin sx kx x' (mapWithKey f l) (mapWithKey f r)

-- | /O(n)/. The function 'mapAccum' threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a b = (a ++ b, b ++ "X")
-- > mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) == ("Everything: ba", fromList [(3, "bX"), (5, "aX")])

mapAccum :: (forall x. a -> b x -> (a, c x)) -> a -> Map k b -> (a, Map k c)
mapAccum f a m
  = mapAccumWithKey (\a' _ x' -> f a' x') a m

-- | /O(n)/. The function 'mapAccumWithKey' threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
-- > mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) == ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])

mapAccumWithKey :: (forall x. a -> k x -> b x -> (a, c x)) -> a -> Map k b -> (a, Map k c)
mapAccumWithKey f a t
  = mapAccumL f a t

-- | /O(n)/. The function 'mapAccumL' threads an accumulating
-- argument through the map in ascending order of keys.
mapAccumL :: (forall x. a -> k x -> b x -> (a, c x)) -> a -> Map k b -> (a, Map k c)
mapAccumL _ a Tip               = (a,Tip)
mapAccumL f a (Bin sx kx x l r) =
  let (a1,l') = mapAccumL f a l
      (a2,x') = f a1 kx x
      (a3,r') = mapAccumL f a2 r
  in x' `seq` (a3,Bin sx kx x' l' r')

-- | /O(n)/. The function 'mapAccumR' threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (forall x. a -> k x -> b x -> (a, c x)) -> a -> Map k b -> (a,Map k c)
mapAccumRWithKey _ a Tip = (a,Tip)
mapAccumRWithKey f a (Bin sx kx x l r) =
  let (a1,r') = mapAccumRWithKey f a r
      (a2,x') = f a1 kx x
      (a3,l') = mapAccumRWithKey f a2 l
  in x' `seq` (a3,Bin sx kx x' l' r')

-- | /O(n*log n)/.
-- @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@.
--
-- > mapKeysWith (++) (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "cdab"
-- > mapKeysWith (++) (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "cdab"

mapKeysWith :: IOrd k2 => (forall x. a x -> a x -> a x) -> (forall x. k1 x -> k2 x) -> Map k1 a -> Map k2 a
mapKeysWith c f = fromListWith c . foldrWithKey (\k x xs -> (Some2 (f k) x) : xs) []
{-# INLINABLE mapKeysWith #-}

{--------------------------------------------------------------------
  Conversions
--------------------------------------------------------------------}

-- | /O(n)/. Build a map from a set of keys and a function which for each key
-- computes its value.
--
-- > fromSet (\k -> replicate k 'a') (Data.Set.fromList [3, 5]) == fromList [(5,"aaaaa"), (3,"aaa")]
-- > fromSet undefined Data.Set.empty == empty

fromSet :: (forall x. k x -> a x) -> Set.Set k -> Map k a
fromSet _ Set.Tip = Tip
fromSet f (Set.Bin sz x l r) = case f x of v -> v `seq` Bin sz x v (fromSet f l) (fromSet f r)

{--------------------------------------------------------------------
  Lists
  use [foldlStrict] to reduce demand on the control-stack
--------------------------------------------------------------------}
-- | /O(n*log n)/. Build a map from a list of key\/value pairs. See also 'fromAscList'.
-- If the list contains more than one value for the same key, the last value
-- for the key is retained.
--
-- If the keys of the list are ordered, linear-time implementation is used,
-- with the performance equal to 'fromDistinctAscList'.
--
-- > fromList [] == empty
-- > fromList [(5,"a"), (3,"b"), (5, "c")] == fromList [(5,"c"), (3,"b")]
-- > fromList [(5,"c"), (3,"b"), (5, "a")] == fromList [(5,"a"), (3,"b")]

-- For some reason, when 'singleton' is used in fromList or in
-- create, it is not inlined, so we inline it manually.
fromList :: IOrd k => [Some2 k a] -> Map k a
fromList [] = Tip
fromList [Some2 kx x] = x `seq` Bin 1 kx x Tip Tip
fromList ((Some2 kx0 x0) : xs0) | not_ordered kx0 xs0 = x0 `seq` fromList' (Bin 1 kx0 x0 Tip Tip) xs0
                                | otherwise = x0 `seq` go (1::Int) (Bin 1 kx0 x0 Tip Tip) xs0
  where
    not_ordered _ [] = False
    not_ordered kx (Some2 ky _ : _) = case icompare kx ky of
        ILT -> False
        _   -> True
    {-# INLINE not_ordered #-}

    fromList' t0 xs = foldlStrict ins t0 xs
      where ins t (Some2 k x) = insert k x t

    go !_ t [] = t
    go !_ t [(Some2 kx x)] = x `seq` insertMax kx x t
    go !s l xs@((Some2 kx x) : xss) | not_ordered kx xss = fromList' l xs
                                    | otherwise = case create s xss of
                                        (r, ys, []) -> x `seq` go (s `shiftL` 1) (link kx x l r) ys
                                        (r, _,  ys) -> x `seq` fromList' (link kx x l r) ys

    -- The create is returning a triple (tree, xs, ys). Both xs and ys
    -- represent not yet processed elements and only one of them can be nonempty.
    -- If ys is nonempty, the keys in ys are not ordered with respect to tree
    -- and must be inserted using fromList'. Otherwise the keys have been
    -- ordered so far.
    create !_ [] = (Tip, [], [])
    create !s xs@(xp : xss)
      | s == 1 = case xp of (Some2 kx x) | not_ordered kx xss -> x `seq` (Bin 1 kx x Tip Tip, [], xss)
                                         | otherwise -> x `seq` (Bin 1 kx x Tip Tip, xss, [])
      | otherwise = case create (s `shiftR` 1) xs of
                      res@(_, [], _) -> res
                      (l, [(Some2 ky y)], zs) -> y `seq` (insertMax ky y l, [], zs)
                      (l, ys@((Some2 ky y):yss), _) | not_ordered ky yss -> (l, [], ys)
                                                    | otherwise -> case create (s `shiftR` 1) yss of
                                                        (r, zs, ws) -> y `seq` (link ky y l r, zs, ws)
{-# INLINABLE fromList #-}

-- | /O(n*log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
--
-- > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "ab"), (5, "aba")]
-- > fromListWith (++) [] == empty

fromListWith :: IOrd k => (forall x. a x -> a x -> a x) -> [Some2 k a] -> Map k a
fromListWith f xs
  = fromListWithKey (\_ x y -> f x y) xs
{-# INLINABLE fromListWith #-}

-- | /O(n*log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWithKey'.
--
-- > let f k a1 a2 = (show k) ++ a1 ++ a2
-- > fromListWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "3ab"), (5, "5a5ba")]
-- > fromListWithKey f [] == empty

fromListWithKey :: IOrd k => (forall x. k x -> a x -> a x -> a x) -> [(Some2 k a)] -> Map k a
fromListWithKey f xs
  = foldlStrict ins empty xs
  where
    ins t (Some2 k x) = insertWithKey f k x t
{-# INLINABLE fromListWithKey #-}

{--------------------------------------------------------------------
  Building trees from ascending/descending lists can be done in linear time.

  Note that if [xs] is ascending that:
    fromAscList xs       == fromList xs
    fromAscListWith f xs == fromListWith f xs
--------------------------------------------------------------------}
-- | /O(n)/. Build a map from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
--
-- > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
-- > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]
-- > valid (fromAscList [(3,"b"), (5,"a"), (5,"b")]) == True
-- > valid (fromAscList [(5,"a"), (3,"b"), (5,"b")]) == False

fromAscList :: IEq k => [(Some2 k a)] -> Map k a
fromAscList xs
  = fromAscListWithKey (\_ x _ -> x) xs
{-# INLINABLE fromAscList #-}

-- | /O(n)/. Build a map from an ascending list in linear time with a combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "ba")]
-- > valid (fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")]) == True
-- > valid (fromAscListWith (++) [(5,"a"), (3,"b"), (5,"b")]) == False

fromAscListWith :: IEq k => (forall x. a x -> a x -> a x) -> [Some2 k a] -> Map k a
fromAscListWith f xs
  = fromAscListWithKey (\_ x y -> f x y) xs
{-# INLINABLE fromAscListWith #-}

-- | /O(n)/. Build a map from an ascending list in linear time with a
-- combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > let f k a1 a2 = (show k) ++ ":" ++ a1 ++ a2
-- > fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b"), (5,"b")] == fromList [(3, "b"), (5, "5:b5:ba")]
-- > valid (fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b"), (5,"b")]) == True
-- > valid (fromAscListWithKey f [(5,"a"), (3,"b"), (5,"b"), (5,"b")]) == False

fromAscListWithKey :: IEq k => (forall x. k x -> a x -> a x -> a x) -> [(Some2 k a)] -> Map k a
fromAscListWithKey f xs
  = fromDistinctAscList (combineEq f xs)
  where
  -- [combineEq f xs] combines equal elements with function [f] in an ordered list [xs]
  combineEq _ xs'
    = case xs' of
        []     -> []
        [x]    -> [x]
        (x:xx) -> combineEq' x xx

  combineEq' z [] = [z]
  combineEq' z@(Some2 kz zz) (x@(Some2 kx xx):xs')
    | ITrue <- ieq kx kz = let yy = f kx xx zz in yy `seq` combineEq' (Some2 kx yy) xs'
    | otherwise          = z:combineEq' x xs'
{-# INLINABLE fromAscListWithKey #-}

-- | /O(n)/. Build a map from an ascending list of distinct elements in linear time.
-- /The precondition is not checked./
--
-- > fromDistinctAscList [(3,"b"), (5,"a")] == fromList [(3, "b"), (5, "a")]
-- > valid (fromDistinctAscList [(3,"b"), (5,"a")])          == True
-- > valid (fromDistinctAscList [(3,"b"), (5,"a"), (5,"b")]) == False

-- For some reason, when 'singleton' is used in fromDistinctAscList or in
-- create, it is not inlined, so we inline it manually.
fromDistinctAscList :: [Some2 k a] -> Map k a
fromDistinctAscList [] = Tip
fromDistinctAscList ((Some2 kx0 x0) : xs0) = x0 `seq` go (1::Int) (Bin 1 kx0 x0 Tip Tip) xs0
  where
    go !_ t [] = t
    go !s l ((Some2 kx x) : xs) = case create s xs of
                                    (r, ys) -> x `seq` go (s `shiftL` 1) (link kx x l r) ys

    create !_ [] = (Tip, [])
    create !s xs@(x' : xs')
      | s == 1 = case x' of (Some2 kx x) -> x `seq` (Bin 1 kx x Tip Tip, xs')
      | otherwise = case create (s `shiftR` 1) xs of
                      res@(_, []) -> res
                      (l, (Some2 ky y):ys) -> case create (s `shiftR` 1) ys of
                        (r, zs) -> y `seq` (link ky y l r, zs)
