{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Indexed.Map.Base
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
--                (c) SkedgeMe LLC 2015
-- License     :  BSD-style
-- Maintainer  :  daniel.haraj@skedge.me
-- Stability   :  provisional
-- Portability :  portable
--
-- An efficient implementation of maps from keys to values (dictionaries).
--
-- Since many function names (but not the type name) clash with
-- "Prelude" names, this module is usually imported @qualified@, e.g.
--
-- >  import Data.Map (Map)
-- >  import qualified Data.Map as Map
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
-- Operation comments contain the operation time complexity in
-- the Big-O notation <http://en.wikipedia.org/wiki/Big_O_notation>.
-----------------------------------------------------------------------------

-- [Note: Using INLINABLE]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- It is crucial to the performance that the functions specialize on the Ord
-- type when possible. GHC 7.0 and higher does this by itself when it sees th
-- unfolding of a function -- that is why all public functions are marked
-- INLINABLE (that exposes the unfolding).


-- [Note: Using INLINE]
-- ~~~~~~~~~~~~~~~~~~~~
-- For other compilers and GHC pre 7.0, we mark some of the functions INLINE.
-- We mark the functions that just navigate down the tree (lookup, insert,
-- delete and similar). That navigation code gets inlined and thus specialized
-- when possible. There is a price to pay -- code growth. The code INLINED is
-- therefore only the tree navigation, all the real work (rebalancing) is not
-- INLINED by using a NOINLINE.
--
-- All methods marked INLINE have to be nonrecursive -- a 'go' function doing
-- the real work is provided.


-- [Note: Type of local 'go' function]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- If the local 'go' function uses an Ord class, it sometimes heap-allocates
-- the Ord dictionary when the 'go' function does not have explicit type.
-- In that case we give 'go' explicit type. But this slightly decrease
-- performance, as the resulting 'go' function can float out to top level.


-- [Note: Local 'go' functions and capturing]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- As opposed to IntMap, when 'go' function captures an argument, increased
-- heap-allocation can occur: sometimes in a polymorphic function, the 'go'
-- floats out of its enclosing function and then it heap-allocates the
-- dictionary and the argument. Maybe it floats out too late and strictness
-- analyzer cannot see that these could be passed on stack.
--
-- For example, change 'member' so that its local 'go' function is not passing
-- argument k and then look at the resulting code for hedgeInt.


-- [Note: Order of constructors]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The order of constructors of Map matters when considering performance.
-- Currently in GHC 7.0, when type has 2 constructors, a forward conditional
-- jump is made when successfully matching second constructor. Successful match
-- of first constructor results in the forward jump not taken.
-- On GHC 7.0, reordering constructors from Tip | Bin to Bin | Tip
-- improves the benchmark by up to 10% on x86.

module Indexed.Map.Base (
    -- * Map type
      Map(..)          -- instance Eq,Show,Read

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

    -- Used by the strict version
    , bin
    , balance
    , balanced
    , balanceL
    , balanceR
    , delta
    , link
    , insertMax
    , merge
    , glue
    , trim
    , trimLookupLo
    , MaybeS(..)
    , filterGt
    , filterLt
    ) where

import Control.Applicative (Applicative(..), (<$>))
import Data.Monoid (Monoid(..))

import Data.Bits (shiftL, shiftR)
import Data.Typeable
import Prelude hiding (lookup, map, filter, foldr, foldl, null)

import Indexed.Class
import qualified Indexed.Set.Base as Set
import Indexed.Some
import Indexed.Utils.StrictFold
import Indexed.Utils.StrictPair

import GHC.Exts ( build )
import qualified GHC.Exts as GHCExts
import Text.Read


{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 !,\\ --

-- | /O(log n)/. Find the value at a key.
-- Calls 'error' when the element can not be found.
--
-- > fromList [(5,'a'), (3,'b')] ! 1    Error: element not in the map
-- > fromList [(5,'a'), (3,'b')] ! 5 == 'a'

(!) :: IOrd k => Map k a -> k x -> a x
(!) m k = find k m
{-# INLINABLE (!) #-}

-- | Same as 'difference'.
(\\) :: IOrd k => Map k a -> Map k b -> Map k a
m1 \\ m2 = difference m1 m2
{-# INLINABLE (\\) #-}

{--------------------------------------------------------------------
  Size balanced trees.
--------------------------------------------------------------------}
-- | A Map from keys @k@ to values @a@.

-- See Note: Order of constructors
data Map (k :: i -> *) (a :: i -> *)  = 
   forall x. Bin {-# UNPACK #-} !Size !(k x) (a x) !(Map k a) !(Map k a)
 | Tip

type Size     = Int

type role Map nominal representational

instance IOrd k => Monoid (Map k v) where
    mempty  = empty
    mappend = union
    mconcat = unions

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is the map empty?
--
-- > Data.Map.null (empty)           == True
-- > Data.Map.null (singleton 1 'a') == False

null :: Map k a -> Bool
null Tip      = True
null (Bin {}) = False
{-# INLINE null #-}

-- | /O(1)/. The number of elements in the map.
--
-- > size empty                                   == 0
-- > size (singleton 1 'a')                       == 1
-- > size (fromList([(1,'a'), (2,'c'), (3,'b')])) == 3

size :: Map k a -> Int
size Tip              = 0
size (Bin sz _ _ _ _) = sz
{-# INLINE size #-}


-- | /O(log n)/. Lookup the value at a key in the map.
--
-- The function will return the corresponding value as @('Just' value)@,
-- or 'Nothing' if the key isn't in the map.
--
-- An example of using @lookup@:
--
-- > import Prelude hiding (lookup)
-- > import Data.Map
-- >
-- > employeeDept = fromList([("John","Sales"), ("Bob","IT")])
-- > deptCountry = fromList([("IT","USA"), ("Sales","France")])
-- > countryCurrency = fromList([("USA", "Dollar"), ("France", "Euro")])
-- >
-- > employeeCurrency :: String -> Maybe String
-- > employeeCurrency name = do
-- >     dept <- lookup name employeeDept
-- >     country <- lookup dept deptCountry
-- >     lookup country countryCurrency
-- >
-- > main = do
-- >     putStrLn $ "John's currency: " ++ (show (employeeCurrency "John"))
-- >     putStrLn $ "Pete's currency: " ++ (show (employeeCurrency "Pete"))
--
-- The output of this program:
--
-- >   John's currency: Just "Euro"
-- >   Pete's currency: Nothing
lookup :: forall k a x. IOrd k => k x -> Map k a -> Maybe (a x)
lookup = go
  where
    go :: k x -> Map k a -> Maybe (a x)
    go !_ Tip = Nothing
    go !k (Bin _ kx x l r) = case icompare k kx of
      ILT -> go k l
      IGT -> go k r
      IEQ -> Just x
{-# INLINABLE lookup #-}

-- | /O(log n)/. Is the key a member of the map? See also 'notMember'.
--
-- > member 5 (fromList [(5,'a'), (3,'b')]) == True
-- > member 1 (fromList [(5,'a'), (3,'b')]) == False
member :: IOrd k => k x -> Map k a -> Bool
member = go
  where
    go !_ Tip = False
    go !k (Bin _ kx _ l r) = case icompare k kx of
      ILT -> go k l
      IGT -> go k r
      IEQ -> True
{-# INLINABLE member #-}

-- | /O(log n)/. Is the key not a member of the map? See also 'member'.
--
-- > notMember 5 (fromList [(5,'a'), (3,'b')]) == False
-- > notMember 1 (fromList [(5,'a'), (3,'b')]) == True

notMember :: IOrd k => k x -> Map k a -> Bool
notMember k m = not $ member k m
{-# INLINABLE notMember #-}
-- | /O(log n)/. Find the value at a key.
-- Calls 'error' when the element can not be found.
find :: forall k a x. IOrd k => k x -> Map k a -> a x
find = go
  where
    go :: k x -> Map k a -> a x
    go !_ Tip = error "Map.!: given key is not an element in the map"
    go !k (Bin _ kx x l r) = case icompare k kx of
      ILT -> go k l
      IGT -> go k r
      IEQ -> x
{-# INLINABLE find #-}

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.
--
-- > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
-- > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'
findWithDefault :: forall k a x. IOrd k => a x -> k x -> Map k a -> a x
findWithDefault = go
  where
    go :: a x -> k x -> Map k a -> a x
    go def !_ Tip = def
    go def !k (Bin _ kx x l r) = case icompare k kx of
      ILT -> go def k l
      IGT -> go def k r
      IEQ -> x
{-# INLINABLE findWithDefault #-}

-- | /O(log n)/. Find largest key smaller than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupLT 3 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLT 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
lookupLT :: IOrd k => k x -> Map k v -> Maybe (Some2 k v)
lookupLT = goNothing
  where
    goNothing :: forall k v x. IOrd k => k x -> Map k v -> Maybe (Some2 k v)
    goNothing !_ Tip = Nothing
    goNothing !k (Bin _ kx x l r) = case icompare k kx of
      IGT -> goJust k kx x r
      _     -> goNothing k l
    goJust :: forall k v x y. IOrd k => k x -> k y -> v y -> Map k v -> Maybe (Some2 k v)
    goJust !_ kx' x' Tip = Just (Some2 kx' x')
    goJust !k kx' x' (Bin _ kx x l r) = case icompare k kx of
      IGT -> goJust k kx x r
      _     -> goJust k kx' x' l
{-# INLINABLE lookupLT #-}

-- | /O(log n)/. Find smallest key greater than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupGT 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGT 5 (fromList [(3,'a'), (5,'b')]) == Nothing
lookupGT :: IOrd k => k x -> Map k v -> Maybe (Some2 k v)
lookupGT = goNothing
  where
    goNothing :: forall k v x. IOrd k => k x -> Map k v -> Maybe (Some2 k v)
    goNothing !_ Tip = Nothing
    goNothing !k (Bin _ kx x l r) = case icompare k kx of
      ILT -> goJust k kx x l
      _     -> goNothing k r
    goJust :: forall k v x y. IOrd k => k x -> k y -> v y -> Map k v -> Maybe (Some2 k v)
    goJust !_ kx' x' Tip = Just (Some2 kx' x')
    goJust !k kx' x' (Bin _ kx x l r) = case icompare k kx of
      ILT -> goJust k kx x l
      _     -> goJust k kx' x' r
{-# INLINABLE lookupGT #-}

-- | /O(log n)/. Find largest key smaller or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupLE 2 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLE 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupLE 5 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
lookupLE :: IOrd k => k x -> Map k v -> Maybe (Some2 k v)
lookupLE = goNothing
  where
    goNothing :: forall k v x. IOrd k => k x -> Map k v -> Maybe (Some2 k v)
    goNothing !_ Tip = Nothing
    goNothing !k (Bin _ kx x l r) = case icompare k kx of
                                      ILT -> goNothing k l
                                      IEQ -> Just (Some2 kx x)
                                      IGT -> goJust k kx x r
    goJust :: forall k v x y. IOrd k => k x -> k y -> v y -> Map k v -> Maybe (Some2 k v)
    goJust !_ kx' x' Tip = Just (Some2 kx' x')
    goJust !k kx' x' (Bin _ kx x l r) = case icompare k kx of 
                                         ILT -> goJust k kx' x' l
                                         IEQ -> Just (Some2 kx x)
                                         IGT -> goJust k kx x r
{-# INLINABLE lookupLE #-}

-- | /O(log n)/. Find smallest key greater or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupGE 3 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupGE 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGE 6 (fromList [(3,'a'), (5,'b')]) == Nothing
lookupGE :: IOrd k => k x -> Map k v -> Maybe (Some2 k v)
lookupGE = goNothing
  where
    goNothing :: forall k v x. IOrd k => k x -> Map k v -> Maybe (Some2 k v)
    goNothing !_ Tip = Nothing
    goNothing !k (Bin _ kx x l r) = case icompare k kx of 
                                      ILT -> goJust k kx x l
                                      IEQ -> Just (Some2 kx x)
                                      IGT -> goNothing k r
    goJust :: forall k v x y. IOrd k => k x -> k y -> v y -> Map k v -> Maybe (Some2 k v)
    goJust !_ kx' x' Tip = Just (Some2 kx' x')
    goJust !k kx' x' (Bin _ kx x l r) = case icompare k kx of 
                                          ILT -> goJust k kx x l
                                          IEQ -> Just (Some2 kx x)
                                          IGT -> goJust k kx' x' r
{-# INLINABLE lookupGE #-}

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. The empty map.
--
-- > empty      == fromList []
-- > size empty == 0

empty :: Map k a
empty = Tip
{-# INLINE empty #-}

-- | /O(1)/. A map with a single element.
--
-- > singleton 1 'a'        == fromList [(1, 'a')]
-- > size (singleton 1 'a') == 1

singleton :: k x -> a x -> Map k a
singleton k x = Bin 1 k x Tip Tip
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

-- See Note: Type of local 'go' function
insert :: forall k a x. IOrd k => k x -> a x -> Map k a -> Map k a
insert = go
  where
    go :: k x -> a x -> Map k a -> Map k a
    go !kx x Tip = singleton kx x
    go !kx x (Bin sz ky y l r) =
        case icompare kx ky of
          ILT -> balanceL ky y (go kx x l) r
          IGT -> balanceR ky y l (go kx x r)
          IEQ -> Bin sz kx x l r
{-# INLINABLE insert #-}

-- Insert a new key and value in the map if it is not already present.
-- Used by `union`.

-- See Note: Type of local 'go' function
insertR :: forall k a x. IOrd k => k x -> a x -> Map k a -> Map k a
insertR = go
  where
    go :: k x -> a x -> Map k a -> Map k a
    go !kx x Tip = singleton kx x
    go !kx x t@(Bin _ ky y l r) =
        case icompare kx ky of
          ILT -> balanceL ky y (go kx x l) r
          IGT -> balanceR ky y l (go kx x r)
          IEQ -> t
{-# INLINABLE insertR #-}

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

-- See Note: Type of local 'go' function
insertWithKey :: forall k a x. IOrd k => (k x -> a x -> a x -> a x) -> k x -> a x -> Map k a -> Map k a
insertWithKey = go
  where
    go :: (k x -> a x -> a x -> a x) -> k x -> a x -> Map k a -> Map k a
    go _ !kx x Tip = singleton kx x
    go f !kx x (Bin sy ky y l r) =
        case icompare kx ky of
          ILT -> balanceL ky y (go f kx x l) r
          IGT -> balanceR ky y l (go f kx x r)
          IEQ -> Bin sy kx (f kx x y) l r
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

-- See Note: Type of local 'go' function
insertLookupWithKey :: IOrd k => (k x -> a x -> a x -> a x) -> k x -> a x -> Map k a
                    -> (Maybe (a x), Map k a)
insertLookupWithKey = go
  where
    go :: IOrd k => (k x -> a x -> a x -> a x) -> k x -> a x -> Map k a -> (Maybe (a x), Map k a)
    go _ !kx x Tip = (Nothing, singleton kx x)
    go f !kx x (Bin sy ky y l r) =
        case icompare kx ky of
          ILT -> let (found, l') = go f kx x l
                   in (found, balanceL ky y l' r)
          IGT -> let (found, r') = go f kx x r
                   in (found, balanceR ky y l r')
          IEQ -> (Just y, Bin sy kx (f kx x y) l r)
{-# INLINABLE insertLookupWithKey #-}

{--------------------------------------------------------------------
  Deletion
--------------------------------------------------------------------}
-- | /O(log n)/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
--
-- > delete 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > delete 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > delete 5 empty                         == empty

-- See Note: Type of local 'go' function
delete :: IOrd k => k x -> Map k a -> Map k a
delete = go
  where
    go :: IOrd k => k x -> Map k a -> Map k a
    go !_ Tip = Tip
    go !k (Bin _ kx x l r) =
      case icompare k kx of
        ILT -> balanceR kx x (go k l) r
        IGT -> balanceL kx x l (go k r)
        IEQ -> glue l r
{-# INLINABLE delete #-}

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

-- See Note: Type of local 'go' function
updateWithKey :: IOrd k => (k x -> a x -> Maybe (a x)) -> k x -> Map k a -> Map k a
updateWithKey = go
  where
    go :: IOrd k => (k x -> a x -> Maybe (a x)) -> k x -> Map k a -> Map k a
    go _ !_ Tip = Tip
    go f !k(Bin sx kx x l r) =
        case icompare k kx of
          ILT -> balanceR kx x (go f k l) r
          IGT -> balanceL kx x l (go f k r)
          IEQ -> case f kx x of
            Just x' -> Bin sx kx x' l r
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

-- See Note: Type of local 'go' function
updateLookupWithKey :: IOrd k => (k x -> a x -> Maybe (a x)) -> k x -> Map k a -> (Maybe (a x), Map k a)
updateLookupWithKey = go
 where
   go :: IOrd k => (k x -> a x -> Maybe (a x)) -> k x -> Map k a -> (Maybe (a x), Map k a)
   go _ !_ Tip = (Nothing,Tip)
   go f !k (Bin sx kx x l r) =
     case icompare k kx of
       ILT -> let (found,l') = go f k l in (found,balanceR kx x l' r)
       IGT -> let (found,r') = go f k r in (found,balanceL kx x l r')
       IEQ -> case f kx x of
         Just x' -> (Just x', Bin sx kx x' l r)
         Nothing -> (Just x, glue l r)
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

-- See Note: Type of local 'go' function
alter :: forall k a x. IOrd k => (Maybe (a x) -> Maybe (a x)) -> k x -> Map k a -> Map k a
alter = go
  where
    go :: IOrd k => (Maybe (a x) -> Maybe (a x)) -> k x -> Map k a -> Map k a
    go f !k Tip = case f Nothing of
      Nothing -> Tip
      Just x  -> singleton k x
    go f !k (Bin sx kx x l r) = case icompare k kx of
      ILT -> balance kx x (go f k l) r
      IGT -> balance kx x l (go f k r)
      IEQ -> case f (Just x) of
        Just x' -> Bin sx kx x' l r
        Nothing -> glue l r
{-# INLINABLE alter #-}

{--------------------------------------------------------------------
  Indexing
--------------------------------------------------------------------}
-- | /O(log n)/. Return the /index/ of a key, which is its zero-based index in
-- the sequence sorted by keys. The index is a number from /0/ up to, but not
-- including, the 'size' of the map. Calls 'error' when the key is not
-- a 'member' of the map.
--
-- > findIndex 2 (fromList [(5,"a"), (3,"b")])    Error: element is not in the map
-- > findIndex 3 (fromList [(5,"a"), (3,"b")]) == 0
-- > findIndex 5 (fromList [(5,"a"), (3,"b")]) == 1
-- > findIndex 6 (fromList [(5,"a"), (3,"b")])    Error: element is not in the map

-- See Note: Type of local 'go' function
findIndex :: forall k x a. IOrd k => k x -> Map k a -> Int
findIndex = go 0
  where
    go :: Int -> k x -> Map k a -> Int
    go !_ !_ Tip  = error "Map.findIndex: element is not in the map"
    go !idx !k (Bin _ kx _ l r) = case icompare k kx of
      ILT -> go idx k l
      IGT -> go (idx + size l + 1) k r
      IEQ -> idx + size l
{-# INLINABLE findIndex #-}

-- | /O(log n)/. Lookup the /index/ of a key, which is its zero-based index in
-- the sequence sorted by keys. The index is a number from /0/ up to, but not
-- including, the 'size' of the map.
--
-- > isJust (lookupIndex 2 (fromList [(5,"a"), (3,"b")]))   == False
-- > fromJust (lookupIndex 3 (fromList [(5,"a"), (3,"b")])) == 0
-- > fromJust (lookupIndex 5 (fromList [(5,"a"), (3,"b")])) == 1
-- > isJust (lookupIndex 6 (fromList [(5,"a"), (3,"b")]))   == False

-- See Note: Type of local 'go' function
lookupIndex :: IOrd k => k x -> Map k a -> Maybe Int
lookupIndex = go 0
  where
    go :: IOrd k => Int -> k x -> Map k a -> Maybe Int
    go !_ !_ Tip  = Nothing
    go !idx !k (Bin _ kx _ l r) = case icompare k kx of
      ILT -> go idx k l
      IGT -> go (idx + size l + 1) k r
      IEQ -> Just $! idx + size l
{-# INLINABLE lookupIndex #-}

-- | /O(log n)/. Retrieve an element by its /index/, i.e. by its zero-based
-- index in the sequence sorted by keys. If the /index/ is out of range (less
-- than zero, greater or equal to 'size' of the map), 'error' is called.
--
-- > elemAt 0 (fromList [(5,"a"), (3,"b")]) == (3,"b")
-- > elemAt 1 (fromList [(5,"a"), (3,"b")]) == (5, "a")
-- > elemAt 2 (fromList [(5,"a"), (3,"b")])    Error: index out of range

elemAt :: Int -> Map k a -> Some2 k a
elemAt !_ Tip = error "Map.elemAt: index out of range"
elemAt !i (Bin _ kx x l r)
  = case compare i sizeL of
      LT -> elemAt i l
      GT -> elemAt (i-sizeL-1) r
      EQ -> Some2 kx x
  where
    sizeL = size l

-- | /O(log n)/. Update the element at /index/, i.e. by its zero-based index in
-- the sequence sorted by keys. If the /index/ is out of range (less than zero,
-- greater or equal to 'size' of the map), 'error' is called.
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
        Just x' -> Bin sx kx x' l r
        Nothing -> glue l r
     where
      sizeL = size l

-- | /O(log n)/. Delete the element at /index/, i.e. by its zero-based index in
-- the sequence sorted by keys. If the /index/ is out of range (less than zero,
-- greater or equal to 'size' of the map), 'error' is called.
--
-- > deleteAt 0  (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
-- > deleteAt 1  (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > deleteAt 2 (fromList [(5,"a"), (3,"b")])     Error: index out of range
-- > deleteAt (-1) (fromList [(5,"a"), (3,"b")])  Error: index out of range

deleteAt :: Int -> Map k a -> Map k a
deleteAt i t = i `seq`
  case t of
    Tip -> error "Map.deleteAt: index out of range"
    Bin _ kx x l r -> case compare i sizeL of
      LT -> balanceR kx x (deleteAt i l) r
      GT -> balanceL kx x l (deleteAt (i-sizeL-1) r)
      EQ -> glue l r
     where
      sizeL = size l


{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}
-- | /O(log n)/. The minimal key of the map. Calls 'error' if the map is empty.
--
-- > findMin (fromList [(5,"a"), (3,"b")]) == (3,"b")
-- > findMin empty                            Error: empty map has no minimal element

findMin :: Map k a -> Some2 k a
findMin (Bin _ kx x Tip _)  = Some2 kx x
findMin (Bin _ _  _ l _)    = findMin l
findMin Tip                 = error "Map.findMin: empty map has no minimal element"

-- | /O(log n)/. The maximal key of the map. Calls 'error' if the map is empty.
--
-- > findMax (fromList [(5,"a"), (3,"b")]) == (5,"a")
-- > findMax empty                            Error: empty map has no maximal element

findMax :: Map k a -> Some2 k a
findMax (Bin _ kx x _ Tip)  = Some2 kx x
findMax (Bin _ _  _ _ r)    = findMax r
findMax Tip                 = error "Map.findMax: empty map has no maximal element"

-- | /O(log n)/. Delete the minimal key. Returns an empty map if the map is empty.
--
-- > deleteMin (fromList [(5,"a"), (3,"b"), (7,"c")]) == fromList [(5,"a"), (7,"c")]
-- > deleteMin empty == empty

deleteMin :: Map k a -> Map k a
deleteMin (Bin _ _  _ Tip r)  = r
deleteMin (Bin _ kx x l r)    = balanceR kx x (deleteMin l) r
deleteMin Tip                 = Tip

-- | /O(log n)/. Delete the maximal key. Returns an empty map if the map is empty.
--
-- > deleteMax (fromList [(5,"a"), (3,"b"), (7,"c")]) == fromList [(3,"b"), (5,"a")]
-- > deleteMax empty == empty

deleteMax :: Map k a -> Map k a
deleteMax (Bin _ _  _ l Tip)  = l
deleteMax (Bin _ kx x l r)    = balanceL kx x l (deleteMax r)
deleteMax Tip                 = Tip

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
                                           Just x' -> Bin sx kx x' Tip r
updateMinWithKey f (Bin _ kx x l r)    = balanceR kx x (updateMinWithKey f l) r

-- | /O(log n)/. Update the value at the maximal key.
--
-- > updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"b"), (5,"5:a")]
-- > updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMaxWithKey :: (forall x. k x -> a x -> Maybe (a x)) -> Map k a -> Map k a
updateMaxWithKey _ Tip                 = Tip
updateMaxWithKey f (Bin sx kx x l Tip) = case f kx x of
                                           Nothing -> l
                                           Just x' -> Bin sx kx x' l Tip
updateMaxWithKey f (Bin _ kx x l r)    = balanceL kx x l (updateMaxWithKey f r)

-- | /O(log n)/. Retrieves the minimal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > minViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((3,"b"), singleton 5 "a")
-- > minViewWithKey empty == Nothing

minViewWithKey :: Map k a -> Maybe (Some2 k a, Map k a)
minViewWithKey Tip = Nothing
minViewWithKey x   = Just (deleteFindMin x)

-- | /O(log n)/. Retrieves the maximal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > maxViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((5,"a"), singleton 3 "b")
-- > maxViewWithKey empty == Nothing

maxViewWithKey :: Map k a -> Maybe (Some2 k a, Map k a)
maxViewWithKey Tip = Nothing
maxViewWithKey x   = Just (deleteFindMax x)

-- | /O(log n)/. Retrieves the value associated with minimal key of the
-- map, and the map stripped of that element, or 'Nothing' if passed an
-- empty map.
--
-- > minView (fromList [(5,"a"), (3,"b")]) == Just ("b", singleton 5 "a")
-- > minView empty == Nothing

minView :: Map k a -> Maybe (Some1 a, Map k a)
minView Tip = Nothing
minView x   = Just (first (\(Some2 _ y) -> Some1 y) $ deleteFindMin x)

-- | /O(log n)/. Retrieves the value associated with maximal key of the
-- map, and the map stripped of that element, or 'Nothing' if passed an
-- empty map.
--
-- > maxView (fromList [(5,"a"), (3,"b")]) == Just ("a", singleton 3 "b")
-- > maxView empty == Nothing

maxView :: Map k a -> Maybe (Some1 a, Map k a)
maxView Tip = Nothing
maxView x   = Just (first (\(Some2 _ y) -> Some1 y) $ deleteFindMax x)

-- Update the 1st component of a tuple (special case of Control.Arrow.first)
first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

{--------------------------------------------------------------------
  Union.
--------------------------------------------------------------------}
-- | The union of a list of maps:
--   (@'unions' == 'Prelude.foldl' 'union' 'empty'@).
--
-- > unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "b"), (5, "a"), (7, "C")]
-- > unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
-- >     == fromList [(3, "B3"), (5, "A3"), (7, "C")]

unions :: IOrd k => [Map k a] -> Map k a
unions ts
  = foldlStrict union empty ts
{-# INLINABLE unions #-}

-- | The union of a list of maps, with a combining operation:
--   (@'unionsWith' f == 'Prelude.foldl' ('unionWith' f) 'empty'@).
--
-- > unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]

unionsWith :: IOrd k => (forall x. a x -> a x -> a x) -> [Map k a] -> Map k a
unionsWith f ts
  = foldlStrict (unionWith f) empty ts
{-# INLINABLE unionsWith #-}

-- | /O(n+m)/.
-- The expression (@'union' t1 t2@) takes the left-biased union of @t1@ and @t2@.
-- It prefers @t1@ when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
-- The implementation uses the efficient /hedge-union/ algorithm.
--
-- > union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "a"), (7, "C")]

union :: IOrd k => Map k a -> Map k a -> Map k a
union Tip t2  = t2
union t1 Tip  = t1
union t1 t2 = hedgeUnion NothingS NothingS t1 t2
{-# INLINABLE union #-}

-- left-biased hedge union
hedgeUnion :: IOrd a => MaybeS (Some1 a) -> MaybeS (Some1 a) -> Map a b -> Map a b -> Map a b
hedgeUnion _   _   t1  Tip = t1
hedgeUnion blo bhi Tip (Bin _ kx x l r) = link kx x (filterGt blo l) (filterLt bhi r)
hedgeUnion _   _   t1  (Bin _ kx x Tip Tip) = insertR kx x t1  -- According to benchmarks, this special case increases
                                                              -- performance up to 30%. It does not help in difference or intersection.
hedgeUnion blo bhi (Bin _ kx x l r) t2 = link kx x (hedgeUnion blo bmi l (trim blo bmi t2))
                                                   (hedgeUnion bmi bhi r (trim bmi bhi t2))
  where bmi = JustS (Some1 kx)
{-# INLINABLE hedgeUnion #-}

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
-- | /O(n+m)/. Difference of two maps.
-- Return elements of the first map not existing in the second map.
-- The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
--
-- > difference (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 3 "b"

difference :: IOrd k => Map k a -> Map k b -> Map k a
difference Tip _   = Tip
difference t1 Tip  = t1
difference t1 t2   = hedgeDiff NothingS NothingS t1 t2
{-# INLINABLE difference #-}

hedgeDiff :: IOrd a => MaybeS (Some1 a) -> MaybeS (Some1 a) -> Map a b -> Map a c -> Map a b
hedgeDiff _   _   Tip              _ = Tip
hedgeDiff blo bhi (Bin _ kx x l r) Tip = link kx x (filterGt blo l) (filterLt bhi r)
hedgeDiff blo bhi t (Bin _ kx _ l r) = merge (hedgeDiff blo bmi (trim blo bmi t) l)
                                             (hedgeDiff bmi bhi (trim bmi bhi t) r)
  where bmi = JustS (Some1 kx)
{-# INLINABLE hedgeDiff #-}

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
-- | /O(n+m)/. Intersection of two maps.
-- Return data in the first map for the keys existing in both maps.
-- (@'intersection' m1 m2 == 'intersectionWith' 'const' m1 m2@).
-- The implementation uses an efficient /hedge/ algorithm comparable with
-- /hedge-union/.
--
-- > intersection (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "a"

intersection :: IOrd k => Map k a -> Map k b -> Map k a
intersection Tip _ = Tip
intersection _ Tip = Tip
intersection t1 t2 = hedgeInt NothingS NothingS t1 t2
{-# INLINABLE intersection #-}

hedgeInt :: IOrd k => MaybeS (Some1 k) -> MaybeS (Some1 k) -> Map k a -> Map k b -> Map k a
hedgeInt _ _ _   Tip = Tip
hedgeInt _ _ Tip _   = Tip
hedgeInt blo bhi (Bin _ kx x l r) t2 = let l' = hedgeInt blo bmi l (trim blo bmi t2)
                                           r' = hedgeInt bmi bhi r (trim bmi bhi t2)
                                       in if kx `member` t2 then link kx x l' r' else merge l' r'
  where bmi = JustS (Some1 kx)
{-# INLINABLE hedgeInt #-}

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
                                                               (Bin _ kx' x' Tip Tip) -> case ieq kx kx' of
                                                                   ITrue -> link kx x' l' r'
                                                                   IFalse -> error "mergeWithKey: Given function only1 does not fulfil required conditions (see documentation)"
                                                               _ -> error "mergeWithKey: Given function only1 does not fulfil required conditions (see documentation)"
                                                  Just x2 -> case f kx x x2 of
                                                               Nothing -> merge l' r'
                                                               Just x' -> link kx x' l' r'
      where bmi = JustS (Some1 kx)
{-# INLINE mergeWithKey #-}

{--------------------------------------------------------------------
  Submap
--------------------------------------------------------------------}
-- | /O(n+m)/.
-- This function is defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
--
isSubmapOf :: (IOrd k, IEq a) => Map k a -> Map k a -> Bool
isSubmapOf m1 m2 = isSubmapOfBy ieq m1 m2
{-# INLINABLE isSubmapOf #-}

{- | /O(n+m)/.
 The expression (@'isSubmapOfBy' f t1 t2@) returns 'True' if
 all keys in @t1@ are in tree @t2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following
 expressions are all 'True':

 > isSubmapOfBy (==) (fromList [('a',1)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (<=) (fromList [('a',1)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (==) (fromList [('a',1),('b',2)]) (fromList [('a',1),('b',2)])

 But the following are all 'False':

 > isSubmapOfBy (==) (fromList [('a',2)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (<)  (fromList [('a',1)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (==) (fromList [('a',1),('b',2)]) (fromList [('a',1)])


-}
isSubmapOfBy :: IOrd k => (forall x. a x -> b x -> IBool x x) -> Map k a -> Map k b -> Bool
isSubmapOfBy f t1 t2
  = (size t1 <= size t2) && (submap' f t1 t2)
{-# INLINABLE isSubmapOfBy #-}

submap' :: IOrd a => (forall x. b x -> c x -> IBool x x) -> Map a b -> Map a c -> Bool
submap' _ Tip _ = True
submap' _ _ Tip = False
submap' f (Bin _ kx x l r) t
  = case found of
      Nothing -> False
      Just y  -> toBool (f x y) && submap' f l lt && submap' f r gt
        where toBool :: IBool a b -> Bool
              toBool ITrue = True
              toBool (IFalse) = False
  where
    (lt,found,gt) = splitLookup kx t
{-# INLINABLE submap' #-}

-- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
-- Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' (==)@).
isProperSubmapOf :: (IOrd k, IEq a) => Map k a -> Map k a -> Bool
isProperSubmapOf m1 m2
  = isProperSubmapOfBy ieq m1 m2
{-# INLINABLE isProperSubmapOf #-}

{- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
 The expression (@'isProperSubmapOfBy' f m1 m2@) returns 'True' when
 @m1@ and @m2@ are not equal,
 all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following
 expressions are all 'True':

  > isProperSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isProperSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)])

 But the following are all 'False':

  > isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)])
  > isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)])
  > isProperSubmapOfBy (<)  (fromList [(1,1)])       (fromList [(1,1),(2,2)])


-}
isProperSubmapOfBy :: IOrd k => (forall x. a x -> b x -> IBool x x) -> Map k a -> Map k b -> Bool
isProperSubmapOfBy f t1 t2
  = (size t1 < size t2) && (submap' f t1 t2)
{-# INLINABLE isProperSubmapOfBy #-}

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}
-- | /O(n)/. Filter all values that satisfy the predicate.
--
-- > filter (> "a") (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > filter (> "x") (fromList [(5,"a"), (3,"b")]) == empty
-- > filter (< "a") (fromList [(5,"a"), (3,"b")]) == empty

filter :: (forall x. a x -> Bool) -> Map k a -> Map k a
filter p m
  = filterWithKey (\_ x -> p x) m

-- | /O(n)/. Filter all keys\/values that satisfy the predicate.
--
-- > filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

filterWithKey :: (forall x. k x -> a x -> Bool) -> Map k a -> Map k a
filterWithKey _ Tip = Tip
filterWithKey p (Bin _ kx x l r)
  | p kx x    = link kx x (filterWithKey p l) (filterWithKey p r)
  | otherwise = merge (filterWithKey p l) (filterWithKey p r)

-- | /O(n)/. Partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partition (> "a") (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > partition (< "x") (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partition (> "x") (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])

partition :: (forall x. a x -> Bool) -> Map k a -> (Map k a, Map k a)
partition p m
  = partitionWithKey (\_ x -> p x) m

-- | /O(n)/. Partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (3,"b")]) == (singleton 5 "a", singleton 3 "b")
-- > partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])

partitionWithKey :: (forall x. k x -> a x -> Bool) -> Map k a -> (Map k a, Map k a)
partitionWithKey p0 t0 = toPair $ go p0 t0
  where
    go :: (forall x. k x -> a x -> Bool) -> Map k a -> StrictPair (Map k a) (Map k a)
    go _ Tip = (Tip :*: Tip)
    go p (Bin _ kx x l r)
      | p kx x    = link kx x l1 r1 :*: merge l2 r2
      | otherwise = merge l1 r1 :*: link kx x l2 r2
      where
        (l1 :*: l2) = go p l
        (r1 :*: r2) = go p r

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
  Just y  -> link kx y (mapMaybeWithKey f l) (mapMaybeWithKey f r)
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
      Left y  -> link kx y l1 r1 :*: merge l2 r2
      Right z -> merge l1 r1 :*: link kx z l2 r2
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
map f (Bin sx kx x l r) = Bin sx kx (f x) (map f l) (map f r)
{-

These rules were borrowed from containers but broke with the indexing.

{-# NOINLINE [1] map #-}
{-# RULES
"map/map" forall f g xs . map f (map g xs) = map (f . g) xs
 #-}
-- Safe coercions were introduced in 7.8, but did not work well with RULES yet.
{-# RULES
"map/coerce" map coerce = coerce
 #-}

-}

-- | /O(n)/. Map a function over all values in the map.
--
-- > let f key x = (show key) ++ ":" ++ x
-- > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]

mapWithKey :: (forall x. k x -> a x -> b x) -> Map k a -> Map k b
mapWithKey _ Tip = Tip
mapWithKey f (Bin sx kx x l r) = Bin sx kx (f kx x) (mapWithKey f l) (mapWithKey f r)

{-

These rules were borrowed from containers but broke with the indexing.

{-# NOINLINE [1] mapWithKey #-}
{-# RULES
"mapWithKey/mapWithKey" forall f g xs . mapWithKey f (mapWithKey g xs) =
  mapWithKey (\k a -> f k (g k a)) xs
"mapWithKey/map" forall f g xs . mapWithKey f (map g xs) =
  mapWithKey (\k a -> f k (g a)) xs
"map/mapWithKey" forall f g xs . map f (mapWithKey g xs) =
  mapWithKey (\k a -> f (g k a)) xs
 #-}

-}

-- | /O(n)/.
-- @'traverseWithKey' f m == 'fromList' <$> 'traverse' (\(k, v) -> (,) k <$> f k v) ('toList' m)@
-- That is, behaves exactly like a regular 'traverse' except that the traversing
-- function also has access to the key associated with a value.
--
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(1, 'a'), (5, 'e')]) == Just (fromList [(1, 'b'), (5, 'f')])
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(2, 'c')])           == Nothing
traverseWithKey :: Applicative t => (forall x. k x -> a x -> t (b x)) -> Map k a -> t (Map k b)
traverseWithKey f = go
  where
    go Tip = pure Tip
    go (Bin 1 k v _ _) = (\v' -> Bin 1 k v' Tip Tip) <$> f k v
    go (Bin s k v l r) = flip (Bin s k) <$> go l <*> f k v <*> go r
{-# INLINE traverseWithKey #-}

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
  in (a3,Bin sx kx x' l' r')

-- | /O(n)/. The function 'mapAccumR' threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (forall x. a -> k x -> b x -> (a, c x)) -> a -> Map k b -> (a, Map k c)
mapAccumRWithKey _ a Tip = (a,Tip)
mapAccumRWithKey f a (Bin sx kx x l r) =
  let (a1,r') = mapAccumRWithKey f a r
      (a2,x') = f a1 kx x
      (a3,l') = mapAccumRWithKey f a2 l
  in (a3,Bin sx kx x' l' r')

-- | /O(n*log n)/.
-- @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the value at the greatest of the
-- original keys is retained.
--
-- > mapKeys (+ 1) (fromList [(5,"a"), (3,"b")])                        == fromList [(4, "b"), (6, "a")]
-- > mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "c"
-- > mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "c"

mapKeys :: IOrd k2 => (forall x. k1 x -> k2 x) -> Map k1 a -> Map k2 a
mapKeys f = fromList . foldrWithKey (\k x xs -> Some2 (f k) x : xs) []
{-# INLINABLE mapKeys #-}

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
mapKeysWith c f = fromListWith c . foldrWithKey (\k x xs -> Some2 (f k) x : xs) []
{-# INLINABLE mapKeysWith #-}


-- | /O(n)/.
-- @'mapKeysMonotonic' f s == 'mapKeys' f s@, but works only when @f@
-- is strictly monotonic.
-- That is, for any values @x@ and @y@, if @x@ < @y@ then @f x@ < @f y@.
-- /The precondition is not checked./
-- Semi-formally, we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapKeysMonotonic f s == mapKeys f s
-- >     where ls = keys s
--
-- This means that @f@ maps distinct original keys to distinct resulting keys.
-- This function has better performance than 'mapKeys'.
--
-- > mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")]) == fromList [(6, "b"), (10, "a")]
-- > valid (mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")])) == True
-- > valid (mapKeysMonotonic (\ _ -> 1)     (fromList [(5,"a"), (3,"b")])) == False

mapKeysMonotonic :: (forall x. k1 x -> k2 x) -> Map k1 a -> Map k2 a
mapKeysMonotonic _ Tip = Tip
mapKeysMonotonic f (Bin sz k x l r) =
    Bin sz (f k) x (mapKeysMonotonic f l) (mapKeysMonotonic f r)

{--------------------------------------------------------------------
  Folds
--------------------------------------------------------------------}

-- | /O(n)/. Fold the values in the map using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
--
-- For example,
--
-- > elems map = foldr (:) [] map
--
-- > let f a len = len + (length a)
-- > foldr f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
foldr :: (forall x. a x -> b -> b) -> b -> Map k a -> b
foldr f z = go z
  where
    go z' Tip             = z'
    go z' (Bin _ _ x l r) = go (f x (go z' r)) l
{-# INLINE foldr #-}

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (forall x. a x -> b -> b) -> b -> Map k a -> b
foldr' f z = go z
  where
    go !z' Tip             = z'
    go !z' (Bin _ _ x l r) = go (f x (go z' r)) l
{-# INLINE foldr' #-}

-- | /O(n)/. Fold the values in the map using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'elems'@.
--
-- For example,
--
-- > elems = reverse . foldl (flip (:)) []
--
-- > let f len a = len + (length a)
-- > foldl f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
foldl :: (forall x. a -> b x -> a) -> a -> Map k b -> a
foldl f z = go z
  where
    go z' Tip             = z'
    go z' (Bin _ _ x l r) = go (f (go z' l) x) r
{-# INLINE foldl #-}

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (forall x. a -> b x -> a) -> a -> Map k b -> a
foldl' f z = go z
  where
    go !z' Tip             = z'
    go !z' (Bin _ _ x l r) = go (f (go z' l) x) r
{-# INLINE foldl' #-}

-- | /O(n)/. Fold the keys and values in the map using the given right-associative
-- binary operator, such that
-- @'foldrWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
--
-- For example,
--
-- > keys map = foldrWithKey (\k x ks -> k:ks) [] map
--
-- > let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldrWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (5:a)(3:b)"
foldrWithKey :: (forall x. k x -> a x -> b -> b) -> b -> Map k a -> b
foldrWithKey f z = go z
  where
    go z' Tip             = z'
    go z' (Bin _ kx x l r) = go (f kx x (go z' r)) l
{-# INLINE foldrWithKey #-}

-- | /O(n)/. A strict version of 'foldrWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldrWithKey' :: (forall x. k x -> a x -> b -> b) -> b -> Map k a -> b
foldrWithKey' f z = go z
  where
    go !z' Tip              = z'
    go !z' (Bin _ kx x l r) = go (f kx x (go z' r)) l
{-# INLINE foldrWithKey' #-}

-- | /O(n)/. Fold the keys and values in the map using the given left-associative
-- binary operator, such that
-- @'foldlWithKey' f z == 'Prelude.foldl' (\\z' (kx, x) -> f z' kx x) z . 'toAscList'@.
--
-- For example,
--
-- > keys = reverse . foldlWithKey (\ks k x -> k:ks) []
--
-- > let f result k a = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldlWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (3:b)(5:a)"
foldlWithKey :: (forall x. a -> k x -> b x -> a) -> a -> Map k b -> a
foldlWithKey f z = go z
  where
    go z' Tip              = z'
    go z' (Bin _ kx x l r) = go (f (go z' l) kx x) r
{-# INLINE foldlWithKey #-}

-- | /O(n)/. A strict version of 'foldlWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldlWithKey' :: (forall x. a -> k x -> b x -> a) -> a -> Map k b -> a
foldlWithKey' f z = go z
  where
    go !z' Tip              = z'
    go !z' (Bin _ kx x l r) = go (f (go z' l) kx x) r
{-# INLINE foldlWithKey' #-}

-- | /O(n)/. Fold the keys and values in the map using the given monoid, such that
--
-- @'foldMapWithKey' f = 'Prelude.fold' . 'mapWithKey' f@
--
-- This can be an asymptotically faster than 'foldrWithKey' or 'foldlWithKey' for some monoids.
foldMapWithKey :: Monoid m => (forall x. k x -> a x -> m) -> Map k a -> m
foldMapWithKey f = go
  where
    go Tip             = mempty
    go (Bin 1 k v _ _) = f k v
    go (Bin _ k v l r) = go l `mappend` (f k v `mappend` go r)
{-# INLINE foldMapWithKey #-}

{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}
-- | /O(n)/.
-- Return all elements of the map in the ascending order of their keys.
-- Subject to list fusion.
--
-- > elems (fromList [(5,"a"), (3,"b")]) == ["b","a"]
-- > elems empty == []

elems :: Map k a -> [Some1 a]
elems = foldr (\x xs -> Some1 x : xs) []

-- | /O(n)/. Return all keys of the map in ascending order. Subject to list
-- fusion.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys empty == []

keys  :: Map k a -> [Some1 k]
keys = foldrWithKey (\k _ ks -> Some1 k : ks) []

-- | /O(n)/. An alias for 'toAscList'. Return all key\/value pairs in the map
-- in ascending key order. Subject to list fusion.
--
-- > assocs (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > assocs empty == []

assocs :: Map k a -> [Some2 k a]
assocs m
  = toAscList m

-- | /O(n)/. The set of all keys of the map.
--
-- > keysSet (fromList [(5,"a"), (3,"b")]) == Data.Set.fromList [3,5]
-- > keysSet empty == Data.Set.empty

keysSet :: Map k a -> Set.Set k
keysSet Tip = Set.Tip
keysSet (Bin sz kx _ l r) = Set.Bin sz kx (keysSet l) (keysSet r)

-- | /O(n)/. Build a map from a set of keys and a function which for each key
-- computes its value.
--
-- > fromSet (\k -> replicate k 'a') (Data.Set.fromList [3, 5]) == fromList [(5,"aaaaa"), (3,"aaa")]
-- > fromSet undefined Data.Set.empty == empty

fromSet :: (forall x. k x -> a x) -> Set.Set k -> Map k a
fromSet _ Set.Tip = Tip
fromSet f (Set.Bin sz x l r) = Bin sz x (f x) (fromSet f l) (fromSet f r)

{--------------------------------------------------------------------
  Lists
  use [foldlStrict] to reduce demand on the control-stack
--------------------------------------------------------------------}
instance IOrd k => GHCExts.IsList (Map k v) where
  type Item (Map k v) = Some2 k v
  fromList = fromList
  toList   = toList

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
fromList [Some2 kx x] = Bin 1 kx x Tip Tip
fromList (Some2 kx0 x0 : xs0) | not_ordered kx0 xs0 = fromList' (Bin 1 kx0 x0 Tip Tip) xs0
                              | otherwise = go (1::Int) (Bin 1 kx0 x0 Tip Tip) xs0
  where
    not_ordered _ [] = False
    not_ordered kx (Some2 ky _ : _) = case icompare kx ky of
        ILT -> False
        _     -> True
    {-# INLINE not_ordered #-}

    fromList' t0 xs = foldlStrict ins t0 xs
      where ins t (Some2 k x) = insert k x t

    go !_ t [] = t
    go !_ t [Some2 kx x] = insertMax kx x t
    go !s l xs@(Some2 kx x : xss) | not_ordered kx xss = fromList' l xs
                                  | otherwise = case create s xss of
                                      (r, ys, []) -> go (s `shiftL` 1) (link kx x l r) ys
                                      (r, _,  ys) -> fromList' (link kx x l r) ys

    -- The create is returning a triple (tree, xs, ys). Both xs and ys
    -- represent not yet processed elements and only one of them can be nonempty.
    -- If ys is nonempty, the keys in ys are not ordered with respect to tree
    -- and must be inserted using fromList'. Otherwise the keys have been
    -- ordered so far.
    create !_ [] = (Tip, [], [])
    create !s xs@(xp : xss)
      | s == 1 = case xp of (Some2 kx x) | not_ordered kx xss -> (Bin 1 kx x Tip Tip, [], xss)
                                         | otherwise -> (Bin 1 kx x Tip Tip, xss, [])
      | otherwise = case create (s `shiftR` 1) xs of
                      res@(_, [], _) -> res
                      (l, [Some2 ky y], zs) -> (insertMax ky y l, [], zs)
                      (l, ys@(Some2 ky y:yss), _) | not_ordered ky yss -> (l, [], ys)
                                                  | otherwise -> case create (s `shiftR` 1) yss of
                                                      (r, zs, ws) -> (link ky y l r, zs, ws)
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

fromListWithKey :: IOrd k => (forall x. k x -> a x -> a x -> a x) -> [Some2 k a] -> Map k a
fromListWithKey f xs
  = foldlStrict ins empty xs
  where
    ins t (Some2 k x) = insertWithKey f k x t
{-# INLINABLE fromListWithKey #-}

-- | /O(n)/. Convert the map to a list of key\/value pairs. Subject to list fusion.
--
-- > toList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > toList empty == []

toList :: Map k a -> [Some2 k a]
toList = toAscList

-- | /O(n)/. Convert the map to a list of key\/value pairs where the keys are
-- in ascending order. Subject to list fusion.
--
-- > toAscList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]

toAscList :: Map k a -> [Some2 k a]
toAscList = foldrWithKey (\k x xs -> Some2 k x : xs) []

-- | /O(n)/. Convert the map to a list of key\/value pairs where the keys
-- are in descending order. Subject to list fusion.
--
-- > toDescList (fromList [(5,"a"), (3,"b")]) == [(5,"a"), (3,"b")]

toDescList :: Map k a -> [Some2 k a]
toDescList = foldlWithKey (\xs k x -> Some2 k x : xs) []

-- List fusion for the list generating functions.
-- The foldrFB and foldlFB are fold{r,l}WithKey equivalents, used for list fusion.
-- They are important to convert unfused methods back, see mapFB in prelude.
foldrFB :: (forall x. k x -> a x -> b -> b) -> b -> Map k a -> b
foldrFB = foldrWithKey
{-# INLINE[0] foldrFB #-}
foldlFB :: (forall x. a -> k x -> b x -> a) -> a -> Map k b -> a
foldlFB = foldlWithKey
{-# INLINE[0] foldlFB #-}

-- Inline assocs and toList, so that we need to fuse only toAscList.
{-# INLINE assocs #-}
{-# INLINE toList #-}

-- The fusion is enabled up to phase 2 included. If it does not succeed,
-- convert in phase 1 the expanded elems,keys,to{Asc,Desc}List calls back to
-- elems,keys,to{Asc,Desc}List.  In phase 0, we inline fold{lr}FB (which were
-- used in a list fusion, otherwise it would go away in phase 1), and let compiler
-- do whatever it wants with elems,keys,to{Asc,Desc}List -- it was forbidden to
-- inline it before phase 0, otherwise the fusion rules would not fire at all.
{-# NOINLINE[0] elems #-}
{-# NOINLINE[0] keys #-}
{-# NOINLINE[0] toAscList #-}
{-# NOINLINE[0] toDescList #-}
{-# RULES "Map.elems" [~1] forall m . elems m = build (\c n -> foldrFB (\_ x xs -> c (Some1 x) xs) n m) #-}
{-# RULES "Map.elemsBack" [1] foldrFB (\_ x xs -> Some1 x : xs) [] = elems #-}
{-# RULES "Map.keys" [~1] forall m . keys m = build (\c n -> foldrFB (\k _ xs -> c (Some1 k) xs) n m) #-}
{-# RULES "Map.keysBack" [1] foldrFB (\k _ xs -> Some1 k : xs) [] = keys #-}
{-# RULES "Map.toAscList" [~1] forall m . toAscList m = build (\c n -> foldrFB (\k x xs -> c (Some2 k x) xs) n m) #-}
{-# RULES "Map.toAscListBack" [1] foldrFB (\k x xs -> Some2 k x : xs) [] = toAscList #-}
{-# RULES "Map.toDescList" [~1] forall m . toDescList m = build (\c n -> foldlFB (\xs k x -> c (Some2 k x) xs) n m) #-}
{-# RULES "Map.toDescListBack" [1] foldlFB (\xs k x -> Some2 k x : xs) [] = toDescList #-}

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

fromAscList :: IEq k => [Some2 k a] -> Map k a
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

fromAscListWithKey :: IEq k => (forall x. k x -> a x -> a x -> a x) -> [Some2 k a] -> Map k a
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
  combineEq' z@(Some2 kz zz) (x@(Some2 kx xx):xs') = case ieq kx kz of
      ITrue -> let yy = f kx xx zz in combineEq' (Some2 kx yy) xs'
      IFalse -> z:combineEq' x xs'
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
fromDistinctAscList ((Some2 kx0 x0) : xs0) = go (1::Int) (Bin 1 kx0 x0 Tip Tip) xs0
  where
    go !_ t [] = t
    go !s l (Some2 kx x : xs) = case create s xs of
                               (r, ys) -> go (s `shiftL` 1) (link kx x l r) ys

    create !_ [] = (Tip, [])
    create !s xs@(x' : xs')
      | s == 1 = case x' of Some2 kx x -> (Bin 1 kx x Tip Tip, xs')
      | otherwise = case create (s `shiftR` 1) xs of
                      res@(_, []) -> res
                      (l, Some2 ky y:ys) -> case create (s `shiftR` 1) ys of
                        (r, zs) -> (link ky y l r, zs)


{--------------------------------------------------------------------
  Utility functions that return sub-ranges of the original
  tree. Some functions take a `Maybe value` as an argument to
  allow comparisons against infinite values. These are called `blow`
  (Nothing is -\infty) and `bhigh` (here Nothing is +\infty).
  We use MaybeS value, which is a Maybe strict in the Just case.

  [trim blow bhigh t]   A tree that is either empty or where [x > blow]
                        and [x < bhigh] for the value [x] of the root.
  [filterGt blow t]     A tree where for all values [k]. [k > blow]
  [filterLt bhigh t]    A tree where for all values [k]. [k < bhigh]

  [split k t]           Returns two trees [l] and [r] where all keys
                        in [l] are <[k] and all keys in [r] are >[k].
  [splitLookup k t]     Just like [split] but also returns whether [k]
                        was found in the tree.
--------------------------------------------------------------------}

data MaybeS a = NothingS | JustS !a

{--------------------------------------------------------------------
  [trim blo bhi t] trims away all subtrees that surely contain no
  values between the range [blo] to [bhi]. The returned tree is either
  empty or the key of the root is between @blo@ and @bhi@.
--------------------------------------------------------------------}
trim :: IOrd k => MaybeS (Some1 k) -> MaybeS (Some1 k) -> Map k a -> Map k a
trim NothingS   NothingS   t = t
trim (JustS (Some1 lk)) NothingS   t = greater lk t 
    where greater lo (Bin _ k _ _ r) = case icompare k lo of
            IGT -> error "trim.greater precondition failed"
            _     -> greater lo r
          greater _ t' = t'
trim NothingS (JustS (Some1 hk)) t = lesser hk t 
    where lesser hi (Bin _ k _ l _) = case icompare k hi of
            ILT -> error "trim.lesser precondition failed"
            _     -> lesser hi l
          lesser _ t' = t'
trim (JustS (Some1 lk)) (JustS (Some1 hk)) t = middle lk hk t  
    where middle lo hi (Bin _ k _ _ r) | (ILT) <- icompare k lo = middle lo hi r
                                       | IEQ <- icompare k lo = middle lo hi r
          middle lo hi (Bin _ k _ l _) | (IGT) <- icompare k hi = middle lo hi l
                                       | IEQ <- icompare k hi = middle lo hi l
                                       | otherwise = error "trim.middle precondition failed"
          middle _ _ t' = t'
{-# INLINABLE trim #-}

-- Helper function for 'mergeWithKey'. The @'trimLookupLo' lk hk t@ performs both
-- @'trim' (JustS lk) hk t@ and @'lookup' lk t@.

-- See Note: Type of local 'go' function
trimLookupLo :: IOrd k => k x -> MaybeS (Some1 k) -> Map k a -> (Maybe (a x), Map k a)
trimLookupLo lk0 mhk0 t0 = toPair $ go lk0 mhk0 t0
  where
    go lk NothingS t = greater lk t
      where greater :: IOrd k => k x -> Map k a -> StrictPair (Maybe (a x)) (Map k a)
            greater lo t'@(Bin _ kx x l r) = case icompare lo kx of
                ILT -> lookup lo l :*: t'
                IEQ -> (Just x :*: r)
                IGT -> greater lo r
            greater _ Tip = (Nothing :*: Tip)
    go lk (JustS (Some1 hk)) t = middle lk hk t
      where middle :: IOrd k => k x -> k y -> Map k a -> StrictPair (Maybe (a x)) (Map k a)
            middle lo hi t'@(Bin _ kx x l r) = case icompare lo kx of
                ILT -> case icompare kx hi of
                  ILT -> lookup lo l :*: t'
                  IEQ -> middle lo hi l
                  IGT -> middle lo hi l
                IEQ -> Just x :*: lesser hi r
                IGT -> middle lo hi r
            middle _ _ Tip = (Nothing :*: Tip)

            lesser :: IOrd k => k x -> Map k a -> Map k a
            lesser hi (Bin _ k _ l _) = case icompare k hi of
              ILT -> error "trimLookupLo.lesser invariant failed"
              _     -> lesser hi l
            lesser _ t' = t'
{-# INLINABLE trimLookupLo #-}

{--------------------------------------------------------------------
  [filterGt b t] filter all keys >[b] from tree [t]
  [filterLt b t] filter all keys <[b] from tree [t]
--------------------------------------------------------------------}
filterGt :: IOrd k => MaybeS (Some1 k) -> Map k v -> Map k v
filterGt NothingS t = t
filterGt (JustS (Some1 b)) t = filter' b t
  where filter' _   Tip = Tip
        filter' b' (Bin _ kx x l r) =
          case icompare b' kx of 
            ILT -> link kx x (filter' b' l) r
            IEQ -> r
            IGT -> filter' b' r
{-# INLINABLE filterGt #-}

filterLt :: IOrd k => MaybeS (Some1 k) -> Map k v -> Map k v
filterLt NothingS t = t
filterLt (JustS (Some1 b)) t = filter' b t
  where filter' _   Tip = Tip
        filter' b' (Bin _ kx x l r) =
          case icompare kx b' of ILT -> link kx x l (filter' b' r)
                                 IEQ -> l
                                 IGT -> filter' b' l
{-# INLINABLE filterLt #-}

{--------------------------------------------------------------------
  Split
--------------------------------------------------------------------}
-- | /O(log n)/. The expression (@'split' k map@) is a pair @(map1,map2)@ where
-- the keys in @map1@ are smaller than @k@ and the keys in @map2@ larger than @k@.
-- Any key equal to @k@ is found in neither @map1@ nor @map2@.
--
-- > split 2 (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3,"b"), (5,"a")])
-- > split 3 (fromList [(5,"a"), (3,"b")]) == (empty, singleton 5 "a")
-- > split 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > split 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", empty)
-- > split 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], empty)

split :: IOrd k => k x -> Map k a -> (Map k a, Map k a)
split k0 t0 = k0 `seq` toPair $ go k0 t0
  where
    go k t =
      case t of
        Tip            -> (Tip :*: Tip)
        Bin _ kx x l r -> case icompare k kx of
          ILT -> let (lt :*: gt) = go k l in lt :*: link kx x gt r
          IGT -> let (lt :*: gt) = go k r in link kx x l lt :*: gt
          IEQ -> (l :*: r)
{-# INLINABLE split #-}

-- | /O(log n)/. The expression (@'splitLookup' k map@) splits a map just
-- like 'split' but also returns @'lookup' k map@.
--
-- > splitLookup 2 (fromList [(5,"a"), (3,"b")]) == (empty, Nothing, fromList [(3,"b"), (5,"a")])
-- > splitLookup 3 (fromList [(5,"a"), (3,"b")]) == (empty, Just "b", singleton 5 "a")
-- > splitLookup 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Nothing, singleton 5 "a")
-- > splitLookup 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Just "a", empty)
-- > splitLookup 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], Nothing, empty)

splitLookup :: IOrd k => k x -> Map k a -> (Map k a, Maybe (a x), Map k a)
splitLookup k t = k `seq`
  case t of
    Tip            -> (Tip,Nothing,Tip)
    Bin _ kx x l r -> case icompare k kx of
      ILT -> let (lt,z,gt) = splitLookup k l
                 gt' = link kx x gt r
             in gt' `seq` (lt,z,gt')
      IGT -> let (lt,z,gt) = splitLookup k r
                 lt' = link kx x l lt
             in lt' `seq` (lt',z,gt)
      IEQ -> (l, Just x, r)
{-# INLINABLE splitLookup #-}

{--------------------------------------------------------------------
  Utility functions that maintain the balance properties of the tree.
  All constructors assume that all values in [l] < [k] and all values
  in [r] > [k], and that [l] and [r] are valid trees.

  In order of sophistication:
    [Bin sz k x l r]  The type constructor.
    [bin k x l r]     Maintains the correct size, assumes that both [l]
                      and [r] are balanced with respect to each other.
    [balance k x l r] Restores the balance and size.
                      Assumes that the original tree was balanced and
                      that [l] or [r] has changed by at most one element.
    [link k x l r]    Restores balance and size.

  Furthermore, we can construct a new tree from two trees. Both operations
  assume that all values in [l] < all values in [r] and that [l] and [r]
  are valid:
    [glue l r]        Glues [l] and [r] together. Assumes that [l] and
                      [r] are already balanced with respect to each other.
    [merge l r]       Merges two trees and restores balance.

  Note: in contrast to Adam's paper, we use (<=) comparisons instead
  of (<) comparisons in [link], [merge] and [balance].
  Quickcheck (on [difference]) showed that this was necessary in order
  to maintain the invariants. It is quite unsatisfactory that I haven't
  been able to find out why this is actually the case! Fortunately, it
  doesn't hurt to be a bit more conservative.
--------------------------------------------------------------------}

{--------------------------------------------------------------------
  Link
--------------------------------------------------------------------}
link :: forall k a x. k x -> a x -> Map k a -> Map k a -> Map k a
link kx x Tip r  = insertMin kx x r
link kx x l Tip  = insertMax kx x l
link kx x l@(Bin sizeL ky y ly ry) r@(Bin sizeR kz z lz rz)
  | delta*sizeL < sizeR  = balanceL kz z (link kx x l lz) rz
  | delta*sizeR < sizeL  = balanceR ky y ly (link kx x ry r)
  | otherwise            = bin kx x l r


-- insertMin and insertMax don't perform potentially expensive comparisons.
insertMax,insertMin :: k x -> a x -> Map k a -> Map k a
insertMax kx x t
  = case t of
      Tip -> singleton kx x
      Bin _ ky y l r
          -> balanceR ky y l (insertMax kx x r)

insertMin kx x t
  = case t of
      Tip -> singleton kx x
      Bin _ ky y l r
          -> balanceL ky y (insertMin kx x l) r

{--------------------------------------------------------------------
  [merge l r]: merges two trees.
--------------------------------------------------------------------}
merge :: Map k a -> Map k a -> Map k a
merge Tip r   = r
merge l Tip   = l
merge l@(Bin sizeL kx x lx rx) r@(Bin sizeR ky y ly ry)
  | delta*sizeL < sizeR = balanceL ky y (merge l ly) ry
  | delta*sizeR < sizeL = balanceR kx x lx (merge rx r)
  | otherwise           = glue l r

{--------------------------------------------------------------------
  [glue l r]: glues two trees together.
  Assumes that [l] and [r] are already balanced with respect to each other.
--------------------------------------------------------------------}
glue :: Map k a -> Map k a -> Map k a
glue Tip r = r
glue l Tip = l
glue l r
  | size l > size r = case deleteFindMax l of (Some2 km m, l') -> balanceR km m l' r
  | otherwise       = case deleteFindMin r of (Some2 km m, r') -> balanceL km m l r'


-- | /O(log n)/. Delete and find the minimal element.
--
-- > deleteFindMin (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((3,"b"), fromList[(5,"a"), (10,"c")])
-- > deleteFindMin                                            Error: can not return the minimal element of an empty map

deleteFindMin :: Map k a -> (Some2 k a,Map k a)
deleteFindMin t
  = case t of
      Bin _ k x Tip r -> (Some2 k x,r)
      Bin _ k x l r   -> let (km, l') = deleteFindMin l in (km,balanceR k x l' r)
      Tip             -> (error "Map.deleteFindMin: can not return the minimal element of an empty map", Tip)

-- | /O(log n)/. Delete and find the maximal element.
--
-- > deleteFindMax (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((10,"c"), fromList [(3,"b"), (5,"a")])
-- > deleteFindMax empty                                      Error: can not return the maximal element of an empty map

deleteFindMax :: Map k a -> (Some2 k a, Map k a)
deleteFindMax t
  = case t of
      Bin _ k x l Tip -> (Some2 k x,l)
      Bin _ k x l r   -> let (km, r') = deleteFindMax r in (km,balanceL k x l r')
      Tip             -> (error "Map.deleteFindMax: can not return the maximal element of an empty map", Tip)


{--------------------------------------------------------------------
  [balance l x r] balances two trees with value x.
  The sizes of the trees should balance after decreasing the
  size of one of them. (a rotation).

  [delta] is the maximal relative difference between the sizes of
          two trees, it corresponds with the [w] in Adams' paper.
  [ratio] is the ratio between an outer and inner sibling of the
          heavier subtree in an unbalanced setting. It determines
          whether a double or single rotation should be performed
          to restore balance. It is corresponds with the inverse
          of $\alpha$ in Adam's article.

  Note that according to the Adam's paper:
  - [delta] should be larger than 4.646 with a [ratio] of 2.
  - [delta] should be larger than 3.745 with a [ratio] of 1.534.

  But the Adam's paper is erroneous:
  - It can be proved that for delta=2 and delta>=5 there does
    not exist any ratio that would work.
  - Delta=4.5 and ratio=2 does not work.

  That leaves two reasonable variants, delta=3 and delta=4,
  both with ratio=2.

  - A lower [delta] leads to a more 'perfectly' balanced tree.
  - A higher [delta] performs less rebalancing.

  In the benchmarks, delta=3 is faster on insert operations,
  and delta=4 has slightly better deletes. As the insert speedup
  is larger, we currently use delta=3.

--------------------------------------------------------------------}
delta,ratio :: Int
delta = 3
ratio = 2

-- The balance function is equivalent to the following:
--
--   balance :: k -> a -> Map k a -> Map k a -> Map k a
--   balance k x l r
--     | sizeL + sizeR <= 1    = Bin sizeX k x l r
--     | sizeR > delta*sizeL   = rotateL k x l r
--     | sizeL > delta*sizeR   = rotateR k x l r
--     | otherwise             = Bin sizeX k x l r
--     where
--       sizeL = size l
--       sizeR = size r
--       sizeX = sizeL + sizeR + 1
--
--   rotateL :: a -> b -> Map a b -> Map a b -> Map a b
--   rotateL k x l r@(Bin _ _ _ ly ry) | size ly < ratio*size ry = singleL k x l r
--                                     | otherwise               = doubleL k x l r
--
--   rotateR :: a -> b -> Map a b -> Map a b -> Map a b
--   rotateR k x l@(Bin _ _ _ ly ry) r | size ry < ratio*size ly = singleR k x l r
--                                     | otherwise               = doubleR k x l r
--
--   singleL, singleR :: a -> b -> Map a b -> Map a b -> Map a b
--   singleL k1 x1 t1 (Bin _ k2 x2 t2 t3)  = bin k2 x2 (bin k1 x1 t1 t2) t3
--   singleR k1 x1 (Bin _ k2 x2 t1 t2) t3  = bin k2 x2 t1 (bin k1 x1 t2 t3)
--
--   doubleL, doubleR :: a -> b -> Map a b -> Map a b -> Map a b
--   doubleL k1 x1 t1 (Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4) = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
--   doubleR k1 x1 (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) t4 = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
--
-- It is only written in such a way that every node is pattern-matched only once.

balance :: k x -> a x -> Map k a -> Map k a -> Map k a
balance k x l r = case l of
  Tip -> case r of
           Tip -> Bin 1 k x Tip Tip
           (Bin _ _ _ Tip Tip) -> Bin 2 k x Tip r
           (Bin _ rk rx Tip rr@(Bin _ _ _ _ _)) -> Bin 3 rk rx (Bin 1 k x Tip Tip) rr
           (Bin _ rk rx (Bin _ rlk rlx _ _) Tip) -> Bin 3 rlk rlx (Bin 1 k x Tip Tip) (Bin 1 rk rx Tip Tip)
           (Bin rs rk rx rl@(Bin rls rlk rlx rll rlr) rr@(Bin rrs _ _ _ _))
             | rls < ratio*rrs -> Bin (1+rs) rk rx (Bin (1+rls) k x Tip rl) rr
             | otherwise -> Bin (1+rs) rlk rlx (Bin (1+size rll) k x Tip rll) (Bin (1+rrs+size rlr) rk rx rlr rr)

  (Bin ls lk lx ll lr) -> case r of
           Tip -> case (ll, lr) of
                    (Tip, Tip) -> Bin 2 k x l Tip
                    (Tip, (Bin _ lrk lrx _ _)) -> Bin 3 lrk lrx (Bin 1 lk lx Tip Tip) (Bin 1 k x Tip Tip)
                    ((Bin _ _ _ _ _), Tip) -> Bin 3 lk lx ll (Bin 1 k x Tip Tip)
                    ((Bin lls _ _ _ _), (Bin lrs lrk lrx lrl lrr))
                      | lrs < ratio*lls -> Bin (1+ls) lk lx ll (Bin (1+lrs) k x lr Tip)
                      | otherwise -> Bin (1+ls) lrk lrx (Bin (1+lls+size lrl) lk lx ll lrl) (Bin (1+size lrr) k x lrr Tip)
           (Bin rs rk rx rl rr)
              | rs > delta*ls  -> case (rl, rr) of
                   (Bin rls rlk rlx rll rlr, Bin rrs _ _ _ _)
                     | rls < ratio*rrs -> Bin (1+ls+rs) rk rx (Bin (1+ls+rls) k x l rl) rr
                     | otherwise -> Bin (1+ls+rs) rlk rlx (Bin (1+ls+size rll) k x l rll) (Bin (1+rrs+size rlr) rk rx rlr rr)
                   (_, _) -> error "Failure in Data.Map.balance"
              | ls > delta*rs  -> case (ll, lr) of
                   (Bin lls _ _ _ _, Bin lrs lrk lrx lrl lrr)
                     | lrs < ratio*lls -> Bin (1+ls+rs) lk lx ll (Bin (1+rs+lrs) k x lr r)
                     | otherwise -> Bin (1+ls+rs) lrk lrx (Bin (1+lls+size lrl) lk lx ll lrl) (Bin (1+rs+size lrr) k x lrr r)
                   (_, _) -> error "Failure in Data.Map.balance"
              | otherwise -> Bin (1+ls+rs) k x l r
{-# NOINLINE balance #-}

-- Functions balanceL and balanceR are specialised versions of balance.
-- balanceL only checks whether the left subtree is too big,
-- balanceR only checks whether the right subtree is too big.

-- balanceL is called when left subtree might have been inserted to or when
-- right subtree might have been deleted from.
balanceL :: k x -> a x -> Map k a -> Map k a -> Map k a
balanceL k x l r = case r of
  Tip -> case l of
           Tip -> Bin 1 k x Tip Tip
           (Bin _ _ _ Tip Tip) -> Bin 2 k x l Tip
           (Bin _ lk lx Tip (Bin _ lrk lrx _ _)) -> Bin 3 lrk lrx (Bin 1 lk lx Tip Tip) (Bin 1 k x Tip Tip)
           (Bin _ lk lx ll@(Bin _ _ _ _ _) Tip) -> Bin 3 lk lx ll (Bin 1 k x Tip Tip)
           (Bin ls lk lx ll@(Bin lls _ _ _ _) lr@(Bin lrs lrk lrx lrl lrr))
             | lrs < ratio*lls -> Bin (1+ls) lk lx ll (Bin (1+lrs) k x lr Tip)
             | otherwise -> Bin (1+ls) lrk lrx (Bin (1+lls+size lrl) lk lx ll lrl) (Bin (1+size lrr) k x lrr Tip)

  (Bin rs _ _ _ _) -> case l of
           Tip -> Bin (1+rs) k x Tip r

           (Bin ls lk lx ll lr)
              | ls > delta*rs  -> case (ll, lr) of
                   (Bin lls _ _ _ _, Bin lrs lrk lrx lrl lrr)
                     | lrs < ratio*lls -> Bin (1+ls+rs) lk lx ll (Bin (1+rs+lrs) k x lr r)
                     | otherwise -> Bin (1+ls+rs) lrk lrx (Bin (1+lls+size lrl) lk lx ll lrl) (Bin (1+rs+size lrr) k x lrr r)
                   (_, _) -> error "Failure in Data.Map.balanceL"
              | otherwise -> Bin (1+ls+rs) k x l r
{-# NOINLINE balanceL #-}

-- balanceR is called when right subtree might have been inserted to or when
-- left subtree might have been deleted from.
balanceR :: k x -> a x -> Map k a -> Map k a -> Map k a
balanceR k x l r = case l of
  Tip -> case r of
           Tip -> Bin 1 k x Tip Tip
           (Bin _ _ _ Tip Tip) -> Bin 2 k x Tip r
           (Bin _ rk rx Tip rr@(Bin _ _ _ _ _)) -> Bin 3 rk rx (Bin 1 k x Tip Tip) rr
           (Bin _ rk rx (Bin _ rlk rlx _ _) Tip) -> Bin 3 rlk rlx (Bin 1 k x Tip Tip) (Bin 1 rk rx Tip Tip)
           (Bin rs rk rx rl@(Bin rls rlk rlx rll rlr) rr@(Bin rrs _ _ _ _))
             | rls < ratio*rrs -> Bin (1+rs) rk rx (Bin (1+rls) k x Tip rl) rr
             | otherwise -> Bin (1+rs) rlk rlx (Bin (1+size rll) k x Tip rll) (Bin (1+rrs+size rlr) rk rx rlr rr)

  (Bin ls _ _ _ _) -> case r of
           Tip -> Bin (1+ls) k x l Tip

           (Bin rs rk rx rl rr)
              | rs > delta*ls  -> case (rl, rr) of
                   (Bin rls rlk rlx rll rlr, Bin rrs _ _ _ _)
                     | rls < ratio*rrs -> Bin (1+ls+rs) rk rx (Bin (1+ls+rls) k x l rl) rr
                     | otherwise -> Bin (1+ls+rs) rlk rlx (Bin (1+ls+size rll) k x l rll) (Bin (1+rrs+size rlr) rk rx rlr rr)
                   (_, _) -> error "Failure in Data.Map.balanceR"
              | otherwise -> Bin (1+ls+rs) k x l r
{-# NOINLINE balanceR #-}


{--------------------------------------------------------------------
  The bin constructor maintains the size of the tree
--------------------------------------------------------------------}
bin :: k x -> a x -> Map k a -> Map k a -> Map k a
bin k x l r
  = Bin (size l + size r + 1) k x l r
{-# INLINE bin #-}


{--------------------------------------------------------------------
  Eq converts the tree to a list. In a lazy setting, this
  actually seems one of the faster methods to compare two trees
  and it is certainly the simplest :-)
--------------------------------------------------------------------}
instance (IEq k, IEq a) => Eq (Map k a) where
  t1 == t2  = (size t1 == size t2) && (toAscList t1 == toAscList t2)

{--------------------------------------------------------------------
  Ord
--------------------------------------------------------------------}

instance (IOrd k, IOrd v) => Ord (Map k v) where
    compare m1 m2 = compare (toAscList m1) (toAscList m2)

{--------------------------------------------------------------------
  Functor
--------------------------------------------------------------------}
{-
instance Functor (Map k) where
  fmap f m  = map f m

instance Traversable (Map k) where
  traverse f = traverseWithKey (\_ -> f)
  {-# INLINE traverse #-}

instance Foldable.Foldable (Map k) where
  fold = go
    where go Tip = mempty
          go (Bin 1 _ v _ _) = v
          go (Bin _ _ v l r) = go l `mappend` (v `mappend` go r)
  {-# INLINABLE fold #-}
  foldr = foldr
  {-# INLINE foldr #-}
  foldl = foldl
  {-# INLINE foldl #-}
  foldMap f t = go t
    where go Tip = mempty
          go (Bin 1 _ v _ _) = f v
          go (Bin _ _ v l r) = go l `mappend` (f v `mappend` go r)
  {-# INLINE foldMap #-}

#if MIN_VERSION_base(4,6,0)
  foldl' = foldl'
  {-# INLINE foldl' #-}
  foldr' = foldr'
  {-# INLINE foldr' #-}
#endif
#if MIN_VERSION_base(4,8,0)
  length = size
  {-# INLINE length #-}
  null   = null
  {-# INLINE null #-}
  toList = elems -- NB: Foldable.toList /= Map.toList
  {-# INLINE toList #-}
  elem = go
    where STRICT_1_OF_2(go)
          go _ Tip = False
          go x (Bin _ _ v l r) = x == v || go x l || go x r
  {-# INLINABLE elem #-}
  maximum = start
    where start Tip = error "Map.Foldable.maximum: called with empty map"
          start (Bin _ _ v l r) = go (go v l) r

          STRICT_1_OF_2(go)
          go m Tip = m
          go m (Bin _ _ v l r) = go (go (max m v) l) r
  {-# INLINABLE maximum #-}
  minimum = start
    where start Tip = error "Map.Foldable.minumum: called with empty map"
          start (Bin _ _ v l r) = go (go v l) r

          STRICT_1_OF_2(go)
          go m Tip = m
          go m (Bin _ _ v l r) = go (go (min m v) l) r
  {-# INLINABLE minimum #-}
  sum = foldl' (+) 0
  {-# INLINABLE sum #-}
  product = foldl' (*) 1
  {-# INLINABLE product #-}
#endif
-}

{--------------------------------------------------------------------
  Read
--------------------------------------------------------------------}
instance (IOrd k, IRead k, IRead e) => Read (Map k e) where
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return (fromList xs)

  readListPrec = readListPrecDefault

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}
instance (IShow k, IShow a) => Show (Map k a) where
  showsPrec d m  = showParen (d > 10) $
    showString "fromList " . shows (toList m)

-- | /O(n)/. Show the tree that implements the map. The tree is shown
-- in a compressed, hanging format. See 'showTreeWith'.
showTree :: (IShow k, IShow a) => Map k a -> String
showTree m
  = showTreeWith showElem True False m
  where
    showElem k x  = ishow k ++ ":=" ++ ishow x


{- | /O(n)/. The expression (@'showTreeWith' showelem hang wide map@) shows
 the tree that implements the map. Elements are shown using the @showElem@ function. If @hang@ is
 'True', a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.

>  Map> let t = fromDistinctAscList [(x,()) | x <- [1..5]]
>  Map> putStrLn $ showTreeWith (\k x -> show (k,x)) True False t
>  (4,())
>  +--(2,())
>  |  +--(1,())
>  |  +--(3,())
>  +--(5,())
>
>  Map> putStrLn $ showTreeWith (\k x -> show (k,x)) True True t
>  (4,())
>  |
>  +--(2,())
>  |  |
>  |  +--(1,())
>  |  |
>  |  +--(3,())
>  |
>  +--(5,())
>
>  Map> putStrLn $ showTreeWith (\k x -> show (k,x)) False True t
>  +--(5,())
>  |
>  (4,())
>  |
>  |  +--(3,())
>  |  |
>  +--(2,())
>     |
>     +--(1,())

-}
showTreeWith :: (forall x. k x -> a x -> String) -> Bool -> Bool -> Map k a -> String
showTreeWith showelem hang wide t
  | hang      = (showsTreeHang showelem wide [] t) ""
  | otherwise = (showsTree showelem wide [] [] t) ""

showsTree :: (forall x. k x -> a x -> String) -> Bool -> [String] -> [String] -> Map k a -> ShowS
showsTree showelem wide lbars rbars t
  = case t of
      Tip -> showsBars lbars . showString "|\n"
      Bin _ kx x Tip Tip
          -> showsBars lbars . showString (showelem kx x) . showString "\n"
      Bin _ kx x l r
          -> showsTree showelem wide (withBar rbars) (withEmpty rbars) r .
             showWide wide rbars .
             showsBars lbars . showString (showelem kx x) . showString "\n" .
             showWide wide lbars .
             showsTree showelem wide (withEmpty lbars) (withBar lbars) l

showsTreeHang :: (forall x. k x -> a x -> String) -> Bool -> [String] -> Map k a -> ShowS
showsTreeHang showelem wide bars t
  = case t of
      Tip -> showsBars bars . showString "|\n"
      Bin _ kx x Tip Tip
          -> showsBars bars . showString (showelem kx x) . showString "\n"
      Bin _ kx x l r
          -> showsBars bars . showString (showelem kx x) . showString "\n" .
             showWide wide bars .
             showsTreeHang showelem wide (withBar bars) l .
             showWide wide bars .
             showsTreeHang showelem wide (withEmpty bars) r

showWide :: Bool -> [String] -> String -> String
showWide wide bars
  | wide      = showString (concat (reverse bars)) . showString "|\n"
  | otherwise = id

showsBars :: [String] -> ShowS
showsBars bars
  = case bars of
      [] -> id
      _  -> showString (concat (reverse (tail bars))) . showString node

node :: String
node           = "+--"

withBar, withEmpty :: [String] -> [String]
withBar bars   = "|  ":bars
withEmpty bars = "   ":bars

{--------------------------------------------------------------------
  Typeable
--------------------------------------------------------------------}

deriving instance Typeable Map

{--------------------------------------------------------------------
  Assertions
--------------------------------------------------------------------}
-- | /O(n)/. Test if the internal map structure is valid.
--
-- > valid (fromAscList [(3,"b"), (5,"a")]) == True
-- > valid (fromAscList [(5,"a"), (3,"b")]) == False

valid :: IOrd k => Map k a -> Bool
valid t
  = balanced t && ordered t && validsize t

ordered :: forall a b. IOrd a => Map a b -> Bool
ordered t
  = bounded (const True) (const True) t
  where
    bounded :: (forall x. a x -> Bool) -> (forall x. a x -> Bool) -> Map a b -> Bool
    bounded lo hi t'
      = case t' of
          Tip              -> True
          Bin _ kx _ l r  -> (lo kx) && (hi kx) && bounded lo (`ilt` kx) l && bounded (`igt` kx) hi r
            where ilt x y = case icompare x y of
                              ILT -> True
                              _     -> False
                  igt x y = case icompare x y of
                              IGT -> True
                              _     -> False

-- | Exported only for "Debug.QuickCheck"
balanced :: Map k a -> Bool
balanced t
  = case t of
      Tip            -> True
      Bin _ _ _ l r  -> (size l + size r <= 1 || (size l <= delta*size r && size r <= delta*size l)) &&
                        balanced l && balanced r

validsize :: Map a b -> Bool
validsize t
  = (realsize t == Just (size t))
  where
    realsize t'
      = case t' of
          Tip            -> Just 0
          Bin sz _ _ l r -> case (realsize l,realsize r) of
                            (Just n,Just m)  | n+m+1 == sz  -> Just sz
                            _                               -> Nothing

{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}

-- | /O(1)/.  Decompose a map into pieces based on the structure of the underlying
-- tree.  This function is useful for consuming a map in parallel.
--
-- No guarantee is made as to the sizes of the pieces; an internal, but
-- deterministic process determines this.  However, it is guaranteed that the pieces
-- returned will be in ascending order (all elements in the first submap less than all
-- elements in the second, and so on).
--
-- Examples:
--
-- > splitRoot (fromList (zip [1..6] ['a'..])) ==
-- >   [fromList [(1,'a'),(2,'b'),(3,'c')],fromList [(4,'d')],fromList [(5,'e'),(6,'f')]]
--
-- > splitRoot empty == []
--
--  Note that the current implementation does not return more than three submaps,
--  but you should not depend on this behaviour because it can change in the
--  future without notice.
splitRoot :: Map k b -> [Map k b]
splitRoot orig =
  case orig of
    Tip           -> []
    Bin _ k v l r -> [l, singleton k v, r]
{-# INLINE splitRoot #-}
