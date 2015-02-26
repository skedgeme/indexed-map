{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Indexed.Ord
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Indexed types that can be compared or ordered.
-----------------------------------------------------------------------------
module Indexed.Ord
  ( IEq(..)
  , IOrd(..)
  , IShow(..)
  , ishows
  , IRead(..)
  , NFData1(..)
  , ilt, igt, ilte, igte
  ) where

import Text.Read
import Text.ParserCombinators.ReadP

class IEq f where
  ieq :: f a -> f b -> ((a ~ b) => r) -> r -> r
  default ieq :: IOrd f => f a -> f b -> ((a ~ b) => r) -> r -> r
  ieq fa fb e ne = icompare fa fb ne e ne

class IEq f => IOrd f where
  icompare :: f a -> f b -> r -> ((a ~ b) => r) -> r -> r

class IShow f where
  ishow :: f a -> String
  ishowsPrec :: Int -> f a -> ShowS
  ishowsPrec _ x s = ishow x ++ s
  ishow x = ishows x ""

ishows :: IShow f => f a -> ShowS
ishows = ishowsPrec 0

class IRead f where
  ireadsPrec :: Int -> ReadS (f a)
  ireadPrec  :: ReadPrec (f a)
  ireadsPrec    = readPrec_to_S ireadPrec
  ireadPrec     = readS_to_Prec ireadsPrec
  {-# MINIMAL ireadsPrec | ireadPrec #-}

class NFData1 f where
  rnf1 :: f a -> ()

ilt, igt, ilte, igte :: IOrd f => f a -> f b -> Bool
ilt x y = icompare x y True False False
igt x y = icompare x y False False True
ilte x y = icompare x y True True False
igte x y = icompare x y False True True
