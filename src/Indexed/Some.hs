{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Indexed.Some
-- Copyright   :  (C) 2015 SkedgeMe LLC
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Daniel Haraj <daniel.haraj@skedge.me>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Wrappers for indexed values
-----------------------------------------------------------------------------
module Indexed.Some where

import Text.Read

import Indexed.Class

data Some1 a = forall x. Some1 (a x)
data Some2 a b = forall x. Some2 (a x) (b x)

instance IEq a => Eq (Some1 a) where
  (Some1 a) == (Some1 b) = case ieq a b of
    IFalse -> False
    ITrue  -> True

instance IOrd a => Ord (Some1 a) where
  compare (Some1 a) (Some1 b) = case icompare a b of
    ILT -> LT
    IEQ -> EQ
    IGT -> GT

instance IShow a => Show (Some1 a) where
    show (Some1 x) = "Some1 " ++ ishow x

instance IRead a => Read (Some1 a) where
    readPrec = parens $ prec 10 $ do
      Ident "Some1" <- lexP
      x <- ireadPrec
      return (Some1 x)
    readListPrec = readListPrecDefault

instance (IEq a, IEq b) => Eq (Some2 a b) where
  (Some2 a b) == (Some2 c d) = case ieq a c of
    IFalse -> False
    ITrue  -> case ieq b d of
      IFalse -> False
      ITrue  -> True

instance (IOrd a, IOrd b) => Ord (Some2 a b) where
  compare (Some2 a b) (Some2 c d) = case icompare a c of
    ILT -> LT
    IEQ -> case icompare b d of
      ILT -> LT
      IEQ -> EQ
      IGT -> GT
    IGT -> GT

instance (IShow a, IShow b) => Show (Some2 a b) where
    show (Some2 x y) = "Some2 " ++ ishow x ++ " " ++ ishow y

instance (IRead a, IRead b) => Read (Some2 a b) where
    readPrec = parens $ prec 10 $ do
      Ident "Some2" <- lexP
      x <- ireadPrec
      y <- ireadPrec
      return (Some2 x y)
    readListPrec = readListPrecDefault
