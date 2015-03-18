{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Indexed.Class
-- Copyright   :  (C) 2015 SkedgeMe LLC
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Daniel Haraj <daniel.haraj@skedge.me>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Indexed types that can be compared or ordered.
-----------------------------------------------------------------------------
module Indexed.Class
  ( IEq(..)
  , IOrd(..)
  , IBool (..)
  , IOrdering (..)
  , IShow(..), ishow, ishows
  , IRead(..), iread, ireads
  , ireadPrec            -- :: (Read1 f, Read a) => ReadPrec (f a)
  , ireadListPrec        -- :: (Read1 f, Read a) => ReadPrec [f a]
  , ireadListDefault     -- :: (Read1 f, Read a) => ReadS [f a]
  , ireadListPrecDefault -- :: (Read1 f, Read a) => ReadPrec [f a]
  ) where

import Text.Read
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.Read.Lex as L

data IBool a b where
  IFalse :: IBool a b
  ITrue :: IBool a a

data IOrdering a b where
  ILT :: IOrdering a b
  IEQ :: IOrdering a a
  IGT :: IOrdering a b

class IEq f where
  ieq :: f a -> f b -> IBool a b
  default ieq :: IOrd f => f a -> f b -> IBool a b
  ieq fa fb = case icompare fa fb of
    IEQ -> ITrue
    _   -> IFalse

class IEq f => IOrd f where
  icompare :: f a -> f b -> IOrdering a b

class IShow f where
  ishowsPrec :: Int -> f a -> ShowS
  default ishowsPrec :: Show (f a) => Int -> f a -> ShowS
  ishowsPrec = showsPrec
  ishowList :: [f a] -> ShowS
  ishowList ls s = showList__ ishows ls s

ishow :: IShow f => f a -> String
ishow x = ishows x ""

ishows :: IShow f => f a -> ShowS
ishows = ishowsPrec 0

class IRead f where
  ireadsPrec :: Int -> ReadS (f a)
  default ireadsPrec :: Read (f a) => Int -> ReadS (f a)
  ireadsPrec = readsPrec
  ireadList :: ReadS [f a]
  ireadList  = readPrec_to_S (list ireadPrec) 0

ireadPrec :: IRead f => ReadPrec (f a)
ireadPrec = readS_to_Prec ireadsPrec

ireadListPrec :: IRead f => ReadPrec [f a]
ireadListPrec = readS_to_Prec (\_ -> ireadList)

iread  :: IRead f => String -> f a
iread s = either error id (ireadEither s)

ireads :: IRead f => ReadS (f a)
ireads = ireadsPrec minPrec

ireadEither :: IRead f => String -> Either String (f a)
ireadEither s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"
 where
  read' =
    do x <- ireadPrec
       lift P.skipSpaces
       return x

ireadListDefault     :: IRead f => ReadS [f a]
ireadListDefault = readPrec_to_S ireadListPrec 0

ireadListPrecDefault :: IRead f => ReadPrec [f a]
ireadListPrecDefault = list ireadPrec

list :: ReadPrec a -> ReadPrec [a]
list readx =
  parens
  ( do L.Punc "[" <- lexP
       (listRest False +++ listNext)
  )
 where
  listRest started =
    do L.Punc c <- lexP
       case c of
         "]"           -> return []
         "," | started -> listNext
         _             -> pfail

  listNext =
    do x  <- reset readx
       xs <- listRest True
       return (x:xs)

showList__ :: (a -> ShowS) ->  [a] -> ShowS
showList__ _     []     s = "[]" ++ s
showList__ showx (x:xs) s = '[' : showx x (showl xs)
  where
    showl []     = ']' : s
    showl (y:ys) = ',' : showx y (showl ys)
