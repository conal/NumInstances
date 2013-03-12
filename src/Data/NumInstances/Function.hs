{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.NumInstances.Function
-- Copyright   :  (c) Conal Elliott 2008-2013
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
--
-- Number class instances for functions
----------------------------------------------------------------------

module Data.NumInstances.Function () where

import Control.Applicative

import Data.NumInstances.Util

noFun :: String -> a
noFun = noOv "function"

-- Eq & Show are prerequisites for Num, so they need to be faked here
instance Eq (a->b) where
  (==) = noFun "(==)"
  (/=) = noFun "(/=)"

instance Ord b => Ord (a->b) where
  min = liftA2 min
  max = liftA2 max

instance Show (a->b) where
  show      = noFun "show"
  showsPrec = noFun "showsPrec"
  showList  = noFun "showList"

instance Num b => Num (a->b) where
  negate      = fmap negate
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum

instance Fractional b => Fractional (a->b) where
  recip        = fmap recip
  fromRational = pure . fromRational

instance Floating b => Floating (a->b) where
  pi    = pure pi
  sqrt  = fmap sqrt
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  cosh  = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh
