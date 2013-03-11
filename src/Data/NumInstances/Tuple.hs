{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.NumInstances.Tuple
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
--
-- Number class instances for tuples
----------------------------------------------------------------------

module Data.NumInstances.Tuple () where

import Data.NumInstances.Util

lift2 :: (a->u) -> (b->v) -> (a,b) -> (u,v)
lift2 f g (a,b) = (f a, g b)

noPair :: String -> a
noPair = noOv "pair"

-- Equivalently, lift2 = (***)

instance (Num a, Num b) => Num (a,b) where
  fromInteger n   = (fromInteger n, fromInteger n)
  (a,b) + (a',b') = (a+a',b+b')
  (a,b) - (a',b') = (a-a',b-b')
  (a,b) * (a',b') = (a*a',b*b')
  negate = lift2 negate negate
  abs    = lift2 abs abs
  signum = lift2 signum signum

instance (Fractional a, Fractional b) => Fractional (a,b) where
  fromRational x = (fromRational x, fromRational x)
  recip = lift2 recip recip

instance (Floating a, Floating b) => Floating (a,b) where
  pi    = (pi,pi)
  exp   = lift2 exp exp
  log   = lift2 log log
  sqrt  = lift2 sqrt sqrt
  sin   = lift2 sin sin
  cos   = lift2 cos cos
  sinh  = lift2 sinh sinh
  cosh  = lift2 cosh cosh
  asin  = lift2 asin asin
  acos  = lift2 acos acos
  atan  = lift2 atan atan
  asinh = lift2 asinh asinh
  acosh = lift2 acosh acosh
  atanh = lift2 atanh atanh

instance (Num a, Num b, Num c) => Num (a,b,c) where
  fromInteger n = (fromInteger n, fromInteger n, fromInteger n)
  (a,b,c) + (a',b',c') = (a+a',b+b',c+c')
  (a,b,c) - (a',b',c') = (a-a',b-b',c-c')
  (a,b,c) * (a',b',c') = (a*a',b*b',c*c')
  negate = lift3 negate negate negate
  abs    = lift3 abs abs abs
  signum = lift3 signum signum signum

instance (Fractional a, Fractional b, Fractional c)
    => Fractional (a,b,c) where
  fromRational x = (fromRational x, fromRational x, fromRational x)
  recip = lift3 recip recip recip


lift3 :: (a->u) -> (b->v) -> (c->w) -> (a,b,c) -> (u,v,w)
lift3 f g h (a,b,c) = (f a, g b, h c)

instance (Floating a, Floating b, Floating c)
    => Floating (a,b,c) where
  pi    = (pi,pi,pi)
  exp   = lift3 exp exp exp
  log   = lift3 log log log
  sqrt  = lift3 sqrt sqrt sqrt
  sin   = lift3 sin sin sin
  cos   = lift3 cos cos cos
  sinh  = lift3 sinh sinh sinh
  cosh  = lift3 cosh cosh cosh
  asin  = lift3 asin asin asin
  acos  = lift3 acos acos acos
  atan  = lift3 atan atan atan
  asinh = lift3 asinh asinh asinh
  acosh = lift3 acosh acosh acosh
  atanh = lift3 atanh atanh atanh



lift4 :: (a->u) -> (b->v) -> (c->w) -> (d->x)
      -> (a,b,c,d) -> (u,v,w,x)
lift4 f g h k (a,b,c,d) = (f a, g b, h c, k d)

instance (Num a, Num b, Num c, Num d) => Num (a,b,c,d) where
  fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n)
  (a,b,c,d) + (a',b',c',d') = (a+a',b+b',c+c',d+d')
  (a,b,c,d) - (a',b',c',d') = (a-a',b-b',c-c',d-d')
  (a,b,c,d) * (a',b',c',d') = (a*a',b*b',c*c',d*d')
  negate = lift4 negate negate negate negate
  abs    = lift4 abs abs abs abs
  signum = lift4 signum signum signum signum

instance (Fractional a, Fractional b, Fractional c, Fractional d)
    => Fractional (a,b,c,d) where
  fromRational x = (fromRational x, fromRational x, fromRational x, fromRational x)
  recip = lift4 recip recip recip recip

instance (Floating a, Floating b, Floating c, Floating d)
    => Floating (a,b,c,d) where
  pi    = (pi,pi,pi,pi)
  exp   = lift4 exp exp exp exp
  log   = lift4 log log log log
  sqrt  = lift4 sqrt sqrt sqrt sqrt
  sin   = lift4 sin sin sin sin
  cos   = lift4 cos cos cos cos
  sinh  = lift4 sinh sinh sinh sinh
  cosh  = lift4 cosh cosh cosh cosh
  asin  = lift4 asin asin asin asin
  acos  = lift4 acos acos acos acos
  atan  = lift4 atan atan atan atan
  asinh = lift4 asinh asinh asinh asinh
  acosh = lift4 acosh acosh acosh acosh
  atanh = lift4 atanh atanh atanh atanh


{--------------------------------------------------------------------
    Some experiments in Enum and Integral instances for tuples
--------------------------------------------------------------------}

{-

-- Integral needs Enum

instance (Enum a, Enum b, Bounded b) => Enum (a,b) where
  toEnum = noPair "toEnum"
  fromEnum = noPair "fromEnum"
  -- enumerate consistently with Ord
  enumFromTo (alo,blo) (ahi,bhi) =
       [ (alo,b) | b <- [blo .. maxBound] ]
    ++ [ (a  ,b) | a <- [succ alo .. pred ahi], b <- [minBound .. maxBound] ]
    ++ [ (ahi,b) | b <- [minBound .. bhi] ]
  -- To do: replace enumFromTo def with an enumFromThenTo def

-- deriving instance (Enum a, Enum b) => Enum (a,b)
--
--     The data constructors of `(,)' are not all in scope
--       so you cannot derive an instance for it

-- To do: toEnum and fromEnum

{-
-- Test:

data Foo = A | B | C | D deriving (Enum,Bounded,Show)

t1 :: [(Foo,Foo)]
t1 = [(B,C) .. (C,C)]
-}

instance (Real a, Real b) => Real (a,b) where
  toRational = noPair "toRational"


instance (Integral a, Integral b, Bounded b) => Integral (a,b) where
--   (a,b) `quotRem` (a',b') = ((qa,qb),(ra,rb))
--     where
--       (qa,ra) = a `quotRem` a'
--       (qb,rb) = b `quotRem` b'
  (a,b) `quotRem` (a',b') = transpose (a `quotRem` a' , b `quotRem` b')
    where
      transpose ((w,x),(y,z)) = ((w,y),(x,z)) -- to-do: pretty point-free
  toInteger (a , b::b) = toInteger a * widthB + toInteger b
   where
     widthB = toInteger (maxBound :: b) - toInteger (minBound :: b)

{-
t2 :: ((Int,Int),(Int,Int))
t2 = (7,8) `quotRem` (2,3)

t3 :: (Int,Int)
t3 = (7,8) `quot` (2,3)

t4 :: (Int,Int)
t4 = (7,8) `rem` (2,3)
-}

{-
  (a,b) `quot` (a',b') = (a `quot` a', b `quot` b')


  rem           = liftA2 rem
  div           = liftA2 div
  mod           = liftA2 mod
  toInteger     = noOv "toInteger"
  x `quotRem` y = (x `quot` y, x `rem` y)
  x `divMod`  y = (x `div`  y, x `mod` y)
-}

-}
