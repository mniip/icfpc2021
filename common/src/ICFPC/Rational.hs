{-# LANGUAGE DerivingStrategies #-}
module ICFPC.Rational where

import Data.Ratio

import ICFPC.Vector

data Q2 = Q2 {-# UNPACK #-} !Rational !Rational
  deriving stock (Eq, Ord, Show)

-- divided by d
{-# INLINE v2ToQ2 #-}
v2ToQ2 :: Int -> V2 -> Q2
v2ToQ2 d (V2 x y) = Q2 (toInteger x % toInteger d) (toInteger y % toInteger d)

{-# INLINE (~+~) #-}
(~+~) :: Q2 -> Q2 -> Q2
Q2 x1 y1 ~+~ Q2 x2 y2 = Q2 (x1 + x2) (y1 + y2)
infixl 6 ~+~

{-# INLINE (~-~) #-}
(~-~) :: Q2 -> Q2 -> Q2
Q2 x1 y1 ~-~ Q2 x2 y2 = Q2 (x1 - x2) (y1 - y2)
infixl 6 ~-~

{-# INLINE dotQ #-}
-- inner/dot product
dotQ :: Q2 -> Q2 -> Rational
Q2 x1 y1 `dotQ` Q2 x2 y2 = x1 * x2 + y1 * y2
infixl 7 `dotQ`

{-# INLINE ccwQ #-}
-- rotate counter clockwise
ccwQ :: Q2 -> Q2
ccwQ (Q2 x y) = Q2 (-y) x

{-# INLINE signedAreaQ #-}
-- signed area of a parallelogram subtended by two vectors
-- signedArea (Q2 1 0) (Q2 0 1) = 1
signedAreaQ :: Q2 -> Q2 -> Rational
signedAreaQ u v = ccwQ u `dotQ` v

{-# INLINE qFloor #-}
qFloor :: Rational -> Int
qFloor q = fromInteger $ numerator q `div` denominator q

{-# INLINE qCeil #-}
qCeil :: Rational -> Int
qCeil q = fromInteger $ -((-numerator q) `div` denominator q)
