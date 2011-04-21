
---------
-- Inf --
---------

module Inf
  ( Inf
  , inf, fin
  ) where

data Inf a
  = Finite a
  | Infinite Bool
  deriving Eq

instance Show a => Show (Inf a) where
  show (Finite x)       = show x
  show (Infinite True ) = "<+inf>"
  show (Infinite False) = "<-inf>"

instance Ord a => Ord (Inf a) where
  compare (Finite   x    ) (Finite   y)     = compare x y
  compare (Infinite b    ) (Infinite c)     = compare b c
  compare (Infinite True ) _                = GT
  compare (Infinite False) _                = LT
  compare _                (Infinite True ) = LT
  compare _                (Infinite False) = GT

instance Num a => Num (Inf a) where
  Finite   x     + Finite   y     = Finite (x + y)
  Infinite True  + Infinite False = error "<+inf> + <-inf>"
  Infinite False + Infinite True  = error "<-inf> + <+inf>"
  Infinite True  + _              = Infinite True
  Infinite False + _              = Infinite False
  _              + Infinite True  = Infinite True
  _              + Infinite False = Infinite False
  Finite   x     * Finite   y     = Finite (x * y)
  Infinite b     * Infinite c     = Infinite (b == c)
  Infinite _     * Finite   0     = error "<inf> * 0"
  Finite   0     * Infinite _     = error "0 * <inf>"
  Infinite b     * Finite   x     = Infinite (b == (signum x == 1))
  Finite   x     * Infinite b     = Infinite ((signum x == 1) == b)
  negate (Infinite b    ) = Infinite (not    b)
  negate (Finite   x    ) = Finite   (negate x)
  signum (Infinite True ) =  1
  signum (Infinite False) = -1
  signum (Finite   x    ) = Finite (signum x)
  abs    (Infinite _    ) = Infinite True
  abs    (Finite   x    ) = Finite (abs x)
  fromInteger x           = Finite (fromInteger x)
  
fin :: a -> Inf a 
fin = Finite

inf :: Inf a
inf = Infinite True