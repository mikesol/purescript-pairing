module Data.Functor.Flipped where

newtype Flipped :: forall k1 k2 k3 k4. (k1 -> k2 -> k3 -> k4 -> Type) -> k1 -> k3 -> k2 -> k4 -> Type
newtype Flipped a b c d e = Flipped (a b d c e)