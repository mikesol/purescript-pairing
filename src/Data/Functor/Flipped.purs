module Data.Functor.Flipped where

type Flipped :: forall k1 k2 k3 k4. (k1 -> k2 -> k3 -> k4 -> Type) -> k1 -> k3 -> k2 -> k4 -> Type
type Flipped a b c d e = a b d c e