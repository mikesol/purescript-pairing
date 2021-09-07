module Data.Functor.Ungrateful where

newtype Ungrateful :: forall k1 k2. (k1 -> Type) -> k2 -> k1 -> Type
newtype Ungrateful f i a = Ungrateful (f a)