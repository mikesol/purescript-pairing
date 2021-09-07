module Control.Comonad.MessengerT where

import Prelude

import Control.Comonad (class Comonad)
import Control.Comonad.Env (EnvT)
import Control.Comonad.Env.Class (class ComonadAsk)
import Control.Comonad.Trans.Class (class ComonadTrans)
import Control.Extend (class Extend)
import Data.Foldable (class Foldable)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable)

newtype MessengerT :: forall k. Type -> Type -> (k -> Type) -> k -> Type
newtype MessengerT e b w a = MessengerT (EnvT (e -> b) w a)

newtype MessengerFlipped :: forall k. Type -> (k -> Type) -> Type -> k -> Type
newtype MessengerFlipped e w b a = MessengerFlipped (MessengerT e b w a)

derive instance newtypeMessengerT :: Newtype (MessengerT e b w a) _

derive newtype instance functorMessengerT :: Functor w => Functor (MessengerT e b w)

derive newtype instance extendMessengerT :: Extend w => Extend (MessengerT e b w)

derive newtype instance comonadMessengerT :: Comonad w => Comonad (MessengerT e b w)

derive newtype instance comonadTransMessengerT :: ComonadTrans (MessengerT e b)

derive newtype instance foldableMessengerT :: Foldable f => Foldable (MessengerT e b f)

derive newtype instance traversableMessengerT :: Traversable f => Traversable (MessengerT e b f)

derive newtype instance comonadAskMessengerT :: Comonad w => ComonadAsk (e -> b) (MessengerT e b w)

derive instance newtypeMessengerFlipped :: Newtype (MessengerFlipped e w b a) _

derive newtype instance functorMessengerFlipped :: Functor w => Functor (MessengerFlipped e w b)

derive newtype instance extendMessengerFlipped :: Extend w => Extend (MessengerFlipped e w b)

derive newtype instance comonadMessengerFlipped :: Comonad w => Comonad (MessengerFlipped e w b)

derive newtype instance foldableMessengerFlipped :: Foldable f => Foldable (MessengerFlipped e f b)

derive newtype instance traversableMessengerFlipped :: Traversable f => Traversable (MessengerFlipped e f b)

derive newtype instance comonadAskMessengerFlipped :: Comonad w => ComonadAsk (e -> b) (MessengerFlipped e w b)
