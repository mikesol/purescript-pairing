-- | Sharings between functors.
-- |

module Data.Functor.Sharing
  ( Sharing
  , type (🤗)
  , identityFunction
  , functionIdentity
  , fromPairing
  , toPairing
  , exceptMessenger
  ) where

import Control.Comonad.Env (EnvT(..))
import Control.Comonad.MessengerT (MessengerT(..))
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Either (Either(..))
import Data.Functor.Flipped (Flipped)
import Data.Functor.Pairing (type (⋈))
import Data.Functor.Ungrateful (Ungrateful(..))
import Data.Identity (Identity(..))
import Data.Tuple (Tuple(..))

-- | A sharing between Functor1's `f` and `g`.
-- |
type Sharing f g = forall a b c. (a -> b -> c) -> f b a -> g a b -> c

infix 4 type Sharing as 🤗

-- | The identity functor shares with a function
identityFunction :: Ungrateful Identity 🤗 Function
identityFunction f (Ungrateful (Identity a)) aToB = f a (aToB a)

functionIdentity :: Function 🤗 Ungrateful Identity
functionIdentity f bToA (Ungrateful (Identity b)) = f (bToA b) b

fromPairing :: forall f g. f ⋈ g -> Ungrateful f 🤗 Ungrateful g
fromPairing pairing f (Ungrateful fx) (Ungrateful gx) = pairing f fx gx

toPairing :: forall f g. Ungrateful f 🤗 Ungrateful g -> f ⋈ g
toPairing pairing f fx gx = pairing f (Ungrateful fx) (Ungrateful gx)

exceptMessenger :: forall e f g. f ⋈ g -> Ungrateful (ExceptT e f) 🤗 Flipped MessengerT e g
exceptMessenger pairing f (Ungrateful e) (MessengerT (EnvT (Tuple handler gb))) =
  pairing
    ( \a b -> f
        ( case a of
            Left h -> handler h
            Right a' -> a'
        )
        b
    )
    (runExceptT e)
    gb
