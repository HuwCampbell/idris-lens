-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Data.Yoneda

import Data.Morphisms

%access public export

-- | @Yoneda f a@ can be viewed as the partial application of 'fmap' to its second argument.
data Yoneda : ( f : Type -> Type ) -> ( a : Type ) -> Type where
  MkYoneda : ({b : Type} -> (a -> b) -> f b) -> Yoneda f a

-- | The natural isomorphism between @f@ and @'Yoneda' f@ given by the Yoneda lemma
-- is witnessed by 'liftYoneda' and 'lowerYoneda'
--
-- @
-- 'liftYoneda' . 'lowerYoneda' ≡ 'id'
-- 'lowerYoneda' . 'liftYoneda' ≡ 'id'
-- @
--
-- @
-- lowerYoneda (liftYoneda fa) =         -- definition
-- lowerYoneda (Yoneda (\f -> fmap f a)) -- definition
-- (\f -> fmap f fa) id                  -- beta reduction
-- fmap id fa                            -- functor law
-- fa
-- @
--
-- @
-- 'lift' = 'liftYoneda'
-- @
liftYoneda : { f : Type -> Type } -> Functor f => f a -> Yoneda f a
liftYoneda a = MkYoneda (flip map a)

lowerYoneda : Yoneda f a -> f a
lowerYoneda (MkYoneda f) = f id

implementation Functor f => Functor (Yoneda f) where
  map h (MkYoneda k) = MkYoneda (flip map (k h))

implementation Applicative f => Applicative (Yoneda f) where
  pure a = MkYoneda (\f => pure (f a))
  (MkYoneda m) <*> (MkYoneda n) = MkYoneda (\f => m (f .) <*> n id)
