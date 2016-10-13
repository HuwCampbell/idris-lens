-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Data.Curried

import Data.Morphisms

%access public export

data Curried : ( g : Type -> Type ) -> ( h : Type -> Type ) -> ( a : Type ) -> Type where
  MkCurried : ({r : Type} -> g (a -> r) -> h r) -> Curried g h a

Functor g => Functor (Curried g h) where
  map f (MkCurried g) = MkCurried (g . map (.f))

Functor g => Applicative (Curried g g) where
  pure a = MkCurried (map (\f => f a))
  (MkCurried mf) <*> (MkCurried ma) = MkCurried (ma . mf . map (.))

liftCurried : Applicative f => f a -> Curried f f a
liftCurried fa = MkCurried (<*> fa)

lowerCurried : Applicative f => Curried f g a -> g a
lowerCurried (MkCurried f) = f (pure id)
