module Data.Profunctor.Class

import Data.Profunctor.Arrow

%access public export

interface Profunctor (p : Type -> Type -> Type) where
  dimap : (a -> b) -> (c -> d) -> p b c -> p a d
  dimap f g = lmap f . rmap g

  lmap : (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  
  rmap : (c -> d) -> p b c -> p b d
  rmap f = dimap id f

Profunctor Arrow where
  dimap f g (MkArrow h) = MkArrow (g . h . f)

  lmap f (MkArrow h) = MkArrow (h . f)

  rmap g (MkArrow h) = MkArrow (g . h)
