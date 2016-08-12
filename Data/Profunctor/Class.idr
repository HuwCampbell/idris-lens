module Data.Profunctor.Class

import Data.Morphisms

%access public export

interface Profunctor (p : Type -> Type -> Type) where
  dimap : (a -> b) -> (c -> d) -> p b c -> p a d
  dimap f g = lmap f . rmap g

  lmap : (a -> b) -> p b c -> p a c
  lmap f = dimap f id

  rmap : (c -> d) -> p b c -> p b d
  rmap f = dimap id f

Profunctor Morphism where
  dimap f g (Mor h) = Mor (g . h . f)

  lmap f (Mor h) = Mor (h . f)

  rmap g (Mor h) = Mor (g . h)
