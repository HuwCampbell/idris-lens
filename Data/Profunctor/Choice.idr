module Data.Profunctor.Choice

import Data.Profunctor.Class
import Data.Profunctor.Arrow

%access public export

interface Profunctor p => Choice (p : Type -> Type -> Type) where
  left' : p a b -> p (Either a c) (Either b c)
  left' = dimap (either Right Left) (either Right Left) . right'

  right' : p a b -> p (Either c a) (Either c b)
  right' = dimap (either Right Left) (either Right Left) . left'

Choice Arrow where
  left' (MkArrow f) = MkArrow g
    where g (Left a) = Left (f a)
          g (Right c) = Right c

  right' (MkArrow f) = MkArrow g
    where g (Left c) = Left c
          g (Right a) = Right (f a)
