module Data.Tagged

import Data.Bifunctor
import Data.Profunctor

%access public export
%default total

data Tagged : (a : Type) -> (b : Type) -> Type where
  MkTagged: b -> Tagged a b

retag : Tagged s b -> Tagged t b
retag (MkTagged b) = MkTagged b

||| Alias for 'unTagged'
untag : Tagged s b -> b
untag (MkTagged x) = x

||| Tag a value with its own type.
tagSelf : a -> Tagged a a
tagSelf = MkTagged

||| 'asTaggedTypeOf' is a type-restricted version of 'const'. It is usually used as an infix operator, and its typing forces its first argument (which is usually overloaded) to have the same type as the tag of the second.
asTaggedTypeOf : s -> Tagged s b -> s
asTaggedTypeOf = const

||| 'untagSelf' is a type-restricted version of 'untag'.
untagSelf : Tagged a a -> a
untagSelf (MkTagged x) = x

Semigroup a => Semigroup (Tagged s a) where
  (MkTagged a) <+> (MkTagged b) = MkTagged (a <+> b)

Monoid a => Monoid (Tagged s a) where
  neutral = MkTagged neutral

Functor (Tagged s) where
  map f (MkTagged x) = MkTagged (f x)

implementation Applicative (Tagged s) where
  pure = MkTagged
  (MkTagged f) <*> (MkTagged x) = MkTagged (f x)

implementation Monad (Tagged s) where
  (MkTagged m) >>= k = k m

implementation Bifunctor Tagged where
  bimap _ g (MkTagged b) = MkTagged (g b)

implementation Profunctor Tagged where
  dimap _ f (MkTagged s) = MkTagged (f s)
  lmap _ = retag
  rmap = map

implementation Choice Tagged where
  left' (MkTagged b) = MkTagged (Left b)
  right' (MkTagged b) = MkTagged (Right b)
