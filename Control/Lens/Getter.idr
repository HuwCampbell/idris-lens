-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.Getter
import Control.Lens.Types
import Control.Lens.Const
import Control.Lens.Contravariant
import Control.Lens.First

%default total
%access public export

view : Getter a s a -> s -> a
view l = getConst . l MkConst

views : Getter a s a -> (a -> r) -> s -> r
views l f a = f $ getConst (l MkConst a)

foldMapOf : Getter r s a -> (a -> r) -> s -> r
foldMapOf l f = getConst . l (MkConst . f)

-- Creates a lens where the Functor instance must be
-- covariant. Practically this means we can only use
-- Const, so this is a valid getter and nothing else

||| Create a Getter from arbitrary functions `s -> a`.
to : Contravariant f => (s -> a) -> LensLike' f s a
to k = dimap' k (contramap k) where
  dimap' : (a -> b) -> (c -> d) -> (b -> c) -> (a -> d)
  dimap' ab cd bc = cd . bc . ab

infixl 8 ^.
(^.) : s -> Getter a s a -> a
a ^. l = view l a

infixl 8 ^?
(^?) : s -> Getter (First a) s a -> Maybe a
s ^? l = getFirst (foldMapOf l (MkFirst . Just) s)

-- --------------------------------------------------------------------- [ EOF ]
