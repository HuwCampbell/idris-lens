module Control.Lens.Lens

import Control.Lens.Types
import Data.Curried
import Data.Yoneda
import Data.Morphisms

%access public export

lens : (s -> a) -> (s -> b -> t) -> {f : Type -> Type}
    -> Functor f => Morphism a (f b) -> Morphism s (f t)
lens sa sbt (Mor afb) = Mor (\s => sbt s <$> afb (sa s))

-- | Fuse a composition of lenses using 'Yoneda' to provide 'map' fusion.
--
-- In general, given a pair of lenses 'foo' and 'bar'
--
-- @
-- fusing (foo.bar) = foo.bar
-- @
--
-- however, @foo@ and @bar@ are either going to 'map' internally or they are trivial.
--
-- 'fusing' exploits the 'Yoneda' lemma to merge these separate uses into a single 'map'.
--
-- This is particularly effective when the choice of functor 'f' is unknown at compile
-- time or when the 'Lens' @foo.bar@ in the above description is recursive or complex
-- enough to prevent inlining.
--
-- @
-- 'fusing' :: 'Lens' s t a b -> 'Lens' s t a b
-- @
fusing : { f : Type -> Type } -> Functor f => LensLike (Yoneda f) s t a b -> LensLike f s t a b
fusing t = \(Mor f) => Mor ( lowerYoneda . ( applyMor . t . Mor ) ( liftYoneda . f ) )

-- | "Fuse" a 'Traversal' by reassociating all of the '\<*\>' operations to the
-- left and fusing all of the 'fmap' calls into one. This is particularly
-- useful when constructing a 'Traversal' using operations from GHC.Generics.
--
-- Given a pair of 'Traversal's 'foo' and 'bar',
--
-- @
-- 'confusing' (foo.bar) = foo.bar
-- @
--
-- However, @foo@ and @bar@ are each going to use the 'Applicative' they are given.
--
-- 'confusing' exploits the 'Yoneda' lemma to merge their separate uses of 'fmap' into a single 'fmap'.
-- and it further exploits an interesting property of the right Kan lift (or 'Curried') to left associate
-- all of the uses of '(<*>)' to make it possible to fuse together more fmaps.
--
-- This is particularly effective when the choice of functor 'f' is unknown at compile
-- time or when the 'Traversal' @foo.bar@ in the above description is recursive or complex
-- enough to prevent inlining.
--
-- 'Control.Lens.Lens.fusing' is a version of this combinator suitable for fusing lenses.
--
-- @
-- 'confusing' :: 'Traversal' s t a b -> 'Traversal' s t a b
-- @
confusing : { f : Type -> Type } -> Applicative f => LensLike (Curried (Yoneda f) (Yoneda f)) s t a b -> LensLike f s t a b
confusing t = \(Mor f) => Mor (lowerYoneda . lowerCurried . ( applyMor . t . Mor ) (liftCurriedYoneda . f))
  where

    yap : Applicative f => Yoneda f (a -> b) -> f a -> Yoneda f b
    yap (MkYoneda k) fa = MkYoneda (\ab_r => k (ab_r .) <*> fa)

    liftCurriedYoneda : Applicative f => f a -> Curried (Yoneda f) (Yoneda f) a
    liftCurriedYoneda fa = MkCurried (flip yap fa)
