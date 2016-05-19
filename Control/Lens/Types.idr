-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.Types
import Control.Lens.Const
import Control.Lens.First
import Control.Monad.Identity

%default total
%access public export

-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
Lens : Type -> Type -> Type -> Type -> Type
Lens s t a b  = { f : Type -> Type } -> Functor f => (a -> f b) -> s -> f t

-- type Lens' s a = Lens s s a a
Lens' : Type -> Type -> Type
Lens' s a  = Lens s s a a

-- type Getting r s a = (a -> Const r a) -> s -> Const r s
Getter : Type -> Type -> Type -> Type
Getter r s a = (a -> Const r a) -> s -> Const r s

-- type ASetter s t a b = (a -> Identity b) -> s -> Identity t
Setter : Type -> Type -> Type -> Type -> Type
Setter s t a b = (a -> Identity b) -> s -> Identity t

-- type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
Traversal : Type -> Type -> Type -> Type -> Type
Traversal s t a b  = { f : Type -> Type } -> Applicative f => (a -> f b) -> s -> f t

-- type Traversal' s a = Traversal s s a a
Traversal' : Type -> Type -> Type
Traversal' s a = Traversal s s a a

-- type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
-- The choice of `Choice p` for a valid `Traversal` is the `(->)` implementation. Which is used
-- when using a prism as a getter or setter.
--
-- Unfortunately in dependently typed languages `(->)` is not a type constructor, and can
-- not have any class instance (aka interface implemenations).
--
-- Here we've fixed the choice as `(->)` making `Traversal` and `Prism` one and the same.
-- This means we lose the ability to "turn them around" and use them as a `Review`.

Prism : Type -> Type -> Type -> Type -> Type
Prism s t a b  = { f : Type -> Type } -> Applicative f => (a -> (f b)) -> (s -> (f t))

Prism' : Type -> Type -> Type
Prism' s a = Prism s s a a

LensLike : (Type -> Type) -> Type -> Type -> Type -> Type -> Type
LensLike f s t a b  = (a -> f b) -> s -> f t

LensLike' : (Type -> Type) -> Type -> Type -> Type
LensLike' f s a  = LensLike f s s a a

-- --------------------------------------------------------------------- [ EOF ]
