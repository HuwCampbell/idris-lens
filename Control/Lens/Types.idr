-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.Types
import Control.Lens.Const
import Control.Lens.First
import Control.Monad.Identity
import Data.Profunctor

%default total
%access public export

Optic : (Type -> Type -> Type) -> (Type -> Type) -> Type -> Type -> Type -> Type -> Type
Optic p f s t a b = p a (f b) -> p s (f t)

Simple : (Type -> Type -> Type -> Type -> Type) -> Type -> Type -> Type
Simple p s a = p s s a a

Optic' : (Type -> Type -> Type) -> (Type -> Type) -> Type -> Type -> Type
Optic' p f = Simple (Optic p f)

LensLike : (Type -> Type) -> Type -> Type -> Type -> Type -> Type
LensLike = Optic Arrow

LensLike' : (Type -> Type) -> Type -> Type -> Type
LensLike' f = Simple (LensLike f)


-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
Lens : Type -> Type -> Type -> Type -> Type
Lens s t a b  = { f : Type -> Type } -> Functor f => LensLike f s t a b

-- type Lens' s a = Lens s s a a
Lens' : Type -> Type -> Type
Lens' = Simple Lens

-- type Getting r s a = (a -> Const r a) -> s -> Const r s
Getter : Type -> Type -> Type -> Type
Getter r = LensLike' (Const r)

-- type ASetter s t a b = (a -> Identity b) -> s -> Identity t
Setter : Type -> Type -> Type -> Type -> Type
Setter = LensLike Identity

-- type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
Traversal : Type -> Type -> Type -> Type -> Type
Traversal s t a b  = { f : Type -> Type } -> Applicative f => LensLike f s t a b

-- type Traversal' s a = Traversal s s a a
Traversal' : Type -> Type -> Type
Traversal' = Simple Traversal

-- type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
-- The choice of `Choice p` for a valid `Traversal` is the `(->)` implementation. Which is used
-- when using a prism as a getter or setter.

Prism : Type -> Type -> Type -> Type -> Type
Prism s t a b  = {p : Type -> Type -> Type} -> { f : Type -> Type } ->
                 (Choice p,Applicative f) => Optic p f s t a b

Prism' : Type -> Type -> Type
Prism' = Simple Prism

Iso : Type -> Type -> Type -> Type -> Type
Iso s t a b = {p : Type -> Type -> Type} -> {f : Type -> Type} ->
              (Profunctor p,Functor f) => Optic p f s t a b

Iso' : Type -> Type -> Type
Iso' = Simple Iso

-- --------------------------------------------------------------------- [ EOF ]
