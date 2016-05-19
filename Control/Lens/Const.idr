-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.Const
import Control.Lens.Contravariant

%access public export

||| Const Functor.
data Const : (a : Type) -> (b : Type) -> Type where
  MkConst: a -> Const a b

getConst : Const a b -> a
getConst (MkConst x) = x

Functor (Const a) where
  map _ (MkConst x) = (MkConst x)

Contravariant (Const a) where
  contramap _ (MkConst x) = (MkConst x)

implementation Monoid m => Applicative (Const m) where
  pure _                      = MkConst neutral
  (MkConst f) <*> (MkConst v) = MkConst (f <+> v)


-- --------------------------------------------------------------------- [ EOF ]
