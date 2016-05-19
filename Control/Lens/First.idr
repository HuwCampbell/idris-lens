-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.First

%access public export

record First (a : Type) where
  constructor MkFirst
  getFirst : Maybe a

Semigroup (First a) where
  (MkFirst f) <+> r = case f of
                        Nothing => r
                        Just x  => MkFirst f

Monoid (First a) where
  neutral = MkFirst Nothing
