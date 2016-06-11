-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Data.Contravariant

%access public export

interface Contravariant (f : Type -> Type) where
  contramap : (b -> a) -> f a -> f b

-- --------------------------------------------------------------------- [ EOF ]
