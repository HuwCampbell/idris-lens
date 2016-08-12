module Control.Lens.Lens

import Data.Morphisms

%access public export

lens : (s -> a) -> (s -> b -> t) -> {f : Type -> Type}
    -> Functor f => Morphism a (f b) -> Morphism s (f t)
lens sa sbt (Mor afb) = Mor (\s => sbt s <$> afb (sa s))
