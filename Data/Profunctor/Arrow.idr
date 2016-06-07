module Data.Profunctor.Arrow

%access public export

data Arrow a b = MkArrow (a -> b)

getArrow : Arrow a b -> a -> b
getArrow (MkArrow f) = f

lens : (s -> a) -> (s -> b -> t) -> {f : Type -> Type}
    -> Functor f => Arrow a (f b) -> Arrow s (f t)
lens sa sbt (MkArrow afb) = MkArrow (\s => sbt s <$> afb (sa s))
