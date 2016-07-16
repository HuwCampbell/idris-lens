-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.Setter
import Control.Lens.Types
import Control.Monad.Identity
import Data.Profunctor

%default total
%access public export

sets : ((a -> b) -> s -> t) -> Setter s t a b
sets l (Mor f) = Mor $ Id . l (runIdentity . f)

over : Setter s t a b -> (a -> b) -> s -> t
over l f = runIdentity . applyMor (l (Mor (Id . f)))

set : Setter s t a b -> b -> s -> t
set l b = over l (const b)

infixr 4 .~
(.~) : Setter s t a b -> b -> s -> t
(.~) = set

infixr 4 &~

(&~) : Setter s t a b -> (a -> b) -> s -> t
(&~) = over

infixl 1 &
(&) : a -> (a -> b) -> b
(&) a f = f a

mapped : Functor f => LensLike Identity (f a) (f b) a b
mapped = sets map

-- --------------------------------------------------------------------- [ EOF ]
