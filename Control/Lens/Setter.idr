-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.Setter
import Control.Lens.Types
import Control.Monad.Identity

%default total
%access public export

sets : ((a -> b) -> s -> t) -> Setter s t a b
sets l f = Id . l (runIdentity . f)

over : Setter s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Id . f)

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

mapped : Functor f => (a -> Identity b) -> f a -> Identity (f b)
mapped = sets map

-- --------------------------------------------------------------------- [ EOF ]
