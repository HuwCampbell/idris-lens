-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.Maths
import Control.Lens.Types
import Control.Lens.Setter

%default total
%access public export

infixr 4 +~
(+~) : Num a => Setter s t a a -> a -> s -> t
l +~ n = over l (+ n)

infixr 4 *~
(*~) : Num a => Setter s t a a -> a -> s -> t
l *~ n = over l (* n)

infixr 4 -~
(-~) : (Neg a, Num a) => Setter s t a a -> a -> s -> t
l -~ n = over l (\x => x - n)

infixr 4 //~
(//~) : Fractional a => Setter s t a a -> a -> s -> t
l //~ n = over l (/ n)

-- --------------------------------------------------------------------- [ EOF ]
