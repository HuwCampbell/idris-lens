-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.Tuple
import Control.Lens.Types

%default total
%access public export

--
-- Note: Tuples in Idris are nested `Pair`s, so these don't currently act like
-- their haskell counterparts, e.g., `_2` on `(1,2,3)` will focus on `(2,3)`.
--

_1 : Lens (a,c) (b,c) a b
_1 f (a,c) = (\b => (b,c)) <$> f a

_2 : Lens (c,a) (c,b) a b
_2 f (c,a) = (\b => (c,b)) <$> f a

both : Traversal (a,a) (b,b) a b
both f (a,b) = (\c,d => (c,d)) <$> f a <*> f b

-- --------------------------------------------------------------------- [ EOF ]
