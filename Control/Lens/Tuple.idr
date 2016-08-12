-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.Tuple
import Control.Lens.Types
import Control.Lens.Lens
import Data.Profunctor

%default total
%access public export

--
-- Note: Tuples in Idris are nested `Pair`s, so these don't currently act like
-- their haskell counterparts, e.g., `_2` on `(1,2,3)` will focus on `(2,3)`.
--

_1 : Lens (a,c) (b,c) a b
_1 = lens (\(a,_) => a)
          (\(_,c),b => (b,c))

_2 : Lens (c,a) (c,b) a b
_2 = lens (\(_,a) => a)
          (\(c,_),b => (c,b))

both : Traversal (a,a) (b,b) a b
both (Mor f) = Mor (\(a,b) => (\c,d => (c,d)) <$> f a <*> f b)

-- --------------------------------------------------------------------- [ EOF ]
