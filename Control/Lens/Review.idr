module Control.Lens.Review
import Data.Contravariant
import Control.Lens.Types
import Control.Lens.Getter
import Control.Monad.Identity
import Data.Profunctor
import Data.Tagged

%default total
%access public export

infixr 8 #

(#) : AReview t b -> b -> t
(#) p = runIdentity . untag . p . MkTagged . Id

re : AReview t b -> Getter b t
re p = to (p #)
