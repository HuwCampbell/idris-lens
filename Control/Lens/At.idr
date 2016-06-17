-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.At

import Control.Lens.Setter
import Control.Lens.Types
import Control.Monad.Identity
import Data.Profunctor
import Prelude.List

%default total
%access public export

interface Ixed ( m : Type ) ind val  where
  ix : ind -> { f : Type -> Type } -> Applicative f => LensLike' f m val

implementation Ixed (List a) Nat a where
  ix k (MkArrow f) = MkArrow (\xs0 => go xs0 k)
    where
      go Nil _           = pure Nil
      go (a :: as) Z     = (:: as) <$> (f a)
      go (a :: as) (S n) = (a ::)  <$> (go as n)

implementation Ixed (Maybe a) Unit a where
  ix () (MkArrow f)  = MkArrow (\g => case g of
    (Just a) => Just <$> f a
    Nothing  => pure Nothing
  )

interface Ixed m ind val => At m ind val where
  -- |
  -- >>> Map.fromList [(1,"world")] ^.at 1
  -- Just "world"
  --
  -- >>> at 1 ?~ "hello" $ Map.empty
  -- fromList [(1,"hello")]
  --
  -- /Note:/ 'Map'-like containers form a reasonable instance, but not 'Array'-like ones, where
  -- you cannot satisfy the 'Lens' laws.
  at : ind -> { f : Type -> Type } -> Applicative f => LensLike' f m (Maybe val)

implementation At (Maybe a) Unit a where
  at _ (MkArrow f) = (MkArrow f)


-- --------------------------------------------------------------------- [ EOF ]
