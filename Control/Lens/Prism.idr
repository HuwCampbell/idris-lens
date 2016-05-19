-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Applicative
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.Prism
import Control.Lens.Types
import Control.Monad.Identity

%default total
%access public export

||| Create a `Prism`
prism : (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap' seta (either pure (map bt)) . map where
  dimap' : (a -> b) -> (c -> d) -> (b -> c) -> (a -> d)
  dimap' ab cd bc = cd . bc . ab

prism' : (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' bs sma = prism bs (\s => maybe (Left s) Right (sma s))

_Left : Prism (Either a c) (Either b c) a b
_Left = prism Left $ either Right (Left . Right)

_Right : Prism (Either c a) (Either c b) a b
_Right = prism Right $ either (Left . Left) Right

_Just : Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ maybe (Left Nothing) Right

_Nothing : Prism' (Maybe a) ()
_Nothing = prism' (const Nothing) $ maybe (Just ()) (const Nothing)

-- --------------------------------------------------------------------- [ EOF ]
