module Control.Lens.Market

import Data.Profunctor

%default total
%access public export


||| This type is used internally by the 'Control.Lens.Prism' code to
||| provide efficient access to the two parts of a 'Prism'.
data Market a b s t = MkMarket (b -> t) (s -> Either t a)

Market' : Type -> Type -> Type -> Type
Market' a = Market a a

Functor (Market a b s) where
  map f (MkMarket bt seta) = MkMarket (f . bt) (either (Left . f) Right . seta)

Profunctor (Market a b) where
  dimap f g (MkMarket bt seta) = MkMarket (g . bt) (either (Left . g) Right . seta . f)

  lmap f (MkMarket bt seta) = MkMarket bt (seta . f)

  rmap f (MkMarket bt seta) = MkMarket (f . bt) (either (Left . f) Right . seta)


Choice (Market a b) where
  left' (MkMarket bt seta) = MkMarket (Left . bt) $ \sc => case sc of
    Left s => case seta s of
      Left t => Left (Left t)
      Right a => Right a
    Right c => Left (Right c)

  right' (MkMarket bt seta) = MkMarket (Right . bt) $ \cs => case cs of
    Left c => Left (Left c)
    Right s => case seta s of
      Left t => Left (Right t)
      Right a => Right a

