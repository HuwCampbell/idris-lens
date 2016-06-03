-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.Examples
import Control.Lens

--
-- Examples of interactions available with these Lenses
--

Ex1 : Maybe Int
Ex1 = over mapped (+1) (Just 2)

Ex1Proof : Ex1 = Just 3
Ex1Proof = Refl

Ex2 : (Int, Int)
Ex2 = (2,3) & over both (+1)

Ex2Proof : Ex2 = (3, 4)
Ex2Proof = Refl

Ex3 : (String, String)
Ex3 = ("Hello",2) & _2 .~ "World!"

Ex3Proof : Ex3 = ("Hello","World!")
Ex3Proof = Refl

Ex4 : Either String Int
Ex4 = f (Left "hi")
  where f : Either String Int -> Either String Int
        f = over _Right (+1)

Ex4Proof : Ex4 = (Left "hi")
Ex4Proof = Refl

Ex5 : Either String Int
Ex5 = f (Right 4)
  where f : Either String Int -> Either String Int
        f = over _Right (+1)

Ex5Proof : Ex5 = (Right 5)
Ex5Proof = Refl

Ex6 : Either (String, Int) Int
Ex6 = f (Left ("hi", 2))
  where f : Either (String,Int) Int -> Either (String,Int) Int
        f = over (_Left . _2) (+1)

Ex6Proof : Ex6 = Left ("hi", 3)
Ex6Proof = Refl

Ex7 : Additive
Ex7 = f (Left ("hi", GetAdditive 2)) where
  f : Either (String, Additive) Int -> Additive
  f = view (_Left . _2)

Ex7Proof : Ex7 = (GetAdditive 2)
Ex7Proof = Refl

Ex8 : Additive
Ex8 = f (Right 2) where
  f : Either (String, Additive) Int -> Additive
  f = view (_Left . _2)

Ex8Proof : Ex8 = (GetAdditive 0)
Ex8Proof = Refl

Ex9 : Maybe String
Ex9 = g ^? _Right . _1 where
 g : Either String (String, Int)
 g = Right ("x",2)

Ex9Proof : Ex9 = Just "x"
Ex9Proof = Refl

Ex10 : Maybe ()
Ex10 = Just "x" ^? _Nothing

Ex10Proof : Ex10 = Nothing
Ex10Proof = Refl

Ex11 : Maybe ()
Ex11 = n ^? _Nothing where
  n : Maybe String
  n = Nothing

Ex11Proof : Ex11 = Just ()
Ex11Proof = Refl

Ex12 : Int
Ex12 = view (to fst) (1,2)

Ex12Proof : Ex12 = 1
Ex12Proof = Refl

-- --------------------------------------------------------------------- [ EOF ]
