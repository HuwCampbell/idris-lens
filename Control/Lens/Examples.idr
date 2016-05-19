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
Ex4 = over _Right (+1) (Left "hi")

Ex4Proof : Ex4 = (Left "hi")
Ex4Proof = Refl

Ex5 : Either String Int
Ex5 = over _Right (+1) (Right 4)

Ex5Proof : Ex5 = (Right 5)
Ex5Proof = Refl

Ex6 : Either (String, Int) Int
Ex6 = over (_Left . _2) (+1) (Left ("hi", 2))

Ex6Proof : Ex6 = Left ("hi", 3)
Ex6Proof = Refl

Ex7 : Additive
Ex7 = view (_Left . _2) x where
  x : Either (String, Additive) Int
  x = Left ("hi", GetAdditive 2)

Ex7Proof : Ex7 = (GetAdditive 2)
Ex7Proof = Refl

Ex8 : Additive
Ex8 = view (_Left . _2) x where
  x : Either (String, Additive) Int
  x = Right 2

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

-- --------------------------------------------------------------------- [ EOF ]
