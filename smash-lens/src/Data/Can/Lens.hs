module Data.Can.Lens where


_Non :: Prism (Can a b) (Can c d) () ()
_Non = prism (const Non) $ \case
  Non -> Right ()
  t -> Left t

_One :: Prism (Can a c) (Can b c) a b
_One = prism One $ \case
  One a -> Right a
  t -> Left t

_Eno :: Prism (Can c a) (Can c b) a b
_Eno = prism Eno $ \case
  Eno a -> Right a
  t -> Left t

_Two :: Prism (Can a b) (Can c d) (a,b) (c,d)
_Two = prism (curr Two) $ \case
  Two a b -> Right (a,b)
  t -> Left t
