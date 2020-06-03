module Control.Monad.Can where




newtype CanT a m b = CanT
  { _runCanT :: m (Can a b)
  } deriving
