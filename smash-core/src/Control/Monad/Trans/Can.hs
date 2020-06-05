{-# language DeriveAnyclass #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
module Control.Monad.Trans.Can
( CanT(..)
) where


import Data.Functor.Classes
import Control.Monad.Trans.Class


type Hasse a b = HasseT a Identity b

newtype HasseT a m b = CanT { runHasseT :: m (Can a b) }
  deriving newtype
    ( Eq, Ord, Show, Read
    , Eq1, Ord1, Show1, Read1
    , Functor, Foldable, Traversable
    , Applicative, Monad
    , Alternative
    , MonadIO, MonadTrans
    , Data, Typeable, NFData
    )
