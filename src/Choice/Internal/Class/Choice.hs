{-# LANGUAGE
    NoImplicitPrelude
  , DefaultSignatures
  , QuantifiedConstraints
  , TypeOperators
  #-}

module Choice.Internal.Class.Choice where
--- Semi-co-Cartesian products ---

import Control.Applicative (Applicative (pure))
import Data.Bifunctor (Bifunctor)
import Data.Functor (Functor)


class (Bifunctor p, forall a. Functor (p a)) => Choice p where
   makeL :: a -> p a c
   makeR :: a -> p c a

   default makeR :: Applicative (p c) => a -> p c a
   makeR = pure
