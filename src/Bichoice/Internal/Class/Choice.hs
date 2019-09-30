{-# LANGUAGE
    NoImplicitPrelude
  , DefaultSignatures
  , QuantifiedConstraints
  , TypeOperators
  #-}

module Bichoice.Internal.Class.Choice where
--- Semi-co-Cartesian products ---

import Control.Applicative (Applicative (pure))
import Data.Bifunctor (Bifunctor)
import Data.Functor (Functor)


class (Bifunctor p, forall a. Functor (p a)) => Choice p where
   makeL :: a -> p a c
   makeR :: a -> p c a

   default makeR :: Applicative (p c) => a -> p c a
   makeR = pure


----- Rules -----

makeL_RULE :: (Choice or) => a -> a `or` c
makeL_RULE = makeL
{-# INLINE [0] makeL_RULE #-}

makeR_RULE :: (Choice or) => a -> c `or` a
makeR_RULE = makeR
{-# INLINE [0] makeR_RULE #-}

{-# RULES
  "Don't inline makeL early." [~0]
  makeL = makeL_RULE
  ;
  "Don't inline makeR early." [~0]
  makeR = makeR_RULE
  #-}
