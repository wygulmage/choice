{-# LANGUAGE
    NoImplicitPrelude
  , QuantifiedConstraints
  , TypeOperators
  #-}

module Data.Bifunctor.Choice
   ( type (||), type (!!), type (!|)
   , makeL, makeR, match
   )
   where

import Data.Bifunctor.Choice.Lazy
import Data.Bifunctor.Choice.Strict
import Data.Bifunctor.Choice.LeftStrict
