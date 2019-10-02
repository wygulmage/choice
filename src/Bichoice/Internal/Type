{-# LANGUAGE
    UnicodeSyntax
  , TypeOperators
  #-}

module Bichoice.Internal.Type.Choice where

import qualified Bichoice.Internal.Class.Choice as C
import qualified Bichoice.Internal.Class.Match as C
import Data.Bifunctor (Bifunctor (bimap, first))

newtype Choice or a b = Choice{ getChoice :: a `or` b }

instance C.Choice or => C.Choice (Choice or) where
   makeL = Choice . C.makeL
   makeR = Choice . C.makeR

instance C.Match or => C.Match (Choice or) where
   match f g = C.match f g . getChoice

instance C.Match or => Functor (Choice or c) where
   fmap = C.fmapDefault

instance C.Match or => Bifunctor (Choice or) where
   bimap = C.bimapDefault
   first = C.firstDefault
