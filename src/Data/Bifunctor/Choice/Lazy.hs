{-# LANGUAGE
    NoImplicitPrelude
  , DeriveGeneric
  , QuantifiedConstraints
  , TypeOperators
  #-}

module Data.Bifunctor.Choice.Lazy where

import Prelude (Read, Show)
import Choice.Internal.Class.Choice
import Choice.Internal.Class.Match
import Control.Applicative (Applicative ((<*>), pure))
import Control.Category ((.), id)
import Control.Monad (Monad ((>>=)))
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Functor (Functor (fmap))
import Data.Semigroup (Semigroup ((<>)))
import Data.Monoid (Monoid (mempty))
import Data.Bifoldable (Bifoldable (bifoldMap, bifoldl, bifoldr))
import Data.Foldable (Foldable (foldMap, foldl, foldr))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Traversable (Traversable (traverse))

import Data.Function (flip)
import Data.Typeable (Typeable)
import GHC.Generics (Generic, Generic1)

data a || b = L a | R b
   deriving (Generic, Generic1, Read, Show, Typeable)
infixr 2 ||

instance Functor ((||) c) where fmap = fmapDefault

instance Bifunctor (||) where
   bimap = bimapDefault
   first = firstDefault

instance Choice (||) where
   makeL = L
   makeR = R

instance Match (||) where
   match f g = go
      where
      go (L x) = f x
      go (R y) = g y

instance Monad ((||) c) where
   (>>=) = flip bindDefault

instance Applicative ((||) c) where
   pure = makeR
   (<*>) = apDefault

instance (Semigroup a, Semigroup b) => Semigroup (a || b) where
   (<>) = semigroupDefault

instance (Monoid a, Semigroup b) => Monoid (a || b) where
   mempty = memptyDefault

instance Foldable ((||) c_) where
   foldMap = foldMapDefault
   foldl = foldlDefault
   foldr = foldrDefault

instance Bifoldable (||) where
   bifoldMap = match
   bifoldl = bifoldlDefault
   bifoldr = bifoldrDefault

instance Traversable ((||) c) where
   traverse = traverseDefault

instance Bitraversable (||) where
   bitraverse = bitraverseDefault
