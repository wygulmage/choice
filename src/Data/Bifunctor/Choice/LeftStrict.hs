{-# LANGUAGE
    NoImplicitPrelude
  , DefaultSignatures
  , QuantifiedConstraints
  , TypeOperators
  #-}

module Data.Bifunctor.Choice.LeftStrict
   ( type (!|), makeL, makeR, match
   )
   where

import Bichoice.Internal.Class.Choice
import Bichoice.Internal.Class.Match
import Prelude (Eq ((==)), Ord (compare), Read, Show, seq)
import Control.Applicative (Applicative ((<*>), pure))
import Control.Category ((.), id)
import Control.Monad (Monad ((>>=)))
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Functor (Functor (fmap))
import Data.Functor.Classes (Eq2 (liftEq2), eq2, Ord2 (liftCompare2), compare2)
import Data.Semigroup (Semigroup ((<>)))
import Data.Monoid (Monoid (mempty))
import Data.Bifoldable (Bifoldable (bifoldMap, bifoldl, bifoldr))
import Data.Foldable (Foldable (foldMap, foldl, foldr))
import Data.Traversable (Traversable (traverse))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Typeable (Typeable)

import Data.Either (Either (Left, Right))
import Data.Function (flip)


-- data a !| b = StrictL !a | LazyR b
newtype a !| b = StrictLeft{ getStrictLeft :: Either a b}
   deriving (Read, Show, Typeable)
infixr 2 !|

instance Functor ((!|) c) where fmap = fmapDefault

instance Bifunctor (!|) where
   bimap = bimapDefault
   first = firstDefault

instance Choice (!|) where
   makeL x = x `seq` StrictLeft (Left x)
   makeR = StrictLeft . Right

instance Match (!|) where
   match f g = go . getStrictLeft
      where
      go (Left x) = f x
      go (Right y) = g y

instance Applicative ((!|) c) where
   pure = makeR
   (<*>) = apDefault

instance Monad ((!|) c) where (>>=) = flip bindDefault

instance (Semigroup a, Semigroup b) => Semigroup (a !| b) where (<>) = semigroupDefault

instance (Monoid a, Semigroup b) => Monoid (a !| b) where mempty = memptyDefault

instance Foldable ((!|) c_) where
   foldMap = foldMapDefault
   foldl = foldlDefault
   foldr = foldrDefault

instance Bifoldable (!|) where
   bifoldMap = match
   bifoldl = bifoldlDefault
   bifoldr = bifoldrDefault

instance Traversable ((!|) c) where traverse = traverseDefault

instance Bitraversable (!|) where bitraverse = bitraverseDefault

instance (Eq a, Eq b) => Eq (a !| b) where (==) = eq2

instance Eq2 (!|) where liftEq2 = liftEq2Default

instance (Ord a, Ord b) => Ord (a !| b) where compare = compare2

instance Ord2 (!|) where liftCompare2 = liftCompare2Default
