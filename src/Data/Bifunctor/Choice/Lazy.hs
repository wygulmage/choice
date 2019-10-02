{-# LANGUAGE
    NoImplicitPrelude
  , DeriveGeneric
  , PatternSynonyms
  , QuantifiedConstraints
  , TypeOperators
  #-}

module Data.Bifunctor.Choice.Lazy
   ( type (||), makeL, makeR, match
   )
   where

import Bichoice.Internal.Choice

-- import Prelude (Eq ((==)), Read, Show (show))
-- import Bichoice.Internal.Class.Choice
-- import Bichoice.Internal.Class.Match
-- import Control.Applicative (Applicative ((<*>), pure))
-- import Control.Category ((.), id)
-- import Control.Monad (Monad ((>>=)))
-- import Data.Bifunctor (Bifunctor (bimap, first))
-- import Data.Functor (Functor (fmap))
-- import Data.Semigroup (Semigroup ((<>)))
-- import Data.Monoid (Monoid (mempty))
-- import Data.Bifoldable (Bifoldable (bifoldMap, bifoldl, bifoldr))
-- import Data.Foldable (Foldable (foldMap, foldl, foldr))
-- import Data.Bitraversable (Bitraversable (bitraverse))
-- import Data.Traversable (Traversable (traverse))

-- import Data.Either (Either (Left, Right))
-- import Data.Function (flip)
-- import Data.Typeable (Typeable)
-- import GHC.Generics (Generic, Generic1)

-- -- data a || b = L a | R b
-- newtype a || b = Choice{ getChoice :: Either a b }
--    deriving (Generic, Generic1, Read, Typeable)
-- infixr 2 ||

-- instance (Show a, Show b) => Show (a || b) where
--   show = match (("makeL" <>) . show) (("makeR" <>) . show)

-- instance (Eq a, Eq b) => Eq (a || b) where (==) = liftEq2Default (==) (==)

-- instance Functor ((||) c) where fmap = fmapDefault

-- instance Bifunctor (||) where
--    bimap = bimapDefault
--    first = firstDefault

-- instance Choice (||) where
--    makeL = Choice . Left
--    makeR = Choice . Right

-- instance Match (||) where
--    match f g = go . getChoice
--       where
--       go (Left x) = f x
--       go (Right y) = g y

-- instance Monad ((||) c) where
--    (>>=) = flip bindDefault

-- instance Applicative ((||) c) where
--    pure = makeR
--    (<*>) = apDefault

-- instance (Semigroup a, Semigroup b) => Semigroup (a || b) where
--    (<>) = semigroupDefault

-- instance (Monoid a, Semigroup b) => Monoid (a || b) where
--    mempty = memptyDefault

-- instance Foldable ((||) c_) where
--    foldMap = foldMapDefault
--    foldl = foldlDefault
--    foldr = foldrDefault

-- instance Bifoldable (||) where
--    bifoldMap = match
--    bifoldl = bifoldlDefault
--    bifoldr = bifoldrDefault

-- instance Traversable ((||) c) where
--    traverse = traverseDefault

-- instance Bitraversable (||) where
--    bitraverse = bitraverseDefault


-- -----

-- type Maybe a = () || a
-- pattern Nothing :: Maybe a
-- pattern Nothing = Choice (Left ())
-- pattern Just x = Choice (Right x)
