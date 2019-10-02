{-# LANGUAGE
    UnicodeSyntax
  , DefaultSignatures
  , FlexibleContexts
  , RankNTypes
  , ScopedTypeVariables
  , TypeOperators
  #-}

module Bichoice.Internal.Choice
   ( type (||), type (!!), type (!|), type (|!)
   , match, makeL, makeR
   )
   where

-- Semigroup classes
import Data.Semigroup (Semigroup ((<>)))
import Data.Monoid (Monoid (mempty))
import Data.Foldable (Foldable (foldMap, foldl, foldr))
import Data.Bifoldable (Bifoldable (bifoldMap, bifoldl, bifoldr))
-- Functor classes
import Data.Functor (Functor (fmap))
import Control.Applicative (Applicative ((<*>), liftA2, pure))
import Control.Monad (Monad ((>>=)))
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Traversable (Traversable (traverse))
import Data.Bitraversable (Bitraversable (bitraverse))

import Data.Either (Either (Left, Right), either)
import Data.Coerce (Coercible, coerce)


----- Class -----

class Choice or where
   makeL :: a → a  `or` b_
   makeR :: b → a_ `or` b
   match :: (a → c) → (b → c) → a `or` b → c

   default match :: Coercible Either or => (a → c) → (b → c) → a `or` b → c
   match f g = either f g . coerce

----- Types -----

newtype Lazy a b = Lazy{ getLazy :: Either a b }

newtype Strict a b = Strict{ getStrict :: Either a b }

newtype LeftStrict a b = LeftStrict{ getLeftStrict :: Either a b }

newtype RightStrict a b = RightStrict{ getRightStrict :: Either a b }

newtype Choose or a b = Choose{ getChoice :: Choice or => a `or` b }

type (||) = Choose Lazy
type (!!) = Choose Strict
type (!|) = Choose LeftStrict
type (|!) = Choose RightStrict

----- Functions -----

strictLeft :: a -> Either a b
strictLeft x = seq x (Left x)

strictRight :: b -> Either a b
strictRight x = seq x (Right x)

bind :: Choice or => (a → c `or` b) → c `or` a → c `or` b
bind = match makeL

(.:) :: (c → d) → (a → b → c) → a → b → d
(.:) = (.) . (.)


----- Instances -----

instance Choice Lazy where
   -- match f g = either f g . getLazy
   makeL = Lazy . Left
   makeR = Lazy . Right

instance Choice Strict where
   -- match f g = either f g . getStrict
   makeL = Strict . strictLeft
   makeR = Strict . strictRight

instance Choice LeftStrict where
   -- match f g = either f g . getLeftStrict
   makeL = LeftStrict . strictLeft
   makeR = LeftStrict . Right

instance Choice RightStrict where
   makeL = RightStrict . Left
   makeR = RightStrict . strictRight

instance Choice or => Choice (Choose or) where
   match f g = match f g . getChoice
   makeL = Choose . makeL
   makeR = Choose . makeR


instance Choice or => Functor (Choose or c) where
   fmap f = bind (makeR . f)

instance Choice or => Bifunctor (Choose or) where
   bimap f g = match (makeL . f) (makeR . g)
   first f = match (makeL . f) makeR

instance (Choice or, Semigroup a, Semigroup b) => Semigroup (Choose or a b) where
   (<>) =
      match (first . (<>)) (makeR .: foldl (<>))

instance (Choice or, Monoid a, Semigroup b) => Monoid (Choose or a b) where
   mempty = makeL mempty

instance Choice or => Foldable (Choose or c_) where
   foldMap = match mempty
   foldl = bifoldl pure
   foldr f = foldl (flip f)

instance Choice or => Bifoldable (Choose or) where
   bifoldMap = match
   bifoldl f g z = match (f z) (g z)
   bifoldr f g = bifoldl (flip f) (flip g)

instance Choice or => Traversable (Choose or c) where
   traverse f = match (pure . makeL) (fmap makeR . f)
   -- bitraverse pure

instance Choice or => Bitraversable (Choose or) where
   bitraverse f g = match (fmap makeL . f) (fmap makeR . g)

instance Choice or => Applicative (Choose or c) where
   pure = makeR
   liftA2 f = match (pure . makeL) (fmap . f)

instance Choice or => Monad (Choose or c) where
   (>>=) = flip bind
