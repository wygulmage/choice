{-# LANGUAGE
    UnicodeSyntax
  , DefaultSignatures
  , DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , RankNTypes
  , ScopedTypeVariables
  , TypeOperators
  #-}

module Bichoice.Internal.Choice
   ( type (||), type (!!), type (!|), type (|!)
   , match, makeL, makeR
   )
   where

import Prelude (Eq ((==)), Ord (compare), Bool (..), Ordering (..), flip, seq)
import Control.Category ((.), id)
import Data.Functor.Classes (Eq1 (liftEq), Eq2 (liftEq2), Ord1 (liftCompare), Ord2 (liftCompare2), Read1 (liftReadPrec), Read2 (liftReadPrec2), Show2 (liftShowsPrec2), showsUnaryWith)
-- Semigroup classes
import Data.Semigroup (Semigroup ((<>)))
import Data.Monoid (Monoid (mempty))
import Data.Foldable (Foldable (foldMap, foldl, foldr))
import Data.Bifoldable (Bifoldable (bifoldMap, bifoldl, bifoldr))
-- Functor classes
import Data.Functor (Functor (fmap))
import Control.Applicative (Applicative ((<*>), liftA2, pure), Alternative ((<|>), empty))
import Control.Monad (Monad ((>>=)), MonadPlus)
import Control.Monad.Fail (MonadFail (fail))
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Traversable (Traversable (traverse, sequenceA))
import Data.Bitraversable (Bitraversable (bitraverse))

import Data.Data (Data (gfoldl, gunfold, toConstr), constrIndex)
import Data.Typeable (Typeable)
import Data.Either (Either (Left, Right), either)
import Data.Coerce (Coercible, coerce)


----- Class -----

class Choice or where
   -- An instance of Choice is more-or-less a co-Cartesian product in (→).
   makeL :: a → a `or` b
   default makeL :: Coercible Either or => a -> a `or` b
   makeL = (coerce :: Either a b -> a `or` b) . Left

   makeR :: b → a `or` b
   default makeR :: Coercible Either or => b → a `or` b
   makeR = (coerce :: Either a b -> a `or` b) . Right

   match :: (a → c) → (b → c) → a `or` b → c
   default match :: Coercible Either or => (a → c) → (b → c) → a `or` b → c
   match f g = either f g . coerce

   -- changeL :: (a -> a `or` c) -> a `or` c -> a `or` c
   -- default changeL :: Coercible Either or => (a -> a `or` c) -> a `or` c -> a `or` c
   -- changeL f = go . (coerce :: a `or` c -> Either a c)
   --    where
   --    go (Left x) = f x
   --    go rx = coerce rx

   -- changeR :: (a -> c `or` a) -> c `or` a -> c `or` a
   -- default changeR :: Coercible Either or => (a -> c `or` a) -> c `or` a -> c `or` a
   -- changeR f = go . (coerce :: c `or` a -> Either c a)
   --    where
   --    go (Right x) = f x
   --    go lx = coerce lx

----- Types -----

-- A choice of two lazy constructors:
-- newtype Lazy a b = Lazy (Either a b)
type Lazy = Either

-- A choice of two strict constructors:
newtype Strict a b = Strict (Either a b)
  deriving Typeable

-- A choice of a strict or a lazy constructor:
newtype LeftStrict a b = LeftStrict (Either a b )
  deriving Typeable

-- A choice of a lazy or a strict constructor:
newtype RightStrict a b = RightStrict (Either a b)
  deriving Typeable

-- A wrapper to create instances of other classes for all instances of Choice:
newtype Choose or a b = Choose{ getChoice :: a `or` b }
  deriving Typeable

--- Here are the types you'll actually use:
type (||) = Choose Lazy -- ^ a choice of two lazy constructors
type (!!) = Choose Strict -- ^ a choice of two strict constructors
type (!|) = Choose LeftStrict -- ^ a choice of one strict and one lazy constructor
type (|!) = Choose RightStrict -- ^ a choice of one lazy and one strict constructor

----- Functions -----

strictLeft :: a -> Either a b
-- ^ Strictly construct Left values. (strictLeft undefined = undefined)
strictLeft x = seq x (Left x)

strictRight :: b -> Either a b
-- ^ Strictly construct Right values. (strictRight undefined = undefined)
strictRight x = seq x (Right x)

-- fromL :: Choice or => c -> (a -> c) -> a `or` b_ -> c
-- -- ^ Return a default for R values and applay a function to L values.
-- fromL z f = match f (pure z)

fromR :: Choice or => c -> (b -> c) -> a_ `or` b -> c
-- ^ Return a default for L values and apply a function to R values (equivalent to 'maybe').
fromR = match . pure -- AKA 'maybe'

match2 ::
  (Choice or1, Choice or2) =>
  (a → b → c) → -- Combine Ls.
  (a → e → c) → -- Combine L R.
  (d → b → c) → -- Combine R L.
  (d → e → c) → -- Combine Rs.
  a `or1` d → b `or2` e → c
match2 fab fae fdb fde = match (bifoldlDefault fab fae) (bifoldlDefault fdb fde)

both :: (Choice or1, Choice or2) => c -> c -> (a -> b -> c) -> (d -> e -> c) -> a `or1` d -> b `or2` e -> c
both x y f = match2 f (pure (pure x)) (pure (pure y))

(.:) :: (c → d) → (a → b → c) → a → b → d
(.:) = (.) . (.)

----- Generalized Specialized Methods -----

bindL :: (Choice or) => (a → b `or` c) → a `or` c → b `or` c
bindL f = match f makeR

bindR :: (Choice or) => (a → c `or` b) → c `or` a → c `or` b
bindR = match makeL

mapR :: (Choice or) => (a -> b) -> c `or` a -> c `or` b
mapR f = bindR (makeR . f)

mapL :: (Choice or) => (a -> b) -> a `or` c -> b `or` c
mapL f = bindL (makeL . f)

bifoldlDefault :: Choice or => (c → a → d) → (c → b → d) → c → a `or` b → d
-- Match a choice with two dyadic functions and a value (useful for combining choice values; more liberal type than bifoldl).
bifoldlDefault f g z = match (f z) (g z)

foldlR :: Choice or => (c -> b -> c) -> c -> a `or` b -> c
foldlR = bifoldlDefault pure

-- foldlL :: Choice or => (c -> a -> c) -> c -> a `or` b -> c
-- foldlL f = bifoldlDefault f pure

zipLBy ::
   (Choice or) =>
   (a -> b -> c) -> (d -> d -> d) -> a `or` d -> b `or` d -> c `or` d
-- If you have a left and a right, return the right.
-- If you have two lefts, combine them with f.
-- If you have two rights, combine them in order with g.
zipLBy f g = match (mapL . f) (makeR .: foldlR g)

-- zipRBy ::
--    (Choice or) =>
--    (d -> d -> d) -> (a -> b -> c) -> d `or` a -> d `or` b -> d `or` c
-- -- If you have a left and a right, return the left.
-- -- If you have two lefts, combine them in order with f.
-- -- If you have two rights, combine them with g.
-- zipRBy f g = match (makeL .: foldlL f) (mapR . g)

bitraverseDefault ::
   (Choice or, Functor m) =>
   (a -> m b) -> (c -> m d) -> a `or` c -> m (b `or` d)
bitraverseDefault g h =
   match (fmap makeL . g) (fmap makeR . h)


----- Instances -----

-- instance Choice Lazy

instance Choice Either

instance Choice Strict where
   makeL = Strict . strictLeft
   makeR = Strict . strictRight

instance Choice LeftStrict where
   makeL = LeftStrict . strictLeft

instance Choice RightStrict where
   makeR = RightStrict . strictRight


--- Choose Instances ---

--- Basic Prelude-type classes:

instance Choice or => Choice (Choose or) where
   match f g = match f g . getChoice
   makeL = Choose . makeL
   makeR = Choose . makeR

instance (Choice or, Eq a, Eq b) => Eq (Choose or a b) where
   (==) = liftEq2 (==) (==)

instance Choice or => Eq2 (Choose or) where
   liftEq2 = both False False

instance (Choice or, Ord a, Ord b) => Ord (Choose or a b) where
   compare = liftCompare2 compare compare

instance Choice or => Ord2 (Choose or) where
   liftCompare2 = both LT GT

instance Choice or => Show2 (Choose or) where
   liftShowsPrec2 sp1 _ sp2 _ d =
      match (showsUnaryWith sp1 "makeL" d) (showsUnaryWith sp2 "makeR" d)

--- Semigroup classes:

instance (Choice or, Semigroup a, Semigroup b) => Semigroup (Choose or a b) where
   (<>) = zipLBy (<>) (<>)

instance (Choice or, Monoid a, Semigroup b) => Monoid (Choose or a b) where
   mempty = makeL mempty

instance Choice or => Foldable (Choose or c_) where
   foldMap = fromR mempty
   foldl = foldlR
   foldr = foldl . flip

instance Choice or => Bifoldable (Choose or) where
   bifoldMap = match
   bifoldl = bifoldlDefault
   bifoldr f g = bifoldl (flip f) (flip g)

--- Functor classes:

instance Choice or => Functor (Choose or c) where
   fmap = mapR

instance Choice or => Bifunctor (Choose or) where
   bimap f g = match (makeL . f) (makeR . g)
   -- bimap f g = match (makeL . f) (pure . g)
   first = mapL

instance Choice or => Applicative (Choose or c) where
   pure = makeR
   -- Must match Monad instance, which cannot combine L values.
   liftA2 f = match (pure . makeL) (fmap . f)
   -- liftA2 = zipLBy pure
   (<*>) = match (pure . makeL) fmap

instance Choice or => Monad (Choose or c) where
   (>>=) = flip bindR

instance Choice or => Traversable (Choose or c) where
   traverse f = match (pure . makeL) (fmap makeR . f)
   -- traverse f = match (pure . makeL) (fmap pure . f)
   -- bitraverse pure
   sequenceA = match (pure . makeL) (fmap makeR)
   -- sequenceA = match (pure . makeL) (fmap pure)

instance Choice or => Bitraversable (Choose or) where
   bitraverse = bitraverseDefault

instance (Choice or, Monoid a) => Alternative (Choose or a) where
   empty = makeL mempty
   -- Take the first R value or combine L values.
   (<|>) = zipLBy (<>) pure

instance (Choice or, Monoid a) => MonadPlus (Choose or a)

instance (Choice or, Monoid a) => MonadFail (Choose or a) where
   fail _ = empty

------------
