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

import Prelude (Eq ((==)), Ord (compare), Bool (..), Ordering (..), flip, seq)
import Control.Category ((.), id)
import Data.Functor.Classes (Eq2 (liftEq2), Ord2 (liftCompare2))
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

-- newtype Lazy a b = Lazy (Either a b)
type Lazy = Either

newtype Strict a b = Strict (Either a b)

newtype LeftStrict a b = LeftStrict (Either a b )

newtype RightStrict a b = RightStrict (Either a b)

newtype Choose or a b = Choose{ getChoice :: a `or` b }

type (||) = Choose Lazy
type (!!) = Choose Strict
type (!|) = Choose LeftStrict
type (|!) = Choose RightStrict

----- Functions -----

strictLeft :: a -> Either a b
strictLeft x = seq x (Left x)

strictRight :: b -> Either a b
strictRight x = seq x (Right x)

fromL :: Choice or => c -> (a -> c) -> a `or` b_ -> c
fromL z f = match f (pure z)

fromR :: Choice or => c -> (b -> c) -> a_ `or` b -> c
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

bifoldlDefault :: Choice or => (c → a → d) → (c → b → d) → c → a `or` b → d
-- More liberal type than bifoldl.
bifoldlDefault f g z = match (f z) (g z)

foldlDefault :: Choice or => (c -> b -> c) -> c -> a `or` b -> c
foldlDefault = bifoldlDefault pure

bind :: (Choice or1, Choice or2) => (a → c `or2` b) → c `or1` a → c `or2` b
bind = match makeL

fmapDefault :: (Choice or1, Choice or2) => (a -> b) -> c `or1` a -> c `or2` b
fmapDefault f = bind (makeR . f)

firstDefault :: (Choice or1, Choice or2) => (a -> b) -> a `or1` c -> b `or2` c
firstDefault f = match (makeL . f) makeR

zipLBy ::
   (Choice or1, Choice or2, Choice or3) =>
   (a -> b -> c) -> (d -> d -> d) -> a `or1` d -> b `or2` d -> c `or3` d
-- If you have a left and a right, return the right.
-- If you have two lefts, combine them with f.
-- If you have two rights, combine them in order with g.
zipLBy f g = match (firstDefault . f) (makeR .: foldlDefault g)

zipRBy ::
   (Choice or1, Choice or2, Choice or3) =>
   (d -> d -> d) -> (a -> b -> c) -> d `or1` a -> d `or2` b -> d `or3` c
-- If you have a left and a right, return the left.
-- If you have two lefts, combine them in order with f.
-- If you have two rights, combine them with g.
zipRBy f g = match (makeL .: bifoldlDefault f pure) (fmapDefault . g)

bitraverseBy ::
   (Choice or1, Choice or2) =>
   (forall α β. (α -> β) -> m α -> m β) -> -- fmap
   (a -> m b) -> (c -> m d) -> a `or1` c -> m (b `or2` d)
-- 'a' for 'affine'
bitraverseBy f g h =
   match (f makeL . g) (f makeR . h)



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

--- Semigroup classes:

instance (Choice or, Semigroup a, Semigroup b) => Semigroup (Choose or a b) where
   (<>) = zipLBy (<>) (<>)

instance (Choice or, Monoid a, Semigroup b) => Monoid (Choose or a b) where
   mempty = makeL mempty

instance Choice or => Foldable (Choose or c_) where
   foldMap = fromR mempty
   foldl = bifoldl pure
   foldr = foldl . flip

instance Choice or => Bifoldable (Choose or) where
   bifoldMap = match
   bifoldl = bifoldlDefault
   bifoldr f g = bifoldl (flip f) (flip g)

--- Functor classes:

instance Choice or => Functor (Choose or c) where
   fmap = fmapDefault

instance Choice or => Bifunctor (Choose or) where
   bimap f g = match (makeL . f) (makeR . g)
   first = firstDefault

instance Choice or => Applicative (Choose or c) where
   pure = makeR
   liftA2 f = match (pure . makeL) (fmap . f)
   -- liftA2 = zipLBy pure

instance Choice or => Monad (Choose or c) where
   (>>=) = flip bind

instance Choice or => Traversable (Choose or c) where
   traverse f = match (pure . makeL) (fmap makeR . f)
   -- bitraverse pure

instance Choice or => Bitraversable (Choose or) where
   bitraverse = bitraverseBy fmap
