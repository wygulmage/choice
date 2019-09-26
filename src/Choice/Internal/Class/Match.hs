{-# LANGUAGE
    NoImplicitPrelude
  , QuantifiedConstraints
  , TypeOperators
  #-}

module Choice.Internal.Class.Match where
--- co-Cartesian products ---

import Choice.Internal.Class.Choice
import Control.Applicative (Applicative (pure))
import Control.Category ((.), id)
import Data.Functor (Functor (fmap))
import Data.Semigroup (Semigroup ((<>)))
import Data.Monoid (Monoid (mempty))

import Data.Bool (Bool (False, True))
import Data.Ord  (Ordering (LT, EQ, GT))
import Data.Function (flip)


class Choice or => Match or where
   match :: (a -> c) -> (b -> c) -> a `or` b -> c


----- Defaults for Classes -----

--- Eq2:
liftEq2Default :: (Match or) => (a -> b -> Bool) -> (c -> d -> Bool) -> a `or` c -> b `or` d -> Bool
liftEq2Default p q = match
   (\ x -> match (p x) (pure False))
   (match (pure False) . q)

--- Ord2:
liftCompare2Default :: (Match or) => (a -> b -> Ordering) -> (c -> d -> Ordering) -> a `or` c -> b `or` d -> Ordering
-- The first type parameter is always 'less than' the second.
liftCompare2Default f g = match
   (\ x -> match (f x) (pure LT))
   (match (pure GT) . g)

--- Monad:
bindDefault :: (Match p, Choice q) => (a -> q c b) -> p c a -> q c b
bindDefault = match makeL

--- Applicative:
apDefault :: (Match p, Match q) => q c (a -> b) -> p c a -> q c b
apDefault mf mx = bindDefault (`fmapDefault` mx) mf

--- (Bi)Functor:
fmapDefault :: (Match p, Choice q) => (a -> b) -> p c a -> q c b
fmapDefault f = bindDefault (makeR . f)

firstDefault :: (Match p, Choice q) => (a -> b) -> p a c -> q b c
firstDefault f = match (makeL . f) makeR

bimapDefault :: (Match p, Choice q) => (a -> b) -> (c -> d) -> p a c -> q b d
bimapDefault f g = match (makeL . f) (makeR . g)

--- Semigroup:
semigroupDefault ::
   (Match or, Semigroup a, Semigroup b) =>
   a `or` b -> a `or` b -> a `or` b
-- Prefer b.
semigroupDefault = match
   (firstDefault . (<>))
   (\ x -> makeR . foldlDefault (<>) x)

--- Monoid:
memptyDefault :: (Choice or, Monoid a) => a `or` b
memptyDefault = makeL mempty

--- Foldable:
foldMapDefault :: (Match p, Monoid b) => (a -> b) -> p c a -> b
foldMapDefault = match mempty
-- foldMapDefault = match (pure mempty) -- pure mempty = mempty

foldlDefault :: (Match p) => (b -> a -> b) -> b -> p c a -> b
foldlDefault = bifoldlDefault pure

foldrDefault :: (Match p) => (a -> b -> b) -> b -> p c a -> b
foldrDefault f = foldlDefault (flip f)

--- Bifoldable:
-- bifoldMap = match

bifoldlDefault :: (Match or) => (c -> a -> c) -> (c -> b -> c) -> c -> a `or` b -> c
bifoldlDefault f g z = match (f z) (g z)

bifoldrDefault :: (Match or) => (a -> c -> c) -> (b -> c -> c) -> c -> a `or` b -> c
bifoldrDefault f g = bifoldlDefault (flip f) (flip g)


--- Traversable:
traverseDefault ::
   (Match p, Choice q, Applicative m) =>
   (a -> m b) -> p c a -> m (q c b)
traverseDefault f = match (pure . makeL) (fmap makeR . f)

--- Bitraversable:
bitraverseDefault ::
   (Match p, Choice q, Applicative m) =>
   (a -> m b) -> (c -> m d) -> p a c -> m (q b d)
bitraverseDefault f g = match (fmap makeL . f) (fmap makeR . g)
