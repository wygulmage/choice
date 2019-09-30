{-# LANGUAGE
    NoImplicitPrelude
  , QuantifiedConstraints
  , TypeOperators
  #-}

module Bichoice.Internal.Class.Match where
--- co-Cartesian products ---

import Bichoice.Internal.Class.Choice
import Prelude (fst, snd)
import Control.Applicative (Applicative (pure))
import Control.Category (Category ((.), id))
import Data.Functor (Functor (fmap))
import Data.Semigroup (Semigroup ((<>)))
import Data.Monoid (Monoid (mempty))

import Data.Bool (Bool (False, True))
import Data.Ord  (Ordering (LT, EQ, GT))
import Data.Function (flip)


class Choice or => Match or where
   match :: (a -> c) -> (b -> c) -> a `or` b -> c

infixr 2 `match`

zipWith :: (Match p, Choice q, Semigroup d) => (a -> b -> c) -> p d a -> p d b -> q d c
-- Use Semigroup to zip left values.
zipWith f = match (makeL .: bifoldlDefault (<>) pure) (bindDefault . (makeR .: f))


----- Defaults for Classes -----

--- Eq2:
liftEq2Default :: (Match or) => (a -> b -> Bool) -> (c -> d -> Bool) -> a `or` c -> b `or` d -> Bool
liftEq2Default p q =
   match
      (\ x -> p x `match` pure False)
      (match (pure False) . q)

--- Ord2:
liftCompare2Default :: (Match or) => (a -> b -> Ordering) -> (c -> d -> Ordering) -> a `or` c -> b `or` d -> Ordering
-- The first type parameter is always 'less than' the second.
liftCompare2Default f g =
   match
      (\ x -> f x `match` pure LT)
      (match (pure GT) . g)

--- Monad:
bindDefault :: (Match p, Choice q) => (a -> q c b) -> p c a -> q c b
bindDefault = match makeL

--- Applicative:
apDefault :: (Match p, Match q) => q c (a -> b) -> p c a -> q c b
-- apDefault mf mx = bindDefault (`fmapDefault` mx) mf -- per Monad class laws
apDefault = match (pure . makeL) fmapDefault

liftA2Default :: (Match p, Choice q) => (a -> b -> c) -> p d a -> p d b -> q d c
liftA2Default f = match (pure . makeL) (fmapDefault . f)


--- (Bi)Functor:
fmapDefault :: (Match p, Choice q) => (a -> b) -> p c a -> q c b
fmapDefault f = bindDefault (makeR . f)

firstDefault :: (Match p, Choice q) => (a -> b) -> p a c -> q b c
firstDefault f = makeL . f `match` makeR

bimapDefault :: (Match p, Choice q) => (a -> b) -> (c -> d) -> p a c -> q b d
bimapDefault f g = makeL . f `match` makeR . g

--- Semigroup:
semigroupDefault ::
   (Match or, Semigroup a, Semigroup b) =>
   a `or` b -> a `or` b -> a `or` b
-- Prefer b.
semigroupDefault = firstDefault . (<>) `match` makeR .: foldlDefault (<>)

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
bifoldlDefault f g z = f z `match` g z

bifoldrDefault :: (Match or) => (a -> c -> c) -> (b -> c -> c) -> c -> a `or` b -> c
bifoldrDefault f g = flip f `bifoldlDefault ` flip g

--- Traversable:
traverseDefault ::
   (Match p, Choice q, Applicative m) =>
   (a -> m b) -> p c a -> m (q c b)
traverseDefault f = pure . makeL `match` fmap makeR . f
-- traverseDefault f = bitraverseDefault pure

--- Bitraversable:
bitraverseDefault ::
   (Match p, Choice q, Applicative m) =>
   (a -> m b) -> (c -> m d) -> p a c -> m (q b d)
bitraverseDefault f g = fmap makeL . f `match` fmap makeR . g


----- Utility Functions -----
(.:) :: (Category f) => c `f` d -> (a -> b `f` c) -> a -> b `f` d
(.:) = (.) . (.)


----- Rules -----

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- f . g = \ x -> f (g x)
-- {-# INLINE [0] (.) #-}

-- id :: a -> a
-- id x = x
-- {-# INLINE [0] id #-}

-- match_RULE :: (Match or) => (a -> c) -> (b -> c) -> a `or` b -> c
-- match_RULE = match
-- {-# INLINE [0] match_RULE #-}

-- {-# RULES
--   "Don't inline match early." [~0]
--   match = match_RULE
--   ;
--   "match inL"
--   forall f g x. match_RULE f g (makeL_RULE x) = f x
--   ;
--   "match inR"
--   forall f g x. match_RULE f g (makeR_RULE x) = g x
--   ;
--   "match after match"
--   forall f g h i x. match_RULE f g (match_RULE (makeL_RULE . h) (makeR_RULE . i) x) = match_RULE (f . h) (g . i) x
--   #-}
