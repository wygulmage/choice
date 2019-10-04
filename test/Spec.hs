{-# LANGUAGE
    UnicodeSyntax
  , NoImplicitPrelude
  , FlexibleInstances
  , TypeOperators
  , TypeSynonymInstances
  #-}

import Test.QuickCheck
-- import Bichoice.Internal.Class.Choice
-- import Bichoice.Internal.Class.Match
import Data.Bifunctor.Choice

import Prelude (Eq, Bool, Int, IO, (==), fmap, pure)
import Control.Category ((.), id)


instance (Arbitrary a, Arbitrary b) => Arbitrary (a || b) where
  arbitrary = oneof (fmap makeL arbitrary : fmap makeR arbitrary : [])

main :: IO ()
main = do
   -- quickCheck prop_fmapDefault_id_lazy
   pure ()

-- prop_fmapDefault_id :: (Choice or, Eq (a `or` b)) => a `or` b -> Bool
-- prop_fmapDefault_id xs = fmapDefault id xs == xs

-- prop_fmapDefault_id_lazy :: Int || Int -> Bool
-- prop_fmapDefault_id_lazy = prop_fmapDefault_id
