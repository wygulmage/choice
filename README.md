Bichoice
--------

Why does this package exist?

1. I want to write `a || b || c` instead of `Either a (Either b c)`, and I want to write `a !! b` for a strict binary sum type.

2. I want `(<>) :: (Semigroup a, Semigroup b) => a || b -> a || b -> a || b` that prefers `b`s. (To get the current `Either` behavior you'd use `First a || First b`)

3. I want `mempty :: (Monoid a, Semigroup b) => a || b`.

4. Once I have 1–3, I want to write `() || a` instead of `Maybe a`.


This package is an attempt to satisfy all of those selfish desires at once.


Use `Data.Bifunctor.Choice.Lazy` for `||` and `Data.Bifunctor.Choice.Strict` for `!!`.


Strategy
--------

Make a class that has the desired properties:
```Haskell
class Choice or where
   match :: (a → c) → (b → c) → a `or` b → c
   makeL :: a → a `or` c
   makeR :: a → c `or` a
```

Use that class to make default methods for existing classes:
```Haskell
bimapDefault :: Choice or => (a → b) → c `or` a → c `or` b
bimapDefault f g = match (makeL . f) (makeR . g)

-- &c.
```

Create instances that have the desired strictness properties:
```Haskell
newtype Lazy a b = Lazy{ getLazy :: Either a b }
newtype Strict a b = Strict{ getStrict :: Either a b }

instance Choice Lazy where
   match f g = either f g . getLazy
   makeL = Lazy . Left
   makeR = Lazy . Right

instance Choice Strict where
   match f g = either f g . getStrict
   makeL x = x `seq` Strict (Left x)
   makeR x = x `seq` Strict (Right x)
```

Create a wrapper type to derive instances from instances of Choice:
```Haskell
newtype ChoiceT or a b = ChoiceT{ getChoiceT :: a `or` b }

instance Choice or => Choice (ChoiceT or) where
   match f g = match f g . getChoiceT
   makeL = ChoiceT . makeL
   makeR = ChoiceT . makeR

instance Choice or => Bifunctor (ChoiceT or) where
  bimap = bimapDefault
```

Create usable types for specific applications of the wrapper:
```Haskell
type (||) = ChoiceT Lazy
type (!!) = ChoiceT Strict
```

Finally, make sure that you aren't exporting any constructors or destructors except those provided by the class (to avoid violation of strictness properties. Yes, you can still use `coerce` to do that.`coerce` is not safe).
