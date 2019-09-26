# choice

Why does this package exist?

1. I want to write `a || b || c` instead of `Either a (Either b c)`, and I want to write `a !! b` for a strict binary sum type.

2. I want `(<>) :: (Semigroup a, Semigroup b) => a || b -> a || b -> a || b` that prefers `b`s. (To get the current `Either` behavior you'd use `First a || First b`)

3. I want `mempty :: (Monoid a, Semigroup b) => a || b`.

4. Once I have 1–3, I want to write `() || a` instead of `Maybe a`.


This package is an attempt to satisfy all of those selfish desires at once.


Use `Data.Bifunctor.Choice.Lazy` for `||` and `Data.Bifunctor.Choice.Strict` for `!!`.
