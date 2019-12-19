## Context

F# library of types that 'enhance' generic types with context specific to the given type. Most types are based on common types ported from the Haskell prelude (Either, Maybe, etc).

Each file includes the data definition as well as common primitives for the type, as well as some typeclass 'instance' implementations (Functor, Monad, Foldable, etc). Types that implement 'Monad' include an F# 'computation expression' builder.

Each type, as applicable, supports various LINQ 'query' methods (Select, SelectMany, Where, etc) for interop with other .NET languages.

*Note: the 'Join' method does necessarily act like an SQL Join-query. Typically it should be treated like the Applicative 'map2' function. This is typically more performant **when it can be used without invoking the 'SelectMany' method**. When used in a 'query' expression should be used as follows:*
```
from x in xs
join y in ys on 1 equals 1
join z in zs on 1 equals 1
select x + y + z
```

Typeclasses used:

- Functor
- Bifunctor
- Profunctor
- Applicative
- Alternative
- Biapplicative
- Monad
- MonadPlus
- Semigroup
- Monoid
- Foldable
- Bifoldable
- Category
- Arrow
- ArrowChoice

*Notes on typeclasses:*
- Show is implemented by default via .ToString() method.
- Eq and Ord (i.e. Equality and Comparison) are implemented (for most types) by default **when called from F# code**.
- Traversable is not implemented, but a sequence-specific pair of 'sequence' and 'traverse' are provided for each Applicative type.
- MonadPlus uses the Alternative functions (nil (instead of the Haskell 'empty') and 'orElse'. These may obey either the LeftCatch law and/or the LeftDistribution laws. This may be unified at some future point.
- Types that can support **some** useful functions of a typeclass but not all are given implementations for what they can support (e.g. Either supports the Alternative 'orElse' function but not nil/empty).

