## Context

Library of types that 'enrich' generic types with a 'context' specific to a given use-case. Most types are based on common monadic types ported from Haskell (Either, Maybe, State, etc).

The library is written in F# and primarily intended for F# use, but supports interoperability with other .NET languages.

Each file includes the data definition, common primitive functions for the type, as well as some typeclass 'instance' implementations (Functor, Monad, Foldable, etc). Types that implement 'Monad' include an F# 'computation expression' builder.

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
- Category    -- removed for now
- Arrow       -- removed for now
- ArrowChoice -- removed for now

*Notes on typeclasses:*
- 'Show' is implemented by default via .ToString() method.
- 'Eq' and 'Ord' (i.e. Equality and Comparison) are implemented (for most types) by default **when called from F# code**.
- 'Traversable' is not implemented, but a sequence-specific pair of 'sequence' and 'traverse' are provided for each Applicative type.
- 'MonadPlus' uses the 'Alternative' functions 'empty' and 'orElse'. These may obey either the LeftCatch law and/or the LeftDistribution laws. This may be unified at some future point.
- Types that can support **some** useful functions of a typeclass but not all are given implementations for what they can support (e.g. Either supports the Alternative 'orElse' function but not 'empty').

*Notes on interop with .NET languages:*
F# supports static type constraints, currying, custom operators, and other features that other .NET languages do not. Exceptions and/or compiler warnings may be added to these in the future.
- Instances of types from this library used outside of F# may have auto-generated properties (Tag, Item, right, left, etc.). These may or may not be safe depending on the type, so their use is not recommended. For example, calling '.Item' on a Maybe-value which has no value will throw an exception.
- Functions and methods that utilize static type constraints in F# will compile in and can sometimes be used by other .NET languages, but their runtime-safety is not guaranteed. Testing is recommended.
- Curried functions are converted to normal function calls. This works well but makes partial application more difficult.
- Custom operators can be used in F# as designed, but do not show up in other languages. The 'CompiledName' attribute has been used to change the names, as seen by other languages, so as to be callable as normal methods.


## Context.Linq

Each type, as applicable, supports various Linq-style 'query' methods (Select, SelectMany, Where, Zip, etc.) for interop with other .NET languages. Query-style syntax approximates F#'s computation expression syntax.

*Note: some methods, such as 'Join', don't necessarily act like their standard Ling/SQL counterparts.*
*For example, several types have valid instances of the 'Applicative' typeclass and thus a 'map2' function. This can be implemented as both the 'Zip' and/or 'Join' methods. In query expressions this is typically more performant **when it can be used without invoking the 'SelectMany' method**. When used in a 'query' expression, it should be used as follows:*
```
from x in xs
join y in ys on 1 equals 1
join z in zs on 1 equals 1
select x + y + z
```