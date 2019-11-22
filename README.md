## Context

F# library of types that 'enhance' generic types with context specific to the given type. Most types are based on common types ported from the Haskell prelude (Either, Maybe, etc).

Each file includes the data definition as well as common primitives for the type, as well as some typeclass 'instance' implementations (Functor, Monad, Foldable, etc). Types that implement 'Monad' include an F# 'computation expression' builder.

Each type, as applicable, supports various LINQ 'query' methods (Select, SelectMany, Where, etc) for interop with other .NET languages.
Note: the 'Join' method does necessarily act like an SQL Join-query. Typically it should be treated like the Applicative 'map2' function, and when used in a 'query' expression should be used as follows:
	from x in xs
	join y in ys on 1 equals 1
	join z in zs on 1 equals 1
	select x + y + z
This is typically more performant when it can be used without invoking the 'SelectMany' method.