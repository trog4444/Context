namespace Rogz.Context.Linq

open Rogz.Context.Base


[<AbstractClass; Sealed; System.Runtime.CompilerServices.Extension>]
type LinqEither =

// Functor

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Lift a function onto a context.</summary>
    static member Select: source: Either<'E, 'T> * mapping: System.Func<'T, 'U> -> Either<'E, 'U>


// Bifunctor

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Map over both arguments covariantly.</summary>
    static member Select: source: Either<'E, 'T> * mapLeft: System.Func<'E, 'F> * mapRight: System.Func<'T, 'U> -> Either<'F, 'U>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Map over the first value, leaving the second value as-is.</summary>
    static member Select: source: Either<'E, 'T> * mapLeft: System.Func<'E, 'F> -> Either<'F, 'T>


// Applicative

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    static member Apply: func: Either<'E, System.Func<'T, 'U>> * value: Either<'E, 'T> -> Either<'E, 'U>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Merge the values of two contexts into a single pair.</summary>
    static member Zip: first: Either<'E, 'T> * second: Either<'E, 'U> -> Either<'E, struct ('T * 'U)>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Lift a binary function onto contexts.</summary>
    static member Zip: first: Either<'E, 'T> * second: Either<'E, 'U> * mapping: System.Func<'T, 'U, 'V> -> Either<'E, 'V>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Lift a binary function onto contexts.</summary>
    static member Join: first: Either<'E, 'T> * second: Either<'E, 'U> * key1: System.Func<'T, 'K> * key2: System.Func<'U, 'K> * mapping: System.Func<'T, 'U, 'V> -> Either<'E, 'V> when 'K : equality


// Alternative

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>An associative operation representing a decision between two structures.</summary>
    static member OrElse: first: Either<'E, 'T> * second: Either<'E, 'T> -> Either<'E, 'T>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>An associative operation representing a decision between two structures.</summary>
    static member OrElse: first: Either<'E, 'T> * second: System.Func<Either<'E, 'T>> -> Either<'E, 'T>


// Monad

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    static member SelectMany: source: Either<'E, 'T> * projection: System.Func<'T, Either<'E, 'U>> -> Either<'E, 'U>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    static member SelectMany: source: Either<'E, 'T> * projection: System.Func<'T, Either<'E, 'U>> * resultSelector: System.Func<'T, 'U, 'V> -> Either<'E, 'V>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    static member Flatten: source: Either<'E, Either<'E, 'T>> -> Either<'E, 'T>


// Foldable

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    static member Aggregate: source: Either<'E, 'T> * seed: 'State * func: System.Func<'State,'T,'State> -> 'State

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    static member Aggregate: source: Either<'E, 'T> * seed: 'State * func: System.Func<'State,'T,'State> * resultSelector: System.Func<'State,'Result> -> 'Result

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
    static member Aggregate: source: Either<'E, 'T> * seed: 'State * func: System.Func<'State, 'T, struct (Either<'E, 'U> * 'State)> -> struct (Either<'E, 'U> * 'State)


// Bifoldablde

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s).</summary>
    static member Aggregate: source: Either<'A, 'B> * seed: 'State * func1: System.Func<'State,'A,'State> * func2: System.Func<'State,'B,'State> -> 'State

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s).</summary>
    static member Aggregate: source: Either<'A, 'B> * seed: 'State * func1: System.Func<'State,'A,'State> * func2: System.Func<'State,'B,'State> * resultSelector: System.Func<'State,'Result> -> 'Result

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
    static member Aggregate: source: Either<'A, 'B> * seed: 'State * func1: System.Func<'State, 'A, struct (Either<'T, 'U> * 'State)> * func2: System.Func<'State, 'B, struct (Either<'T, 'U> * 'State)> -> struct (Either<'T, 'U> * 'State)