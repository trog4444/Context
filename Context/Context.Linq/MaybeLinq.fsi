namespace Rogz.Context.Linq

open Rogz.Context.Base


[<AbstractClass; Sealed; System.Runtime.CompilerServices.Extension>]
type LinqMaybe =

// Functor

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Lift a function onto a context.</summary>
    static member Select: source: Maybe<'T> * mapping: System.Func<'T, 'U> -> Maybe<'U>


// Applicative

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    static member Apply: func: Maybe<System.Func<'T, 'U>> * value: Maybe<'T> -> Maybe<'U>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Merge the values of two contexts into a single pair.</summary>
    static member Zip: first: Maybe<'T> * second: Maybe<'U> -> Maybe<struct ('T * 'U)>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Lift a binary function onto contexts.</summary>
    static member Zip: first: Maybe<'T> * second: Maybe<'U> * mapping: System.Func<'T, 'U, 'V> -> Maybe<'V>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Acts similar to a SQL 'inner join', combining elements of each given monad when the elements satisfy a predicate.</summary>
    static member Join: first: Maybe<'T> * second: Maybe<'U> * key1: System.Func<'T, 'K> * key2: System.Func<'U, 'K> * mapping: System.Func<'T, 'U, 'V> -> Maybe<'V> when 'K : equality


// Alternative

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>An associative operation representing a decision between two structures.</summary>
    static member OrElse: first: Maybe<'T> * second: Maybe<'T> -> Maybe<'T>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>An associative operation representing a decision between two structures.</summary>
    static member OrElse: first: Maybe<'T> * second: System.Func<Maybe<'T>> -> Maybe<'T>


// Monad

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    static member SelectMany: source: Maybe<'T> * projection: System.Func<'T, Maybe<'U>> -> Maybe<'U>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    static member SelectMany: source: Maybe<'T> * projection: System.Func<'T, Maybe<'U>> * resultSelector: System.Func<'T, 'U, 'V> -> Maybe<'V>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    static member Flatten: source: Maybe<Maybe<'T>> -> Maybe<'T>


// MonadPlus

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Generalizes the sequence-based 'filter' function.</summary>
    static member Where: source: Maybe<'T> * predicate: System.Func<'T, bool> -> Maybe<'T>

    [<System.Runtime.CompilerServices.Extension>]
    // No apparent use in QUERIES, but still useful for method syntax.
    /// <summary>Acts similar to a SQL 'inner join', combining elements of each given monad when the elements satisfy a predicate.</summary>
    static member GroupJoin: first: Maybe<'T> * second: Maybe<'U> * predicate: System.Func<'T, 'U, bool> * mapping: System.Func<'T, 'U, 'V> -> Maybe<'V> when 'K: equality

    [<System.Runtime.CompilerServices.Extension>]
    // Same as Join but allows 'into' syntax in queries.
    /// <summary>Correlates the element(s) of two sources based on equality of keys and selects the results.</summary>
    static member GroupJoin: outer: Maybe<'T> * inner: Maybe<'U> * outerKeySelector: System.Func<'T, 'K> * innerKeySelector: System.Func<'U, 'K> * resultSelector: System.Func<'T, Maybe<'U>, 'V> -> Maybe<'V> when 'K: equality


// Foldable

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    static member Aggregate: source: Maybe<'T> * seed: 'State * func: System.Func<'State,'T,'State> -> 'State

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    static member Aggregate: source: Maybe<'T> * seed: 'State * func: System.Func<'State,'T,'State> * resultSelector: System.Func<'State,'Result> -> 'Result

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
    static member Aggregate: source: Maybe<'T> * seed: 'State * func: System.Func<'State, 'T, struct (Maybe<'U> * 'State)> -> struct (Maybe<'U> * 'State)