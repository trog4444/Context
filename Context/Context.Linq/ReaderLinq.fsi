namespace Rogz.Context.Linq

open Rogz.Context.Base


[<AbstractClass; Sealed; System.Runtime.CompilerServices.Extension>]
type LinqReader =

// Functor

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Lift a function onto a context.</summary>
    static member Select: source: Reader<'E,'T> * mapping: System.Func<'T, 'U> -> Reader<'E,'U>


// Profunctor

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Map over both arguments at the same time, the first (i.e. 'left') contravariantly and the second (i.e. 'right') covariantly.</summary>
    static member Select: source: Reader<'E,'T> * pre: System.Func<'E0, 'E> * post: System.Func<'T, 'U> -> Reader<'E0, 'U>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Map the first (i.e. 'left') argument contravariantly.</summary>
    static member Select: source: Reader<'E,'T> * pre: System.Func<'E0, 'E> -> Reader<'E0,'T>


// Applicative

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    static member Apply: func: Reader<'E,System.Func<'T, 'U>> * value: Reader<'E,'T> -> Reader<'E,'U>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Merge the values of two contexts into a single pair.</summary>
    static member Zip: first: Reader<'E,'T> * second: Reader<'E,'U> -> Reader<'E, struct ('T * 'U)>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Lift a binary function onto contexts.</summary>
    static member Zip: first: Reader<'E,'T> * second: Reader<'E,'U> * mapping: System.Func<'T, 'U, 'V> -> Reader<'E,'V>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Lift a binary function onto contexts.</summary>
    static member Join: first: Reader<'E,'T> * second: Reader<'E,'U> * key1: System.Func<'T, 'K> * key2: System.Func<'U, 'K> * mapping: System.Func<'T, 'U, 'V> -> Reader<'E,'V> when 'K : equality


// Monad

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    static member SelectMany: source: Reader<'E,'T> * projection: System.Func<'T, Reader<'E,'U>> -> Reader<'E,'U>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    static member SelectMany: source: Reader<'E,'T> * projection: System.Func<'T, Reader<'E,'U>> * resultSelector: System.Func<'T, 'U, 'V> -> Reader<'E,'V>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    static member Flatten: source: Reader<'E,Reader<'E,'T>> -> Reader<'E,'T>