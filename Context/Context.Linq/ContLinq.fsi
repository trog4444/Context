namespace Rogz.Context.Linq

open Rogz.Context.Base


[<AbstractClass; Sealed; System.Runtime.CompilerServices.Extension>]
type LinqCont =

// Functor

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Lift a function onto a context.</summary>
    static member Select: source: Cont<'R,'T> * mapping: System.Func<'T, 'U> -> Cont<'R,'U>


// Applicative

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    static member Apply: func: Cont<'R,System.Func<'T, 'U>> * value: Cont<'R,'T> -> Cont<'R,'U>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Merge the values of two contexts into a single pair.</summary>
    static member Zip: first: Cont<'R,'T> * second: Cont<'R,'U> -> Cont<'R, struct ('T * 'U)>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Lift a binary function onto contexts.</summary>
    static member Zip: first: Cont<'R,'T> * second: Cont<'R,'U> * mapping: System.Func<'T, 'U, 'V> -> Cont<'R,'V>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Lift a binary function onto contexts.</summary>
    static member Join: first: Cont<'R,'T> * second: Cont<'R,'U> * key1: System.Func<'T, 'K> * key2: System.Func<'U, 'K> * mapping: System.Func<'T, 'U, 'V> -> Cont<'R,'V> when 'K : equality


// Monad

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    static member SelectMany: source: Cont<'R,'T> * projection: System.Func<'T, Cont<'R,'U>> -> Cont<'R,'U>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    static member SelectMany: source: Cont<'R,'T> * projection: System.Func<'T, Cont<'R,'U>> * resultSelector: System.Func<'T, 'U, 'V> -> Cont<'R,'V>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    static member Flatten: source: Cont<'R,Cont<'R,'T>> -> Cont<'R,'T>