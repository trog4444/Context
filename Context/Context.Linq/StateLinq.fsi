namespace Rogz.Context.Linq

open Rogz.Context.Base


[<AbstractClass; Sealed; System.Runtime.CompilerServices.Extension>]
type LinqState =

// Functor

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Lift a function onto a context.</summary>
    static member Select: source: State<'S,'T> * mapping: System.Func<'T, 'U> -> State<'S,'U>


// Applicative

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    static member Apply: func: State<'S,System.Func<'T, 'U>> * value: State<'S,'T> -> State<'S,'U>

    [<System.Runtime.CompilerServices.Extension>]
    static member Zip: first: State<'S,'T> * second: State<'S,'U> -> State<'S, struct ('T * 'U)>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Lift a binary function onto contexts.</summary>
    static member Zip: first: State<'S,'T> * second: State<'S,'U> * mapping: System.Func<'T, 'U, 'V> -> State<'S,'V>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Lift a binary function onto contexts.</summary>
    static member Join: first: State<'S,'T> * second: State<'S,'U> * key1: System.Func<'T, 'K> * key2: System.Func<'U, 'K> * mapping: System.Func<'T, 'U, 'V> -> State<'S,'V> when 'K : equality


// Monad

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    static member SelectMany: source: State<'S,'T> * projection: System.Func<'T, State<'S,'U>> -> State<'S,'U>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    static member SelectMany: source: State<'S,'T> * projection: System.Func<'T, State<'S,'U>> * resultSelector: System.Func<'T, 'U, 'V> -> State<'S,'V>

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    static member Flatten: source: State<'S,State<'S,'T>> -> State<'S,'T>