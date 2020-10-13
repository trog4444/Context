namespace Rogz.Context.Linq

open Rogz.Context.Base


[<AbstractClass; Sealed; System.Runtime.CompilerServices.Extension>]
type LinqState =

// Functor

    [<System.Runtime.CompilerServices.Extension>]
    static member Select((State st), mapping: System.Func<'T, 'U>) =
        State (fun (s: 'S) -> let struct(s,v) = st s in struct(s, mapping.Invoke(v)))


// Applicative

    [<System.Runtime.CompilerServices.Extension>]
    static member Apply((State (sf: 'S -> struct('S * System.Func<'T, 'U>))), (State sv)) =
        State (fun s ->
            let struct(s,a) = sf s
            let struct(s,b) = sv s
            (s, a.Invoke(b)))

    [<System.Runtime.CompilerServices.Extension>]
    static member Zip((State first), (State second)) : State<'S, struct ('T * 'U)> =
        State(fun (s: 'S) ->
            let struct(s,a) = first s
            let struct(s,b) = second s
            (s, struct (a, b)))

    [<System.Runtime.CompilerServices.Extension>]
    static member Zip((State first), (State second), mapping: System.Func<'T, 'U, 'V>) =
        State(fun (s: 'S) ->
            let struct(s,a) = first s
            let struct(s,b) = second s
            (s, mapping.Invoke(a, b)))

    [<System.Runtime.CompilerServices.Extension>]
    static member Join((State first), (State second), key1: System.Func<'T, 'K>, key2: System.Func<'U, 'K>, mapping: System.Func<'T, 'U, 'V>) : State<'S, 'V> when 'K : equality =
        State(fun (s: 'S) ->
            let struct(s,a) = first s
            let struct(s,b) = second s
            (s, mapping.Invoke(a, b)))


// Monad

    [<System.Runtime.CompilerServices.Extension>]
    static member SelectMany((State source), projection: System.Func<'T, State<'S, 'U>>) =
        State(fun s ->
            let struct(s,a) = source s
            let (State f) = projection.Invoke(a)
            f s)

    [<System.Runtime.CompilerServices.Extension>]
    static member SelectMany((State source), projection: System.Func<'T, State<'S, 'U>>, resultSelector: System.Func<'T, 'U, 'V>) =
        State(fun s ->
            let struct(s,a) = source s
            let (State f) = projection.Invoke(a)
            let struct(s,b) = f s
            (s, resultSelector.Invoke(a, b)))

    [<System.Runtime.CompilerServices.Extension>]
    static member Flatten((State source)) : State<'S, 'T> =
        State(fun s ->
            let struct(s,(State f)) = source s
            f s)