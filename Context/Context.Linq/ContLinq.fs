namespace Rogz.Context.Linq

open Rogz.Context.Base


[<AbstractClass; Sealed; System.Runtime.CompilerServices.Extension>]
type LinqCont =

// Funtor

    [<System.Runtime.CompilerServices.Extension>]
    static member Select((Cont cx), mapping: System.Func<'T, 'U>) : Cont<'R, 'U> =
        Cont (fun k -> cx (fun a -> k (mapping.Invoke(a))))


// Applicative

    [<System.Runtime.CompilerServices.Extension>]
    static member Apply((Cont (f: (System.Func<'T, 'U> -> 'R) -> 'R)), (Cont (v: ('T -> 'R) -> 'R))) : Cont<'R, 'U> =
        Cont(fun k -> f (fun f' -> v (fun v' -> k (f'.Invoke v'))))

    [<System.Runtime.CompilerServices.Extension>]
    static member Zip((Cont a), (Cont b)) : Cont<'R, struct ('T * 'U)> =
        Cont(fun k -> a (fun a -> b (fun b -> k (a, b))))

    [<System.Runtime.CompilerServices.Extension>]
    static member Zip((Cont a), (Cont b), mapping: System.Func<'T, 'U, 'V>) : Cont<'R, 'V> =
        Cont(fun k -> a (fun a -> b (fun b -> k (mapping.Invoke(a, b)))))

    [<System.Runtime.CompilerServices.Extension>]
    static member Join((Cont a), (Cont b), key1: System.Func<'T, 'K>, key2: System.Func<'U, 'K>, mapping: System.Func<'T, 'U, 'V>) : Cont<'R, 'V> =
        Cont(fun k -> a (fun a -> b (fun b -> k (mapping.Invoke(a, b)))))


// Monad

    [<System.Runtime.CompilerServices.Extension>]
    static member SelectMany((Cont c), projection: System.Func<'T, Cont<'R, 'U>>) =
        Cont(fun k -> c (fun a -> let (Cont cc) = projection.Invoke(a) in cc k))

    [<System.Runtime.CompilerServices.Extension>]
    static member SelectMany((Cont c), projection: System.Func<'T, Cont<'R, 'U>>, resultSelector: System.Func<'T, 'U, 'V>) =
        Cont(fun k ->
            c (fun a ->
                let (Cont cc) =projection.Invoke(a)
                cc (fun b -> k (resultSelector.Invoke(a, b)))))

    [<System.Runtime.CompilerServices.Extension>]
    static member Flatten(Cont c) : Cont<'R, 'T> =
        Cont(fun k -> c (fun (Cont cc) -> cc k))