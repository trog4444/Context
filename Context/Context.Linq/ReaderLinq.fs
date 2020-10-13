namespace Rogz.Context.Linq

open Rogz.Context.Base


[<AbstractClass; Sealed; System.Runtime.CompilerServices.Extension>]
type LinqReader =

// Functor

    [<System.Runtime.CompilerServices.Extension>]
    static member Select((Reader r), mapping: System.Func<'T, 'U>) =
        Reader (fun e -> mapping.Invoke(r e))


// Profunctor

    [<System.Runtime.CompilerServices.Extension>]
    static member Select((Reader r), pre: System.Func<'E0, 'E>, post: System.Func<'T, 'U>) =
        Reader (fun d -> post.Invoke(r (pre.Invoke d)))

    [<System.Runtime.CompilerServices.Extension>]
    static member Select((Reader r), pre: System.Func<'E0, 'E>) =
        Reader (fun d -> r (pre.Invoke d))

        
// Applicative

    [<System.Runtime.CompilerServices.Extension>]
    static member Apply((Reader (rf: 'e -> System.Func<'T, 'U>)), (Reader rv)) =
        Reader (fun e -> (rf e).Invoke(rv e))

    [<System.Runtime.CompilerServices.Extension>]
    static member Zip((Reader first), (Reader second)) : Reader<'E, struct ('T * 'U)> =
        Reader(fun (e: 'E) -> struct (first e, second e))

    [<System.Runtime.CompilerServices.Extension>]
    static member Zip((Reader first), (Reader second), mapping: System.Func<'T, 'U, 'V>) =
        Reader(fun (e: 'E) -> mapping.Invoke(first e, second e))

    [<System.Runtime.CompilerServices.Extension>]
    static member Join((Reader first), (Reader second), key1: System.Func<'T, 'K>, key2: System.Func<'U, 'K>, mapping: System.Func<'T, 'U, 'V>) : Reader<'E, 'V> when 'K : equality =
        Reader(fun (e: 'E) -> mapping.Invoke(first e, second e))


// Monad

    [<System.Runtime.CompilerServices.Extension>]
    static member SelectMany((Reader source), projection: System.Func<'T, Reader<'E, 'U>>) =
        Reader(fun e ->
            let (Reader f) = projection.Invoke(source e)
            in f e)

    [<System.Runtime.CompilerServices.Extension>]
    static member SelectMany((Reader source), projection: System.Func<'T, Reader<'E, 'U>>, resultSelector: System.Func<'T, 'U, 'V>) =
        Reader(fun e ->
            let a = source e
            let (Reader f) = projection.Invoke(a)
            resultSelector.Invoke(a, f e))

    [<System.Runtime.CompilerServices.Extension>]
    static member Flatten((Reader source)) : Reader<'E, 'T> =
        Reader(fun e -> let (Reader r) = source e in r e)