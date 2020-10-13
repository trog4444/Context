namespace Rogz.Context.Linq

open Rogz.Context.Base


[<AbstractClass; Sealed; System.Runtime.CompilerServices.Extension>]
type LinqEither =

// Functor

    [<System.Runtime.CompilerServices.Extension>]
    static member Select(source, mapping: System.Func<'T, 'U>) =
        match source with
        | Left e  -> Left e
        | Right a -> Right (mapping.Invoke a)


// Bifunctor

    [<System.Runtime.CompilerServices.Extension>]
    static member Select(source: Either<'E, 'T>, mapLeft: System.Func<'E, 'F>, mapRight: System.Func<'T, 'U>) =
        match source with
        | Left a  -> Left  (mapLeft.Invoke(a))
        | Right b -> Right (mapRight.Invoke(b))

    [<System.Runtime.CompilerServices.Extension>]
    static member Select(source: Either<'E, 'T>, mapLeft: System.Func<'E, 'F>) =
        match source with
        | Left a  -> Left  (mapLeft.Invoke(a))
        | Right b -> Right b


// Applicative

    [<System.Runtime.CompilerServices.Extension>]
    static member Apply(func: Either<'E, System.Func<'T, 'U>>, value: Either<'E, 'T>) : Either<'E, 'U> =
        match func, value with
        | Left e, _        -> Left e
        | _, Left e        -> Left e
        | Right f, Right v -> Right (f.Invoke(v))

    [<System.Runtime.CompilerServices.Extension>]
    static member Zip(first: Either<'E, 'T>, second: Either<'E, 'U>) =
        match first, second with
        | Left e, _        -> Left e
        | _, Left e        -> Left e
        | Right a, Right b -> Right (struct (a, b))

    [<System.Runtime.CompilerServices.Extension>]
    static member Zip(first, second, mapping: System.Func<'T, 'U, 'V>) =
        match first, second with
        | Left e, _        -> Left e
        | _, Left e        -> Left e
        | Right a, Right b -> Right (mapping.Invoke(a, b))

    [<System.Runtime.CompilerServices.Extension>]
    static member Join(first: Either<'E, 'T>, second: Either<'E, 'U>, key1: System.Func<'T, 'K>, key2: System.Func<'U, 'K>, mapping: System.Func<'T, 'U, 'V>) : Either<'E, 'V> when 'K : equality =
        match first, second with
        | Left e, _        -> Left e
        | _, Left e        -> Left e
        | Right a, Right b -> Right (mapping.Invoke(a, b))


// Alternative

    [<System.Runtime.CompilerServices.Extension>]
    static member OrElse(first, second: Either<'E, 'T>) =
        match first with
        | Left _  -> second
        | Right _ -> first

    [<System.Runtime.CompilerServices.Extension>]
    static member OrElse(first, second: System.Func<Either<'E, 'T>>) =
        match first with
        | Left _  -> second.Invoke()
        | Right _ -> first

// Monad

    [<System.Runtime.CompilerServices.Extension>]
    static member SelectMany(source: Either<'E, 'T>, projection: System.Func<'T, Either<'E, 'U>>) =
        match source with
        | Left e  -> Left e
        | Right a -> projection.Invoke(a)

    [<System.Runtime.CompilerServices.Extension>]
    static member SelectMany(source: Either<'E, 'T>, projection: System.Func<'T, Either<'E, 'U>>, resultSelector: System.Func<'T, 'U, 'V>) =
        match source with
        | Left e  -> Left e
        | Right a -> match projection.Invoke(a) with
                     | Left e  -> Left e
                     | Right b -> Right (resultSelector.Invoke(a, b))

    [<System.Runtime.CompilerServices.Extension>]
    static member Flatten(source: Either<'E, Either<'E, 'T>>) =
        match source with
        | Left e  -> Left e
        | Right m -> m


// Foldable

    [<System.Runtime.CompilerServices.Extension>]
    static member Aggregate(source: Either<'E, 'T>, seed: 'State, func: System.Func<_,_,_>) : 'State =
        match source with
        | Left _  -> seed
        | Right a -> func.Invoke(seed, a)        

    [<System.Runtime.CompilerServices.Extension>]
    static member Aggregate(source: Either<'E, 'T>, seed: 'State, func: System.Func<_,_,_>, resultSelector: System.Func<_,_>) : 'Result =
        match source with
        | Left _  -> resultSelector.Invoke(seed)
        | Right a -> resultSelector.Invoke(func.Invoke(seed, a))        

    [<System.Runtime.CompilerServices.Extension>]
    static member Aggregate(source: Either<'E, 'T>, seed: 'State, func: System.Func<'State, 'T, struct (Either<'E, 'U> * 'State)>) =
        match source with
        | Left e  -> struct (Left e, seed)
        | Right a -> func.Invoke(seed, a)


// Bifoldablde

    [<System.Runtime.CompilerServices.Extension>]
    static member Aggregate(source: Either<'A, 'B>, seed: 'State, func1: System.Func<_,_,_>, func2: System.Func<_,_,_>) : 'State =
        match source with
        | Left a  -> func1.Invoke(seed, a)
        | Right b -> func2.Invoke(seed, b)

    [<System.Runtime.CompilerServices.Extension>]
    static member Aggregate(source: Either<'A, 'B>, seed: 'State, func1: System.Func<_,_,_>, func2: System.Func<_,_,_>, resultSelector: System.Func<'State,_>) : 'Result =
        match source with
        | Left a  -> resultSelector.Invoke(func1.Invoke(seed, a))
        | Right b -> resultSelector.Invoke(func2.Invoke(seed, b))

    [<System.Runtime.CompilerServices.Extension>]
    static member Aggregate(source: Either<'A, 'B>, seed: 'State, func1: System.Func<'State, 'A, struct (Either<'T, 'U> * 'State)>, func2: System.Func<'State, 'B, struct (Either<'T, 'U> * 'State)>) =
        match source with
        | Left a  -> func1.Invoke(seed, a)
        | Right b -> func2.Invoke(seed, b)