namespace Rogz.Context.Linq

open Rogz.Context.Base


[<AbstractClass; Sealed; System.Runtime.CompilerServices.Extension>]
type LinqMaybe =

// Functor

    [<System.Runtime.CompilerServices.Extension>]
    static member Select(source, mapping: System.Func<'T, 'U>) =
        match source with
        | Nothing -> Nothing
        | Just a  -> Just (mapping.Invoke a)

        
// Applicative

    [<System.Runtime.CompilerServices.Extension>]
    static member Apply(func: Maybe<System.Func<'T, 'U>>, value: Maybe<'T>) =
        match func, value with
        | Nothing, _ | _, Nothing -> Nothing
        | Just f, Just v          -> Just (f.Invoke(v))

    [<System.Runtime.CompilerServices.Extension>]
    static member Zip(first: Maybe<'T>, second: Maybe<'U>) =
        match first, second with
        | Nothing, _ | _, Nothing -> Nothing
        | Just a, Just b          -> Just (struct (a, b))        

    [<System.Runtime.CompilerServices.Extension>]
    static member Zip(first, second, mapping: System.Func<'T, 'U, 'V>) =
        match first, second with
        | Nothing, _ | _, Nothing -> Nothing
        | Just a, Just b          -> Just (mapping.Invoke(a, b))        

    [<System.Runtime.CompilerServices.Extension>]
    static member Join(first: Maybe<'T>, second, key1: System.Func<'T, 'K>, key2: System.Func<'U, 'K>, mapping: System.Func<'T, 'U, 'V>) : Maybe<'V> when 'K : equality =
        match first, second with
        | Nothing, _ | _, Nothing -> Nothing
        | Just a, Just b          -> if key1.Invoke(a) = key2.Invoke(b)
                                     then Just (mapping.Invoke(a, b))
                                     else Nothing


// Alternative

    [<System.Runtime.CompilerServices.Extension>]
    static member OrElse(first, second: Maybe<'T>) =
        match first with
        | Nothing -> second
        | Just _  -> first

    [<System.Runtime.CompilerServices.Extension>]
    static member OrElse(first, second: System.Func<Maybe<'T>>) =
        match first with
        | Nothing -> second.Invoke()
        | Just _  -> first


// Monad

    [<System.Runtime.CompilerServices.Extension>]
    static member SelectMany(source: Maybe<'T>, projection: System.Func<'T, Maybe<'U>>) =
        match source with
        | Nothing -> Nothing
        | Just a  -> projection.Invoke a

    [<System.Runtime.CompilerServices.Extension>]
    static member SelectMany(source: Maybe<'T>, projection: System.Func<'T, Maybe<'U>>, resultSelector: System.Func<'T, 'U, 'V>) =
        match source with
        | Nothing -> Nothing
        | Just a  -> match projection.Invoke a with
                     | Nothing -> Nothing
                     | Just b  -> Just (resultSelector.Invoke(a, b))

    [<System.Runtime.CompilerServices.Extension>]
    static member Flatten(source: Maybe<Maybe<'T>>) =
        match source with
        | Nothing -> Nothing
        | Just m  -> m


// MonadPlus

    [<System.Runtime.CompilerServices.Extension>]
    /// <summary>Generalizes the sequence-based 'filter' function.</summary>
    static member Where(source: Maybe<'T>, predicate: System.Func< ^T, bool>) =
        match source with
        | Nothing -> Nothing
        | Just a  -> if predicate.Invoke(a) then source else Nothing

    [<System.Runtime.CompilerServices.Extension>]
    // No apparent use in QUERIES, but still useful for method syntax.
    /// <summary>Acts similar to a SQL 'inner join', combining elements of each given monad when the elements satisfy a predicate.</summary>
    static member GroupJoin(first, second, predicate: System.Func<'T, 'U, bool>, mapping: System.Func<'T, 'U, 'V>) : Maybe<'V> when 'K: equality =
        match first, second with
        | Nothing, _ | _, Nothing -> Nothing
        | Just a, Just b          -> if predicate.Invoke(a, b) then Just (mapping.Invoke(a, b)) else Nothing

    [<System.Runtime.CompilerServices.Extension>]
    // Same as Join but allows 'into' syntax in queries.
    /// <summary>Correlates the element(s) of two sources based on equality of keys and selects the results.</summary>
    static member GroupJoin(outer: Maybe<'T>, inner: Maybe<'U>, outerKeySelector: System.Func<'T, 'K>, innerKeySelector: System.Func<'U, 'K>, resultSelector: System.Func<'T, Maybe<'U>, 'V>) : Maybe<'V> when 'K: equality =
        match outer, inner with
        | Nothing, _ | _, Nothing -> Nothing
        | Just a, Just b          -> if outerKeySelector.Invoke(a) = innerKeySelector.Invoke(b)
                                     then Just (resultSelector.Invoke(a, inner))
                                     else Nothing


// Foldable

    [<System.Runtime.CompilerServices.Extension>]
    static member Aggregate(source: Maybe<'T>, seed: 'State, func: System.Func<_,_,_>) : 'State =
        match source with
        | Nothing -> seed
        | Just a  -> func.Invoke(seed, a)        

    [<System.Runtime.CompilerServices.Extension>]
    static member Aggregate(source: Maybe<'T>, seed: 'State, func: System.Func<_,_,_>, resultSelector: System.Func<_,_>) : 'Result =
        match source with
        | Nothing -> resultSelector.Invoke(seed)
        | Just a  -> resultSelector.Invoke(func.Invoke(seed, a))        

    [<System.Runtime.CompilerServices.Extension>]
    static member Aggregate(source: Maybe<'T>, seed: 'State, func: System.Func<'State, 'T, struct (Maybe<'U> * 'State)>) =
        match source with
        | Nothing -> struct (Nothing, seed)
        | Just a  -> func.Invoke(seed, a)