namespace Rogz.Context.Data.Maybe

[<Struct>]
type Maybe<'T> = Nothing | Just of ^T with

// Union
    member inline s.Match(fNothing: System.Func< ^U>, fJust: System.Func< ^T, ^U>) =
        match s with Just a -> fJust.Invoke(a) | Nothing -> fNothing.Invoke()

    member inline s.Match(nothingAction: System.Action, justAction: System.Action< ^T>) =
        match s with Just a -> justAction.Invoke(a) | Nothing -> nothingAction.Invoke()

// Functor
    member inline s.Select(f: System.Func< ^T, ^U>) = match s with Nothing -> Nothing | Just a -> Just (f.Invoke a)
    
// Applicative
    member inline s.Zip(other, f: System.Func< ^T, ^U, ^V>) = match s, other with Just a, Just b -> Just (f.Invoke(a, b)) | Nothing, _ | _, Nothing -> Nothing
    member inline s.Join(other, kt: System.Func< ^T, ^K>, ku: System.Func< ^U, ^K>, f: System.Func< ^T, ^U, ^V>) =
        match s, other with
        | Just a, Just b -> if kt.Invoke(a) = ku.Invoke(b) then Just (f.Invoke(a, b)) else Nothing
        | Nothing, _ | _, Nothing -> Nothing

// Alternative
    static member inline Nix() : Maybe< ^a> = Nothing
    member inline s.OrElse(other: Maybe< ^T>) = match s with Just _ -> s | Nothing -> other

// Monad
    member inline s.SelectMany(f: System.Func< ^T, Maybe< ^U>>) = match s with Nothing -> Nothing | Just a -> f.Invoke a
    member inline s.SelectMany(f: System.Func< ^T, Maybe< ^U>>, g: System.Func< ^T, ^U, ^V>) = match s with Nothing -> Nothing | Just a -> match f.Invoke a with Nothing -> Nothing | Just b -> Just (g.Invoke(a, b))

// MonadPlus
    member inline s.GroupJoin(other, p: System.Func< ^T, ^U, bool>, f: System.Func< ^T, ^U, ^V>) : Maybe< ^V> when ^K: equality =
        match s, other with
        | Nothing, _ | _, Nothing -> Nothing
        | Just a, Just b -> if p.Invoke(a, b) then Just (f.Invoke(a, b)) else Nothing
    member inline s.GroupJoin(other, kt: System.Func< ^T, ^K>, ku: System.Func< ^U, ^K>, f: System.Func< ^T, Maybe< ^U>, ^V>) : Maybe< ^V> when ^K: equality =
        match s, other with
        | Nothing, _ | _, Nothing -> Nothing
        | Just a, Just b -> if kt.Invoke(a) = ku.Invoke(b) then Just (f.Invoke(a, other)) else Nothing

// MonadPlus.General
    member inline s.Where(p: System.Func< ^T, bool>) = match s with Nothing -> Nothing | Just a -> if p.Invoke(a) then s else Nothing

// Semigroup
    static member inline Append(first: Maybe< ^a>, second: Maybe< ^a>) : Maybe< ^a> when ^a: (static member Append: ^a -> ^a -> ^a) =
        match first, second with
        | Nothing, _ -> Nothing
        | _, Nothing -> Nothing
        | Just a, Just b -> Just (^a: (static member Append: ^a -> ^a -> ^a) (a, b))

// Monoid
    static member inline Empty() : Maybe< ^a> = Nothing

// Foldable
    member inline s.Fold(seed, f: System.Func< ^S, ^T, ^S>) = match s with Nothing -> seed | Just a -> f.Invoke(seed, a)
    member inline s.FoldBack(seed, f: System.Func< ^T, ^S, ^S>) = match s with Nothing -> seed | Just a -> f.Invoke(a, seed)