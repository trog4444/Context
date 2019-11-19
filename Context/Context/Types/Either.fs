namespace Rogz.Context.Data.Either

[<Struct>]
type Either<'L, 'R> = Left of left: 'L | Right of right: 'R with

// Union Type
    member inline s.Match(fLeft: System.Func< ^L, ^T>, fRight: System.Func< ^R, ^T>) =
        match s with Right r -> fRight.Invoke(r) | Left l -> fLeft.Invoke(l)

    member inline s.Match(leftAction: System.Action< ^L>, rightAction: System.Action< ^R>) =
        match s with Right r -> rightAction.Invoke(r) | Left l -> leftAction.Invoke(l)

// Functor
    member inline s.Select(f: System.Func< ^R, ^R2>) = match s with Left e -> Left e | Right a -> Right (f.Invoke(a))

// Bifunctor
    member inline s.BiSelect(f: System.Func< ^L, ^L2>, g: System.Func< ^R, ^R2>) = match s with Left e -> Left (f.Invoke(e)) | Right a -> Right (g.Invoke(a))

// Applicative
    member inline s.Zip(other, f: System.Func< ^R, ^S, ^T>) = match s, other with Right a, Right b -> Right (f.Invoke(a, b)) | Left e, _ -> Left e | _, Left e -> Left e
    member inline s.Join(other, _: System.Func< ^R, int>, _: System.Func< ^S, int>, f: System.Func< ^R, ^S, ^T>) = match s, other with Right a, Right b -> Right (f.Invoke(a, b)) | Left e, _ -> Left e | _, Left e -> Left e

// Alternative
    member inline s.OrElse(other) = match s with Right _ -> s | Left _ -> other

// Monad
    member inline s.SelectMany(f: System.Func< ^R, Either< ^L, ^S>>) = match s with Left e -> Left e | Right a -> f.Invoke a
    member inline s.SelectMany(f: System.Func< ^R, Either< ^L, ^S>>, g: System.Func< ^R, ^S, ^T>) = match s with Left e -> Left e | Right a -> match f.Invoke a with Left e -> Left e | Right b -> Right (g.Invoke(a, b))

// Semigroup
    static member inline Append(first: Either< ^e, ^a>, second: Either< ^e, ^a>) : Either< ^e, ^a> when ^a: (static member Append: ^a -> ^a -> ^a) =
        match first, second with
        | Right a, Right b -> Right (^a: (static member Append: ^a -> ^a -> ^a) (a, b))
        | Left e, _ -> Left e
        | _, Left e -> Left e
        
// Foldable
    member inline s.Fold(seed, f: System.Func< ^S, ^R, ^S>) = match s with Left _ -> seed | Right a -> f.Invoke(seed, a)
    member inline s.FoldBack(seed, f: System.Func< ^R, ^S, ^S>) = match s with Left _ -> seed | Right a -> f.Invoke(a, seed)

// Bifoldable
    member inline s.BiFold(seed, fold1: System.Func< ^S, ^L, ^S>, fold2: System.Func< ^S, ^R, ^S>) =
        match s with
        | Left a -> fold1.Invoke(seed, a)
        | Right b -> fold2.Invoke(seed, b)

    member inline s.BiFoldBack(seed, fold1: System.Func< ^L, ^S, ^S>, fold2: System.Func< ^R, ^S, ^S>) =
        match s with
        | Left a -> fold1.Invoke(a, seed)
        | Right b -> fold2.Invoke(b, seed)