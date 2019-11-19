namespace Rogz.Context.Data.Attr

[<Struct>]
type Attr<'Attribute, 'T> = Attr of 'T with

// Util
    member inline internal s.Value = let (Attr a) = s in a

// Union
    member inline s.Match(f: System.Func< ^T, ^U>) = f.Invoke(s.Value)

    member inline s.Match(action: System.Action< ^T>) = action.Invoke(s.Value)

// Functor
    member inline s.Select(f: System.Func< ^T, ^U>) : Attr< ^Attribute, ^U> = Attr(f.Invoke(s.Value))

// Applicative
    member inline s.Zip(other: Attr< ^Attribute, ^U>, f: System.Func< ^T, ^U, ^V>) : Attr< ^Attribute, ^V> = Attr(f.Invoke(s.Value, other.Value))

    member inline s.Join(other: Attr< ^Attribute, ^U>, _: System.Func< ^T, int>, _: System.Func< ^U, int>, f: System.Func< ^T, ^U, ^V>) : Attr< ^Attribute, ^V> = Attr(f.Invoke(s.Value, other.Value))

// Monad
    member inline s.SelectMany(f: System.Func< ^T, Attr< ^Attribute, ^U>>) = f.Invoke(s.Value)

    member inline s.SelectMany(f: System.Func< ^T, Attr< ^Attribute, ^U>>, g: System.Func< ^T, ^U, ^V>) : Attr< ^Attribute, ^V> =
        let a = s.Value
        let (Attr b) = f.Invoke(a)
        Attr(g.Invoke(a, b))

// Comonad
    member inline s.ContinueWith(f: System.Func<Attr< ^Attribute, ^T>, ^U>) : Attr< ^Attribute, ^U> = Attr(f.Invoke(s))

// Semigroup
    static member inline Append(first: Attr< ^t, ^a>, second: Attr< ^t, ^a>) : Attr< ^t, ^a> = Attr(^a: (static member Append: ^a -> ^a -> ^a) (first.Value, second.Value))

// Foldable
    member inline s.Fold(seed, f: System.Func< ^S, ^T, ^S>) = f.Invoke(seed, s.Value)

    member inline s.FoldBack(seed, f: System.Func< ^T, ^S, ^S>) = f.Invoke(s.Value, seed)