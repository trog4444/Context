namespace Rogz.Context.Data.Tagged


[<Struct>]
type Tagged<'Attribute, 'T> = Tagged of 'T with

// Util
    member inline internal s.Value = let (Tagged a) = s in a

// Union
    member inline s.Match(f: System.Func< ^T, ^U>) = f.Invoke(s.Value)

    member inline s.Match(action: System.Action< ^T>) = action.Invoke(s.Value)

// Functor
    member inline s.Select(f: System.Func< ^T, ^U>) : Tagged< ^Attribute, ^U> = Tagged(f.Invoke(s.Value))

// Applicative
    member inline s.Zip(other: Tagged< ^Attribute, ^U>, f: System.Func< ^T, ^U, ^V>) : Tagged< ^Attribute, ^V> = Tagged(f.Invoke(s.Value, other.Value))

    member inline s.Join(other: Tagged< ^Attribute, ^U>, _: System.Func< ^T, int>, _: System.Func< ^U, int>, f: System.Func< ^T, ^U, ^V>) : Tagged< ^Attribute, ^V> = Tagged(f.Invoke(s.Value, other.Value))

// Monad
    member inline s.SelectMany(f: System.Func< ^T, Tagged< ^Attribute, ^U>>) = f.Invoke(s.Value)

    member inline s.SelectMany(f: System.Func< ^T, Tagged< ^Attribute, ^U>>, g: System.Func< ^T, ^U, ^V>) : Tagged< ^Attribute, ^V> =
        let a = s.Value
        let (Tagged b) = f.Invoke(a)
        Tagged(g.Invoke(a, b))

// Comonad
    member inline s.ContinueWith(f: System.Func<Tagged< ^Attribute, ^T>, ^U>) : Tagged< ^Attribute, ^U> = Tagged(f.Invoke(s))

// Semigroup
    static member inline Append(first: Tagged< ^t, ^a>, second: Tagged< ^t, ^a>) : Tagged< ^t, ^a> = Tagged(^a: (static member Append: ^a -> ^a -> ^a) (first.Value, second.Value))

// Foldable
    member inline s.Fold(seed, f: System.Func< ^S, ^T, ^S>) = f.Invoke(seed, s.Value)

    member inline s.FoldBack(seed, f: System.Func< ^T, ^S, ^S>) = f.Invoke(s.Value, seed)