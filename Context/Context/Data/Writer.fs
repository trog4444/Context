namespace Rogz.Context.Data.Writer

[<Struct>]
type Writer<'Log, 'T> = { Log: 'Log; Value: 'T } with

    member inline s.Select(f: System.Func< ^T, ^U>) =
        { Writer.Log = s.Log; Value = f.Invoke(s.Value) }

    member inline s.BiSelect(f: System.Func< ^Log, ^Log2>, g: System.Func< ^T, ^U>) =
        { Writer.Log = f.Invoke(s.Log); Value = g.Invoke(s.Value) }

    member inline s.Fold(seed, f: System.Func< ^S, ^T, ^S>) = f.Invoke(seed, s.Value)

    member inline s.FoldBack(seed, f: System.Func< ^T, ^S, ^S>) = f.Invoke(s.Value, seed)

    member inline s.BiFold(seed, fold1: System.Func< ^S, ^Log, ^S>, fold2: System.Func< ^S, ^T, ^S>) =
        fold2.Invoke(fold1.Invoke(seed, s.Log), s.Value)

    member inline s.BiFoldBack(seed, fold1: System.Func< ^Log, ^S, ^S>, fold2: System.Func< ^T, ^S, ^S>) =
        fold2.Invoke(s.Value, fold1.Invoke(s.Log, seed))