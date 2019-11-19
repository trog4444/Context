namespace Rogz.Context.Data.Writer


module Writer =

// Util

    let inline private append' a b = (^a: (static member Append: ^a -> ^a -> ^a) (a, b))
    let inline private mt () = (^w: (static member Empty: unit -> ^w) ())


// Minimal

    let inline tell (record: ^w) =
        { Writer.Log = record
        ; Value = () }

    let inline listen (writer: Writer< ^w, ^a>) =
        { Writer.Log = writer.Log
        ; Value = struct (writer.Log, writer.Value) }   


// Primitives

    let inline runWriter f (writer: Writer< ^w, ^a>) : ^b =
        f writer.Log writer.Value

    let inline read (writer: Writer< ^w, ^a>) = writer.Log

    let inline listens (f: ^w -> ^b) (writer: Writer< ^w, ^a>) =
        { Writer.Log = writer.Log
        ; Value = struct (writer.Value, f writer.Log) }


// Functor

    let inline map (f: ^a -> ^b) (fa: Writer< ^w, ^a>) =
        { Writer.Log = fa.Log
        ; Value = f fa.Value }


// Bifunctor

    let inline bimap (f: ^a -> ^c) (g: ^b -> ^d) (bf: Writer< ^a, ^b>) =
        { Writer.Log = f bf.Log
        ; Value = g bf.Value }

    let inline mapFirst (f: ^a -> ^c) (bf: Writer< ^a, ^b>) =
        { Writer.Log = f bf.Log
        ; Value = bf.Value }

    let inline mapSecond (g: ^b -> ^d) (bf: Writer< ^a, ^b>) =
        { Writer.Log = bf.Log
        ; Value = g bf.Value }


// Applicative

    let inline unit value : Writer< ^w, ^a> =
        { Writer.Log = mt ()
        ; Value = value }

    let inline ap (fv: Writer< ^w, ^a>) (ff: Writer< ^w, (^a -> ^b)>) =
        { Writer.Log = append' ff.Log fv.Log
        ; Value = ff.Value fv.Value }

    let inline map2 f (fa: Writer< ^w, ^a>) (fb: Writer< ^w, ^b>) : Writer< ^w, ^c> =
        { Writer.Log = append' fa.Log fb.Log
        ; Value = f fa.Value fb.Value }

    let inline andthen (fb: Writer< ^w, ^b>) (fa: Writer< ^w, ^a>) =
        { fb with Writer.Log = append' fa.Log fb.Log }


// Monad

    let inline bind f (m: Writer< ^w, ^a>) : Writer< ^w, ^b> =
        let w = f m.Value in { w with Writer.Log = append' m.Log w.Log }

    let inline flatten mm : Writer< ^w, ^a> = bind id mm

    let inline fixM loop (em: Rogz.Context.Data.Either.Either< ^a, Writer< ^w, ^a>>) : Writer< ^w, ^b> =
        let mutable l = Unchecked.defaultof< ^w>
        let rec go (w: Writer< ^w, ^a>) = l <- append' l w.Log; k w.Value
        and k a = loop k go a
        { (match em with
           | Rogz.Context.Data.Either.Left a  -> l <- mt (); k a
           | Rogz.Context.Data.Either.Right m -> l <- m.Log; k m.Value) with Writer.Log = l }

    
    [<RequireQualifiedAccess>]
    module Workflow =

        type WriterBuilder() =
            member inline _.Return(x) : Writer< ^w, ^a> = unit x
            member inline _.ReturnFrom(m) : Writer< ^w, ^a> = m
            member inline _.Bind(m, f: (^a -> Writer< ^w, ^b>)) = bind f m
            member inline _.Zero() : Writer< ^w, unit> = unit ()
            member inline _.Using(disp: ^d, f) : Writer< ^w, ^a> when ^d :> System.IDisposable = using disp f
            member inline _.TryWith(m, h) : Writer< ^w, ^a> = try m with e -> h e
            member inline _.TryFinally(m, f) : Writer< ^w, ^a> = try m finally f ()


    let writer = Workflow.WriterBuilder()


// Comonad

    let inline extract (w: Writer< ^w, ^a>) = w.Value

    let inline extend f (w: Writer< ^w, ^a>) : Writer< ^w, ^b> =
        { Writer.Log = w.Log
        ; Value = f w }

    let inline duplicate (w: Writer< ^w, ^a>) =
        { Writer.Log = w.Log
        ; Value = w }


// Semigroup

    let inline append (first: Writer< ^w, ^a>) (second: Writer< ^w, ^a>) =
        { Writer.Log = append' first.Log second.Log
        ; Value = append' first.Value second.Value }


// Foldable

    let inline fold folder (seed: ^s) (ta: Writer< ^w, ^a>) : ^s =
        folder seed ta.Value

    let inline foldBack folder (seed: ^s) (ta: Writer< ^w, ^a>) : ^s =
        folder ta.Value seed

    let inline foldl folder (seed: unit -> ^s) (ta: Writer< ^w, ^a>) : ^s =
        folder seed ta.Value

    let inline foldr folder (seed: unit -> ^s) (ta: Writer< ^w, ^a>) : ^s =
        folder ta.Value seed

    let inline mapFold mapping (seed: ^s) (ta: Writer< ^w, ^a>) : struct (Writer< ^w, ^b> * ^s) =
        let struct (r, s) = mapping seed ta.Value
        struct ({ Writer.Log = ta.Log
                ; Value = r }, s)

    let inline mapFoldBack mapping (seed: ^s) (ta: Writer< ^w, ^a>) : struct (Writer< ^w, ^b> * ^s) =
        let struct (r, s) = mapping ta.Value seed
        struct ({ Writer.Log = ta.Log
                ; Value = r }, s)


// Bifoldable

    let inline bifold (fold1: ^s -> ^a -> ^s) (fold2: ^s -> ^b -> ^s) (seed: ^s) (t: Writer< ^a, ^b>) : ^s =
        fold2 (fold1 seed t.Log) t.Value

    let inline bifoldBack (fold1: ^a -> ^s -> ^s) (fold2: ^b -> ^s -> ^s) (seed: ^s) (t: Writer< ^a, ^b>) : ^s =
        fold2 t.Value (fold1 t.Log seed)

    let inline bifoldl (fold1: (unit -> ^s) -> ^a -> ^s) (fold2: (unit -> ^s) -> ^b -> ^s) (seed: unit -> ^s) (t: Writer< ^a, ^b>) : ^s =
        fold2 (fun () -> fold1 seed t.Log) t.Value

    let inline bifoldr (fold1: ^a -> (unit -> ^s) -> ^s) (fold2: ^b -> (unit -> ^s) -> ^s) (seed: unit -> ^s) (t: Writer< ^a, ^b>) : ^s =
        fold2 t.Value (fun () -> fold1 t.Log seed)

    let inline bimapFold (mapping1: ^s -> ^a -> struct (^b * ^s)) (mapping2: ^s -> ^c -> struct (^d * ^s)) (seed: ^s) (t: Writer< ^a, ^c>) : struct (Writer< ^b, ^d> * ^s) =
        let struct (b, s) = mapping1 seed t.Log
        let struct (d, s) = mapping2 s t.Value
        struct ({ Writer.Log = b
                ; Value = d }, s)

    let inline bimapFoldBack (mapping1: ^a -> ^s -> struct (^b * ^s)) (mapping2: ^c -> ^s -> struct (^d * ^s)) (seed: ^s) (t: Writer< ^a, ^c>) : struct (Writer< ^b, ^d> * ^s) =
        let struct (b, s) = mapping1 t.Log seed
        let struct (d, s) = mapping2 t.Value s
        struct ({ Writer.Log = b
                ; Value = d }, s)


// Traversable

    let inline sequence (source: Writer< ^w, ^a> seq) : Writer< ^w, ^a seq> =
        let mutable l = mt ()
        let d = ResizeArray<_>()
        for x in source do
            l <- append' l x.Log
            d.Add(x.Value)
        { Writer.Log = l
        ; Value = System.Linq.Enumerable.AsEnumerable(d) }

    let inline traverse f (source: ^a seq) : Writer< ^w, ^b seq> =
        let mutable l = mt ()
        let d = ResizeArray<_>()
        for x in source do
            let w = f x
            l <- append' l w.Log
            d.Add(w.Value)
        { Writer.Log = l
        ; Value = System.Linq.Enumerable.AsEnumerable(d) }