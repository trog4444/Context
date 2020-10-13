namespace Rogz.Context.Base


[<Struct>]
type Writer<'Log, 'T> = { Log: 'Log; Value: 'T }
with

// Semigroup

    //[<CompiledName("Append")>]
    static member inline ( + ) ({ Writer.Log = la; Value = va }, { Writer.Log = lb; Value = vb }) : Writer< ^w, ^a> =
        { Writer.Log = (la: ^w) + (lb: ^w); Value = (va: ^a) + (vb: ^a) }


module Writer =

// Util

    //let inline private append' a b = (^a: (static member Append: ^a -> ^a -> ^a) (a, b))
    let inline private zero_ () = (^w: (static member Zero: unit -> ^w) ())


// Haskell Primitives

    let write (struct(w: 'w, a: 'a)) = { Log = w; Value = a }

    let tell (record: 'w) =
        { Writer.Log = record
        ; Value = () }

    let listen (writer: Writer<'w, 'a>) =
        { Writer.Log = writer.Log
        ; Value = struct (writer.Log, writer.Value) }   

    // pass

    let inline listens (f: ^w -> ^a -> ^b) (writer: Writer< ^w, ^a>) =
        { Writer.Log = writer.Log
        ; Value = f writer.Log writer.Value }

    // censor

    let inline runWriter f (writer: Writer< ^w, ^a>) : ^b =
        f writer.Log writer.Value

    // execWriter

    let inline mapWriter (f: (struct(^w0 * ^a) -> struct(^w * ^b))) { Writer.Log = w0; Value = a } =
        let struct (w,b) = f (struct(w0,a)) in { Log = w; Value = b }


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


// Applicative

    [<CompiledName("Unit")>]
    let inline unit value : Writer< ^w, ^a> =
        { Writer.Log = zero_ ()
        ; Value = value }

    let inline ap (fv: Writer< ^w, ^a>) (ff: Writer< ^w, (^a -> ^b)>) : Writer< ^w, ^b> =
        { Writer.Log = ff.Log + fv.Log
        ; Value = ff.Value fv.Value }

    let inline map2 f (fa: Writer< ^w, ^a>) (fb: Writer< ^w, ^b>) : Writer< ^w, ^c> =
        { Writer.Log = fa.Log + fb.Log
        ; Value = f fa.Value fb.Value }

    let inline sequence (source: Writer< ^w, ^a> seq) : Writer< ^w, ^a seq> =
        let mutable l = zero_ ()
        let d = ResizeArray<_>()
        for x in source do
            l <- l + x.Log
            d.Add(x.Value)
        { Writer.Log = l
        ; Value = d :> seq<_> }

    let inline traverse (f: ^a -> Writer< ^w, ^b>) (source: ^a seq) : Writer< ^w, ^b seq> =
        let mutable l = zero_ ()
        let xs = ResizeArray<_>()
        for x in source do
            let w = f x
            l <- l + w.Log
            xs.Add(w.Value)
        { Writer.Log = l
        ; Value = xs :> seq<_> }


// Biapplicative

    let biunit a b : Writer<'a, 'b> = { Writer.Log = a; Value = b }

    let inline bimap2 f g (fad: Writer< ^a, ^d>) (fbe: Writer< ^b, ^e>) : Writer< ^c, ^f> =
        { Writer.Log = f fad.Log fbe.Log
        ; Value = g fad.Value fbe.Value }


// Monad

    let inline bind f (m: Writer< ^w, ^a>) : Writer< ^w, ^b> =
        let w = f m.Value in { w with Writer.Log = m.Log + w.Log }

    let inline flatten mm : Writer< ^w, ^a> = bind id mm

    let inline fixM loop (em: Choice< ^a, Writer< ^w, ^a>>) : Writer< ^w, ^b> =
        let mutable l = Unchecked.defaultof< ^w>
        let rec go (w: Writer< ^w, ^a>) = l <- l + w.Log; k w.Value
        and k a = loop k go a
        { (match em with
           | Choice1Of2 a -> l <- zero_ (); k a
           | Choice2Of2 m -> l <- m.Log; k m.Value) with Writer.Log = l }

    
    [<RequireQualifiedAccess>]
    module Workflow =

        type WriterBuilder() =
            member inline _.Return(x) : Writer< ^w, ^a> = unit x
            member _.ReturnFrom(m: Writer<_, _>) : Writer<_, _> = m
            member inline _.Zero() : Writer< ^w, unit> = unit ()
            member inline _.Bind(m, f: (^a -> Writer< ^w, ^b>)) = bind f m            

    let writer = Workflow.WriterBuilder()


// Comonad

    let extract (w: Writer<'w, 'a>) = w.Value

    let inline extend f (w: Writer< ^w, ^a>) : Writer< ^w, ^b> =
        { Writer.Log = w.Log
        ; Value = f w }

    let duplicate (w: Writer<'w, 'a>) =
        { Writer.Log = w.Log
        ; Value = w }


// Semigroup

    let inline append (second: Writer< ^w, ^a>) (first: Writer< ^w, ^a>) : Writer< ^w, ^a> =
        { Writer.Log = first.Log + second.Log
        ; Value = first.Value + second.Value }


// Foldable

    let inline fold folder (seed: ^s) (ta: Writer< ^w, ^a>) : ^s =
        folder seed ta.Value

    let inline foldBack folder (seed: ^s) (ta: Writer< ^w, ^a>) : ^s =
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

    let inline bimapFold (mapping1: ^s -> ^a -> struct (^b * ^s)) (mapping2: ^s -> ^c -> struct (^d * ^s)) (seed: ^s) (t: Writer< ^a, ^c>) : struct (Writer< ^b, ^d> * ^s) =
        let struct (b, s) = mapping1 seed t.Log
        let struct (d, s) = mapping2 s t.Value
        struct ({ Writer.Log = b
                ; Value = d }, s)

    let inline bimapFoldBack (mapping1: ^a -> ^s -> struct (^b * ^s)) (mapping2: ^c -> ^s -> struct (^d * ^s)) (seed: ^s) (t: Writer< ^a, ^c>) : struct (Writer< ^b, ^d> * ^s) =
        let struct (b, s) = mapping1 t.Log seed
        let struct (d, s) = mapping2 t.Value s
        struct ({ Writer.Log = b; Value = d }, s)