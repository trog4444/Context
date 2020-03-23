namespace Rogz.Context.Data.Tagged


module Tagged =

// Primitives

    let untag (tag: Tagged<'t, 'a>) = tag.Value

    let withtag<'tag, 'a> : ^a -> Tagged< ^tag, ^a> = Tagged

    let retag<'tOld, 'tNew, 'a> : Tagged< ^tOld, ^a> -> Tagged< ^tNew, ^a> =
        fun (Tagged a) -> Tagged a

    let proxy<'t> : Tagged<'t, unit> = Tagged ()    

    let inline unproxy (f: Tagged< ^t, unit> -> ^a) : Tagged< ^t, ^a> = Tagged (f proxy< ^t>)

    let inline taggedBy (_: Tagged< ^t, ^``_``>) f (x: ^t) : ^r = f x

    let tagOf (_: Tagged<'t, '``_``>) = typeof<'t>

    let tagSelf (x: 'a) : Tagged< ^a, ^a> = Tagged x


// Functor

    let inline map f (fa: Tagged< ^t, ^a>) : Tagged< ^t, ^b> = Tagged (f fa.Value)


// Bifunctor

    let inline bimap (f: ^a -> ^c) (g: ^b -> ^d) (bf: Tagged< ^a, ^b>) : Tagged< ^c, ^d> = Tagged (g bf.Value)

    let inline mapFirst (f: ^a -> ^c) (bf: Tagged< ^a, ^b>) : Tagged< ^c, ^b> = Tagged bf.Value


// Applicative

    let unit (value: 'a) : Tagged< 't, ^a> = Tagged value

    let inline ap (fv: Tagged< ^t, ^a>) (ff: Tagged< ^t, (^a -> ^b)>) : Tagged< ^t, ^b> =
        Tagged (ff.Value fv.Value)

    let inline map2 f (fa: Tagged< ^t, ^a>) (fb: Tagged< ^t, ^b>) : Tagged< ^t, ^c> =
        Tagged (f fa.Value fb.Value)

    //let inline andthen (fb: Tagged< ^t, ^b>) (_: Tagged< ^t, ^a>) = fb

    let inline sequence (source: Tagged< ^t, ^a> seq) : Tagged< ^t, ^a seq> =
        Tagged (System.Linq.Enumerable.Select(source, fun (Tagged x) -> x))

    let inline traverse (f: ^a -> Tagged< ^t, ^b>) (source: ^a seq) : Tagged< ^t, ^b seq> =
        Tagged (System.Linq.Enumerable.Select(source, fun a -> let (Tagged x) = f a in x))


// Monad

    let inline bind f (ma: Tagged< ^t, ^a>) : Tagged< ^t, ^b> = f ma.Value

    let flatten (mm: Tagged<'t, Tagged< ^t, 'a>>) = mm.Value

    let inline fixM loop em : Tagged< ^t, ^b> =
        let rec go (Tagged a) = k a
        and k a = loop k go a
        match em with
        | Choice1Of2 a -> k a
        | Choice2Of2 m -> go (m: Tagged< ^t, ^a>)


    // foldlM
    // foldrM


    [<RequireQualifiedAccess>]
    module Workflow =

        type AttrBuilder() =
            member _.Return(x) : Tagged<'t, 'a> = unit x
            member _.ReturnFrom(m) : Tagged<'t, 'a> = m
            member inline _.Bind(m: Tagged< ^t, ^a>, f) : Tagged< ^t, ^b> = bind f m
            member _.Zero() : Tagged<'t, unit> = unit ()
            //abstract member Using: disp: 'd * f: ('d -> Tagged<'t, 'a>) -> Tagged<'t, 'a> when 'd :> System.IDisposable
            //abstract member TryWith: m: Tagged<'t, 'a> * h: (exn -> Tagged<'t, 'a>) -> Tagged<'t, 'a>
            //abstract member TryFinally: m: Tagged<'t, 'a> * f: (unit -> unit) -> Tagged<'t, 'a>
            member _.Using(disp: 'd, f) : Tagged<'t, 'a> when 'd :> System.IDisposable = using disp f
            //default _.TryWith(m, h) : Tagged<'t, 'a> = try m with e -> h e
            //default _.TryFinally(m, f) : Tagged<'t, 'a> = try m finally f ()


    let tag = Workflow.AttrBuilder()


// Comonad

    let extract (w: Tagged<'t, 'a>) : ^a = w.Value

    let inline extend (f: Tagged< ^t, ^a> -> ^b) w : Tagged< ^t, ^b> =
        Tagged (f w)

    let duplicate (w: Tagged<'t, 'a>) : Tagged< ^t, Tagged< ^t, ^a>> = Tagged w


// Semigroup

    let inline append (first: Tagged< ^t, ^a>) (second: Tagged< ^t, ^a>) : Tagged< ^t, ^a> =
        Tagged (^a: (static member Append: ^a -> ^a -> ^a) (first.Value, second.Value))


// Foldable

    let inline fold folder (seed: ^s) (ta: Tagged< ^t, ^a>) : ^s =
        folder seed ta.Value

    let inline foldBack folder (seed: ^s) (ta: Tagged< ^t, ^a>) : ^s =
        folder ta.Value seed

    let inline foldl folder (seed: unit -> ^s) (ta: Tagged< ^t, ^a>) : ^s =
        folder seed ta.Value

    let inline foldr folder (seed: unit -> ^s) (ta: Tagged< ^t, ^a>) : ^s =
        folder ta.Value seed

    let inline mapFold mapping (seed: ^s) (ta: Tagged< ^t, ^a>) : struct (Tagged< ^t, ^b> * ^s) =
        let struct (r, s) = mapping seed ta.Value in Tagged r, s

    let inline mapFoldBack mapping (seed: ^s) (ta: Tagged< ^t, ^a>) : struct (Tagged< ^t, ^b> * ^s) =
        let struct (r, s) = mapping ta.Value seed in Tagged r, s