namespace Rogz.Context.Base


[<Struct>]
type Tagged<'Attribute, 'T> = Tag of 'T
with

// Util

    member inline internal s.Value = let (Tag a) = s in a

// Union

    member inline s.Match(f: System.Func< ^T, ^U>)    = f.Invoke(s.Value)
    member inline s.Match(action: System.Action< ^T>) = action.Invoke(s.Value)


module Tagged =

// F# Primitives

    let untag (tag: Tagged<'t, 'a>) = tag.Value

    let withtag<'tag, 'a> : ^a -> Tagged< ^tag, ^a> = Tag

    let retag<'tOld, 'tNew, 'a> : Tagged< ^tOld, ^a> -> Tagged< ^tNew, ^a> =
        fun (Tag a) -> Tag a    

    let proxy<'t> : Tagged<'t, unit> = Tag ()    

    let inline unproxy (f: Tagged< ^t, unit> -> ^a) : Tagged< ^t, ^a> = Tag (f proxy< ^t>)

    let inline taggedBy (_: Tagged< ^t, ^``_``>) f (x: ^t) : ^r = f x

    let tagOf (_: Tagged<'t, '``_``>) = typeof<'t>

    let tagSelf (x: 'a) : Tagged< ^a, ^a> = Tag x

    ///////////////// Are ANY of these useful in F# where the type system sucks compared to Haskell?
    //// New and working  
    //let swapPos (old: Tagged<'tObj, 'tVal>) : Tagged<'tVal, obj> = Tag (box old.Value)
    //let tryRevertTag (tag: Tagged<'tOld, obj>) : Tagged<'tOld, 'tOld option> = Tag (tryUnbox (tag.Value))
    

// Functor

    let inline map f (fa: Tagged< ^t, ^a>) : Tagged< ^t, ^b> = Tag (f fa.Value)


// Bifunctor

    let inline bimap (f: ^a -> ^c) (g: ^b -> ^d) (bf: Tagged< ^a, ^b>) : Tagged< ^c, ^d> = Tag (g bf.Value)

    let inline mapFirst (f: ^a -> ^c) (bf: Tagged< ^a, ^b>) : Tagged< ^c, ^b> = Tag bf.Value


// Applicative

    [<CompiledName("Unit")>]
    let unit (value: 'a) : Tagged< 't, ^a> = Tag value

    let inline ap (fv: Tagged< ^t, ^a>) (ff: Tagged< ^t, (^a -> ^b)>) : Tagged< ^t, ^b> =
        Tag (ff.Value fv.Value)

    let inline map2 f (fa: Tagged< ^t, ^a>) (fb: Tagged< ^t, ^b>) : Tagged< ^t, ^c> =
        Tag (f fa.Value fb.Value)

    let sequence (source: #seq<Tagged<'t, 'a>>) : Tagged< ^t, ^a seq> =
        Tag (System.Linq.Enumerable.Select(source, fun (Tag x) -> x))

    let inline traverse (f: ^a -> Tagged< ^t, ^b>) (source: #seq< ^a>) : Tagged< ^t, ^b seq> =
        Tag (System.Linq.Enumerable.Select(source, fun a -> let (Tag x) = f a in x))


// Monad

    let inline bind f (ma: Tagged< ^t, ^a>) : Tagged< ^t, ^b> = f ma.Value

    let flatten (mm: Tagged<'t, Tagged< ^t, 'a>>) = mm.Value

    let inline fixM loop em : Tagged< ^t, ^b> =
        let rec go (Tag a) = k a
        and k a = loop k go a
        match em with
        | Choice1Of2 a -> k a
        | Choice2Of2 m -> go (m: Tagged< ^t, ^a>)


    [<RequireQualifiedAccess>]
    module Workflow =

        type TaggedBuilder() =
            member _.Return(x) : Tagged<'t, 'a> = Tag x
            member _.ReturnFrom(m) : Tagged<'t, 'a> = m
            member _.Zero() : Tagged<'t, unit> = Tag ()
            member inline _.Bind(m: Tagged< ^t, ^a>, f) : Tagged< ^t, ^b> = bind f m            

    let tagged = Workflow.TaggedBuilder()


// Comonad

    let extract (w: Tagged<'t, 'a>) : ^a = w.Value

    let inline extend (f: Tagged< ^t, ^a> -> ^b) w : Tagged< ^t, ^b> =
        Tag (f w)

    let duplicate (w: Tagged<'t, 'a>) : Tagged< ^t, Tagged< ^t, ^a>> = Tag w


// Semigroup

    let inline append (second: Tagged< ^t, ^a>) (first: Tagged< ^t, ^a>) : Tagged< ^t, ^a> =
        Tag (first.Value + second.Value)    


// Foldable

    let inline fold folder (seed: ^s) (ta: Tagged< ^t, ^a>) : ^s =
        folder seed ta.Value

    let inline foldBack folder (seed: ^s) (ta: Tagged< ^t, ^a>) : ^s =
        folder ta.Value seed

    let inline mapFold mapping (seed: ^s) (ta: Tagged< ^t, ^a>) : struct (Tagged< ^t, ^b> * ^s) =
        let struct (r, s) = mapping seed ta.Value in (Tag r), s

    let inline mapFoldBack mapping (seed: ^s) (ta: Tagged< ^t, ^a>) : struct (Tagged< ^t, ^b> * ^s) =
        let struct (r, s) = mapping ta.Value seed in (Tag r), s