namespace Rogz.Context.Data.Attr


module Attr =

// Primitives

    let unAttr (attr: Attr<'t, 'a>) = attr.Value

    let wAttr<'attr, 'a> : ^a -> Attr< ^attr, ^a> = Attr

    let setAttr<'tOld, 'tNew, 'a> : Attr< ^tOld, ^a> -> Attr< ^tNew, ^a> =
        fun (Attr a) -> Attr a


// Functor

    let inline map f (fa: Attr< ^t, ^a>) : Attr< ^t, ^b> = Attr (f fa.Value)


// Applicative

    let unit (value: 'a) : Attr< 't, ^a> = Attr value

    let inline ap (fv: Attr< ^t, ^a>) (ff: Attr< ^t, (^a -> ^b)>) : Attr< ^t, ^b> =
        Attr (ff.Value fv.Value)

    let inline map2 f (fa: Attr< ^t, ^a>) (fb: Attr< ^t, ^b>) : Attr< ^t, ^c> =
        Attr (f fa.Value fb.Value)

    let inline andthen (fb: Attr< ^t, ^b>) (_: Attr< ^t, ^a>) = fb

    let inline sequence (source: Attr< ^t, ^a> seq) : Attr< ^t, ^a seq> =
        Attr (System.Linq.Enumerable.Select(source, fun (Attr x) -> x))

    let inline traverse (f: ^a -> Attr< ^t, ^b>) (source: ^a seq) : Attr< ^t, ^b seq> =
        Attr (System.Linq.Enumerable.Select(source, fun a -> let (Attr x) = f a in x))


// Monad

    let inline bind f (ma: Attr< ^t, ^a>) : Attr< ^t, ^b> = f ma.Value

    let flatten (mm: Attr<'t, Attr< ^t, 'a>>) = mm.Value

    let inline fixM (loop: (^a -> Attr< ^t, ^b>) -> (Attr< ^t, ^a> -> Attr< ^t, ^b>) -> ^a -> Attr< ^t, ^b>) (em: Rogz.Context.Data.Either.Either< ^a, Attr< ^t, ^a>>) : Attr< ^t, ^b> =
        let rec go (Attr a) = k a
        and k a = loop k go a
        match em with
        | Rogz.Context.Data.Either.Left a  -> k a
        | Rogz.Context.Data.Either.Right m -> go m


    // foldlM
    // foldrM


    [<RequireQualifiedAccess>]
    module Workflow =

        type AttrBuilder() =
            member inline _.Return(x) : Attr< ^t, ^a> = Attr x
            member inline _.ReturnFrom(m) : Attr< ^t, ^a> = m
            member inline _.Bind(m: Attr< ^t, ^a>, f) : Attr< ^t, ^b> = bind f m
            member inline _.Zero() : Attr< ^t, unit> = Attr ()
            member inline _.Using(disp: ^d, f) : Attr< ^t, ^a> when ^d :> System.IDisposable = using disp f
            member inline _.TryWith(m, h) : Attr< ^t, ^a> = try m with e -> h e
            member inline _.TryFinally(m, f) : Attr< ^t, ^a> = try m finally f ()


    let attr = Workflow.AttrBuilder()


// Comonad

    let extract (w: Attr<'t, 'a>) : ^a = w.Value

    let inline extend (f: Attr< ^t, ^a> -> ^b) w : Attr< ^t, ^b> =
        Attr (f w)

    let duplicate (w: Attr<'t, 'a>) : Attr< ^t, Attr< ^t, ^a>> = Attr w


// Semigroup

    let inline append (first: Attr< ^t, ^a>) (second: Attr< ^t, ^a>) : Attr< ^t, ^a> =
        Attr (^a: (static member Append: ^a -> ^a -> ^a) (first.Value, second.Value))


// Foldable

    let inline fold folder (seed: ^s) (ta: Attr< ^t, ^a>) : ^s =
        folder seed ta.Value

    let inline foldBack folder (seed: ^s) (ta: Attr< ^t, ^a>) : ^s =
        folder ta.Value seed

    let inline foldl folder (seed: unit -> ^s) (ta: Attr< ^t, ^a>) : ^s =
        folder seed ta.Value

    let inline foldr folder (seed: unit -> ^s) (ta: Attr< ^t, ^a>) : ^s =
        folder ta.Value seed

    let inline mapFold mapping (seed: ^s) (ta: Attr< ^t, ^a>) : struct (Attr< ^t, ^b> * ^s) =
        let struct (r, s) = mapping seed ta.Value in Attr r, s

    let inline mapFoldBack mapping (seed: ^s) (ta: Attr< ^t, ^a>) : struct (Attr< ^t, ^b> * ^s) =
        let struct (r, s) = mapping ta.Value seed in Attr r, s