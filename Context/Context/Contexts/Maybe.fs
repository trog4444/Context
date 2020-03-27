namespace Rogz.Context.Data.Maybe


module Maybe =

// Primitives

    let inline caseof onNothing (onJust: ^a -> ^b) maybe =
        match maybe with Just a -> onJust a | Nothing -> onNothing ()

    let isJust (maybe: Maybe<'a>) =
        match maybe with Just _ -> true | Nothing -> false

    let isNothing (maybe: Maybe<'a>) =
        match maybe with Just _ -> false | Nothing -> true

    let justs (source: Maybe<'a> seq) : ^a seq =
        seq { for x in source do
                  match x with
                  | Just a -> yield a
                  | Nothing -> () }

    let inline mapMaybes (f: ^a -> Maybe< ^b>) (source: ^a seq) : ^b seq =
        seq { for a in source do
                  match f a with
                  | Just b  -> yield b
                  | Nothing -> () }


// Isomorphisms

    //let toSeq maybe : 'a seq =
    //    match maybe with
    //    | Just a -> Seq.singleton a
    //    | Nothing -> Seq.empty

    let ofObj (obj: 'a) : Maybe<'a> when 'a : null =
        if isNull obj then Nothing else Just obj

    let ofNullable (nullable: System.Nullable<'a>) =
        if nullable.HasValue then Just nullable.Value else Nothing

    let toNullable (maybe: Maybe<'a>) =
        match maybe with
        | Just a -> System.Nullable<_>(a)
        | Nothing -> System.Nullable<_>()

    //let ofOption option : Maybe<'a> =
    //    match option with
    //    | Some a -> Just a
    //    | None -> Nothing

    //let toOption maybe : Option<'a> =
    //    match maybe with
    //    | Just a -> Some a
    //    | Nothing -> None

    //let ofVOption voption : Maybe<'a> =
    //    match voption with
    //    | ValueSome a -> Just a
    //    | ValueNone -> Nothing

    //let toVOption maybe : ValueOption<'a> =
    //    match maybe with
    //    | Just a  -> ValueSome a
    //    | Nothing -> ValueNone


//// Recurs

//    let inline recur loop seed : ^a = 
//        let rec go a =
//            match loop a with
//            | Nothing -> a
//            | Just a  -> go a
//        go seed


// Functor

    let inline map (f: ^a -> ^b) fa =
        match fa with
        | Just a  -> Just (f a)
        | Nothing -> Nothing


// Applicative

    let unit (value: 'a) = Just value

    let inline ap fv (ff: Maybe<(^a -> ^b)>) =
        match ff, fv with
        | Just f, Just v -> Just (f v)
        | Nothing, _ | _, Nothing -> Nothing

    let inline map2 (f: ^a -> ^b -> ^c) fa fb =
        match fa, fb with
        | Just a, Just b -> Just (f a b)
        | Nothing, _ | _, Nothing -> Nothing

    //let andthen fb (fa: Maybe<'a>) : Maybe<'b> =
    //    match fa with Just _ -> fb | Nothing -> Nothing    

    let sequence (source: #seq<Maybe<'a>>) =
        use e = source.GetEnumerator()
        let xs = ResizeArray<_>()
        let rec go () =
            if e.MoveNext() then
                match e.Current with
                | Nothing -> xs.Clear(); Nothing
                | Just a  -> go (xs.Add(a))
            else xs.Clear()
                 Just (System.Linq.Enumerable.AsEnumerable(xs))
        go ()


    let inline traverse f (source: #seq< ^a>) : Maybe< ^b seq> =
        use e = source.GetEnumerator()
        let xs = ResizeArray<_>()
        let rec go () =
            if e.MoveNext() then
                match f e.Current with
                | Nothing -> xs.Clear(); Nothing
                | Just a  -> go (xs.Add(a))
            else xs.Clear()
                 Just (System.Linq.Enumerable.AsEnumerable(xs))
        go ()


// Alternative

    let empty<'a> : Maybe< ^a> = Nothing

    let orElse second first : Maybe<'a> =
        match first with Just _ -> first | Nothing -> second

    let inline orElseWith second first : Maybe< ^a> =
        match first with Just _ -> first | Nothing -> second ()

    let concat (source: #seq<Maybe<'a>>) =
        System.Linq.Enumerable.FirstOrDefault(source, fun a -> isJust a)


// Monad    

    let inline bind f (ma: Maybe< ^a>) : Maybe< ^b> =
        match ma with Just a -> f a | Nothing -> Nothing

    let flatten mm : Maybe<'a> =
        match mm with Just m -> m | Nothing -> Nothing    

    let inline fixM loop (em: Choice< ^a, Maybe< ^a>>) : Maybe< ^b> =
        let rec go = function Just a -> k a | Nothing -> Nothing
        and k a = loop k go a
        match em with
        | Choice1Of2 a -> k a
        | Choice2Of2 m -> go m

    // foldlM
    // foldrM

    [<RequireQualifiedAccess>]
    module Workflow =

        type MaybeBuilder() =
            member _.Return(x: 'a) = Just x
            member _.ReturnFrom m : Maybe<'a> = m
            member inline _.Bind(m: Maybe< ^a>, f) : Maybe< ^b> = bind f m
            member _.Zero() = Just ()
            //member inline _.Using(disp: ^d, f) : Maybe< ^a> when ^d :> System.IDisposable = using disp f
            //member inline _.TryWith(m, h) : Maybe< ^a> = try m with e -> h e
            //member inline _.TryFinally(m, f) : Maybe< ^a> = try m finally f ()
            //abstract member Using: disp: 'd * f: ('d -> Maybe<'a>) -> Maybe<'a> when 'd :> System.IDisposable
            //abstract member TryWith: m: Maybe<'a> * h: (exn -> Maybe<'a>) -> Maybe<'a>
            //abstract member TryFinally: m: Maybe<'a> * f: (unit -> unit) -> Maybe<'a>
            //member _.Using(disp: 'd, f) : Maybe<'a> when 'd :> System.IDisposable = using disp f
            //default _.TryWith(m, h) : Maybe<'a> = try m with e -> h e
            //default _.TryFinally(m, f) : Maybe<'a> = try m finally f ()
            //member inline _.Run f = f
            //member inline _.Delay f = f ()


    let maybe = Workflow.MaybeBuilder()


// MonadPlus

    let guard condition = if condition then Just () else Nothing

    let inline join p (f: ^a -> ^b -> ^c) ma mb =
        match ma, mb with
        | Just a, Just b -> if p a b then Just (f a b) else Nothing
        | Nothing, _ | _, Nothing -> Nothing

    let inline filter p m : Maybe< ^a> =
        match m with
        | Just a  -> if p a then m else Nothing
        | Nothing -> m


// Semigroup

    let inline append first second : Maybe< ^a> =
        match first, second with
        | Just a, Just b -> Just (^a: (static member Append: ^a -> ^a -> ^a) (a, b))
        | Nothing, b -> b
        | a, Nothing -> a


// Monoid

    let inline mconcat (source: Maybe< ^a> seq) =
        Seq.fold append empty source

    let inline repeat count (elem: Maybe< ^a>) =
        if count <= 0 then Nothing else
            match elem with
            | Nothing -> elem
            | Just a -> let mutable sum = a
                        for _ = 1 to count - 1 do
                            sum <- (^a: (static member Append: ^a -> ^a -> ^a) (sum, a))
                        Just sum


// Foldable

    let inline fold (folder: ^s -> ^a -> ^s) seed ta =
        match ta with Nothing -> seed | Just a -> folder seed a

    let inline foldBack (folder: ^a -> ^s -> ^s) seed ta =
        match ta with Nothing -> seed | Just a -> folder a seed

    //let inline foldl (folder: (unit -> ^s) -> ^a -> ^s) seed ta =
    //    match ta with Nothing -> seed () | Just a -> folder seed a

    //let inline foldr (folder: ^a -> (unit -> ^s) -> ^s) seed ta =
    //    match ta with Nothing -> seed () | Just a -> folder a seed

    let inline mapFold mapping (seed: ^s) (ta: Maybe< ^a>) : struct (Maybe< ^b> * ^s) =
        match ta with
        | Nothing -> Nothing, seed
        | Just a -> let struct (r, s) = mapping seed a in Just r, s

    let inline mapFoldBack mapping (seed: ^s) (ta: Maybe< ^a>) : struct (Maybe< ^b> * ^s) =
        match ta with
        | Nothing -> Nothing, seed
        | Just a -> let struct (r, s) = mapping a seed in Just r, s