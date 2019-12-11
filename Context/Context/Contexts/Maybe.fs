﻿namespace Rogz.Context.Data.Maybe


module Maybe =

// Primitives

    let inline caseof onNothing (onJust: ^a -> ^b) maybe =
        match maybe with Just a  -> onJust a | Nothing -> onNothing ()

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
        seq { for x in source do
                  match f x with
                  | Just b -> yield b
                  | Nothing -> () }


// Isomorphisms

    let toSeq maybe : 'a seq =
        match maybe with
        | Just a -> Seq.singleton a
        | Nothing -> Seq.empty

    let inline ofObj obj : Maybe< ^a> when ^a : null =
        if isNull obj then Nothing else Just obj

    let ofNullable (nullable: System.Nullable<'a>) =
        if nullable.HasValue then Just nullable.Value else Nothing

    let toNullable (maybe: Maybe<'a>) =
        match maybe with
        | Just a -> System.Nullable<_>(a)
        | Nothing -> System.Nullable<_>()

    let ofOption option : Maybe<'a> =
        match option with
        | Some a -> Just a
        | None -> Nothing

    let toOption maybe : Option<'a> =
        match maybe with
        | Just a -> Some a
        | Nothing -> None

    let ofVOption voption : Maybe<'a> =
        match voption with
        | ValueSome a -> Just a
        | ValueNone -> Nothing

    let toVOption maybe : ValueOption<'a> =
        match maybe with
        | Just a -> ValueSome a
        | Nothing -> ValueNone


// Recurs

    let inline recur loop seed : ^a = 
        let rec go a =
            match loop a with
            | Nothing -> a
            | Just a  -> go a
        go seed


// Functor

    let inline map (f: ^a -> ^b) fa =
        match fa with
        | Just a -> Just (f a)
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

    let andthen fb (fa: Maybe<'a>) : Maybe<'b> =
        match fa with Just _ -> fb | Nothing -> Nothing    

    let inline sequence (source: Maybe< ^a> seq) =
        let xs = ResizeArray<_>()
        let mutable flag = true
        use e = source.GetEnumerator()
        while flag && e.MoveNext() do
            match e.Current with
            | Just a -> xs.Add a
            | Nothing -> flag <- false
        if flag then Just (System.Linq.Enumerable.AsEnumerable(xs)) else Nothing

    let inline traverse f (source: ^a seq) : Maybe< ^b seq> =
        let xs = ResizeArray<_>()
        let mutable flag = true
        use e = source.GetEnumerator()
        while flag && e.MoveNext() do
            match f e.Current with
            | Just b -> xs.Add b
            | Nothing -> flag <- false
        if flag then Just (System.Linq.Enumerable.AsEnumerable(xs)) else Nothing


// Alternative

    let nil<'a> : Maybe< ^a> = Nothing

    let orElse second first : Maybe<'a> =
        match first with Just _ -> first | Nothing -> second

    let inline orElseWith second first : Maybe< ^a> =
        match first with Just _ -> first | Nothing -> second ()

    let inline concat (source: Maybe< ^a> seq) =
        System.Linq.Enumerable.FirstOrDefault(source, fun a -> isJust a)


// Monad    

    let inline bind f (ma: Maybe< ^a>) : Maybe< ^b> =
        match ma with Just a -> f a | Nothing -> Nothing

    let flatten mm : Maybe<'a> =
        match mm with Just m -> m | Nothing -> Nothing

    let inline fixM loop (em: Rogz.Context.Data.Either.Either< ^a, Maybe< ^a>>) : Maybe< ^b> =
        let rec go = function Just a -> k a | Nothing -> Nothing
        and k a = loop k go a
        match em with
        | Rogz.Context.Data.Either.Left a  -> k a
        | Rogz.Context.Data.Either.Right m -> go m

    // foldlM
    // foldrM

    [<RequireQualifiedAccess>]
    module Workflow =

        type MaybeBuilder() =
            member inline _.Return(x: ^a) = Just x
            member inline _.ReturnFrom m : Maybe< ^a> = m
            member inline _.Bind(m: Maybe< ^a>, f) : Maybe< ^b> = bind f m
            member inline _.Zero() = Just ()
            member inline _.Using(disp: ^d, f) : Maybe< ^a> when ^d :> System.IDisposable = using disp f
            member inline _.TryWith(m, h) : Maybe< ^a> = try m with e -> h e
            member inline _.TryFinally(m, f) : Maybe< ^a> = try m finally f ()


    let maybe = Workflow.MaybeBuilder()


// MonadPlus

    let guard condition = if condition then Just () else Nothing

    let inline join p (f: ^a -> ^b -> ^c) ma mb =
        match ma, mb with
        | Just a, Just b -> if p a b then Just (f a b) else Nothing
        | Nothing, _ | _, Nothing -> Nothing

    let inline filter p ma : Maybe< ^a> =
        match ma with
        | Just a -> if p a then ma else Nothing
        | Nothing -> Nothing


// Semigroup

    let inline append first second : Maybe< ^a> =
        match first, second with
        | Just a, Just b -> Just (^a: (static member Append: ^a -> ^a -> ^a) (a, b))
        | Nothing, b -> b
        | a, Nothing -> a


// Monoid

    let empty<'a> : Maybe< ^a> = Nothing

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

    let inline foldl (folder: (unit -> ^s) -> ^a -> ^s) seed ta =
        match ta with Nothing -> seed () | Just a -> folder seed a

    let inline foldr (folder: ^a -> (unit -> ^s) -> ^s) seed ta =
        match ta with Nothing -> seed () | Just a -> folder a seed

    let inline mapFold mapping (seed: ^s) (ta: Maybe< ^a>) : struct (Maybe< ^b> * ^s) =
        match ta with
        | Nothing -> Nothing, seed
        | Just a -> let struct (r, s) = mapping seed a in Just r, s

    let inline mapFoldBack mapping (seed: ^s) (ta: Maybe< ^a>) : struct (Maybe< ^b> * ^s) =
        match ta with
        | Nothing -> Nothing, seed
        | Just a -> let struct (r, s) = mapping a seed in Just r, s