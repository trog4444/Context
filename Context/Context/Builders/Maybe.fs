namespace PTR.Context.Builder.Maybe

open PTR.Context.Type.Maybe
open PTR.Context.Type.Maybe.Maybe
open PTR.Context.Type.Maybe.Maybe.Workflow


[<RequireQualifiedAccess>]
module Build =

    module Workflow =

        [<AbstractClass>]
        type MaybeRunnable () =
            inherit MaybeBuilder()
            member inline _.Delay f : ^a = f ()
            member inline _.Run f : ^f = f
            member inline s.Yield x : Maybe< ^a> = s.Return x
            member inline s.YieldFrom m : Maybe< ^a> = s.ReturnFrom m


        [<Sealed>]
        type MaybeList () =
            inherit MaybeRunnable()
            static member inline internal cons x xs = x::xs
            static member inline internal list a b = [a; b]
            member inline _.Combine(a: Maybe< ^a>, b: Maybe< ^a>) : Maybe< ^a list> = map2 MaybeList.list a b
            member inline _.Combine(a: Maybe< ^a>, bs: Maybe< ^a list>) : Maybe< ^a list> = map2 MaybeList.cons a bs
            member inline _.Combine(bs: Maybe< ^a list>, a: Maybe< ^a>) : Maybe< ^a list> = map2 (fun xs x -> xs @ [x]) bs a
            member inline _.Combine(az: Maybe< ^a list>, bz: Maybe< ^a list>) : Maybe< ^a list> = map2 (@) az bz
            member inline _.For(seq: #seq< ^a>, f: ^a -> Maybe< ^b>) : Maybe< ^b list> =
                traverse f seq |> map Seq.toList


        [<Sealed>]
        type MaybeAllJusts() =
            inherit MaybeRunnable()
            member inline _.Combine(a: Maybe< ^a>, b: Maybe< ^a>) : Maybe< ^a list> =
                match a, b with
                | Nothing, Nothing -> Nothing
                | Just a, Nothing -> Just [a]
                | Nothing, Just a -> Just [a]
                | Just a, Just b -> Just [a; b]
            member inline _.Combine(a: Maybe< ^a>, bs: Maybe< ^a list>) : Maybe< ^a list> =
                match a, bs with
                | Nothing, Nothing -> Nothing
                | Just a, Nothing -> Just [a]
                | Nothing, Just _ -> bs
                | Just a, Just bs -> Just (a::bs)
            member inline _.For(seq: #seq< ^a>, f: ^a -> Maybe< ^b>) : Maybe< ^b list> =
                traverse f seq |> map Seq.toList


        [<Sealed>]
        type MaybeFirstJust () =
            inherit MaybeRunnable()
            member inline _.Combine(a, b) : Maybe< ^a> = if isJust a then a else b


        [<Sealed>]
        type MaybeLastJust () =
            inherit MaybeRunnable()
            member inline _.Combine(a, b) : Maybe< ^a> = if isNothing b then a else b



    open Workflow

    let maybeAllJusts = MaybeAllJusts()
    let maybeList = MaybeList()
    let maybeFirstJust = MaybeFirstJust()
    let maybeLastJust = MaybeLastJust()
    

    module Extensions =

        type MaybeBuilder with

            member inline _.ReturnFrom(m: Option< ^a>) : Maybe< ^a> = ofOption m
            member inline _.ReturnFrom(m: ValueOption< ^a>) : Maybe< ^a> = ofVOption m
            member inline _.ReturnFrom(m: System.Nullable< ^a>) : Maybe< ^a> = ofNullable m

            member inline _.Bind(m: Option< ^a>, f) : Maybe< ^b> = match m with Some a -> f a | None -> Nothing
            member inline _.Bind(m: ValueOption< ^a>, f) : Maybe< ^b> = match m with ValueSome a -> f a | ValueNone -> Nothing
            member inline _.Bind(m: System.Nullable< ^a>, f) : Maybe< ^b> = if m.HasValue then f m.Value else Nothing


    module Linq =

        type MaybeBuilder with
            
            member inline s.Yield x : Maybe< ^a> = s.Return x

            member inline s.For(m, f) : Maybe< ^b> = s.Bind(m, f)

            [<CustomOperation("where", MaintainsVariableSpace = true)>]
            member inline _.Where(m, [<ProjectionParameter>] p: ^a -> bool) =
                filter p m

            [<CustomOperation("select", MaintainsVariableSpace = true)>]
            member inline _.Select(m, f: ^a -> ^b) =
                map f m

            [<CustomOperation("join", MaintainsVariableSpace = true, AllowIntoPattern = true)>]
            member inline _.Join
                (m: Maybe< ^a>
                ,n: Maybe< ^b>
                ,f: ^a -> ^b -> ^c
                ,k1: ^a -> ^k
                ,k2: ^b -> ^k) : Maybe< ^c> when ^k : equality =
                 join f k1 k2 m n

            [<CustomOperation("joinBy", MaintainsVariableSpace = true, AllowIntoPattern = true)>]
            member inline _.JoinBY
                (m: Maybe< ^a>
                ,n: Maybe< ^b>
                ,f: ^a -> ^b -> ^c
                ,k1: ^a -> ^b -> bool) : Maybe< ^c> =
                 joinBy f k1 m n