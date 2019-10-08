namespace PTR.Context.Builder.Either

open PTR.Context.Type.Either
open PTR.Context.Type.Either.Either
open PTR.Context.Type.Either.Either.Workflow


module Build =

    module Workflow =

        [<AbstractClass>]
        type EitherRunnable () =
            inherit EitherBuilder()
            member inline _.Delay f : ^a = f ()
            member inline _.Run f : ^f = f
            member inline s.Yield x : Either< ^e, ^a> = s.Return x
            member inline s.YieldFrom m : Either< ^e, ^a> = s.ReturnFrom m


        [<Sealed>]
        type EitherList () =
            inherit EitherRunnable()
            static member inline private cons x xs = x::xs
            static member inline private list a b = [a; b]
            member inline _.Combine(a: Either< ^e, ^a>, b: Either< ^e, ^a>) : Either< ^e, ^a list> = map2 EitherList.list a b
            member inline _.Combine(a: Either< ^e, ^a>, bs: Either< ^e, ^a list>) : Either< ^e, ^a list> = map2 EitherList.cons a bs
            member inline _.For(seq: #seq< ^a>, f: ^a -> Either< ^e, ^b>) : Either< ^e, ^b list> =
                traverse f seq |> map Seq.toList


        [<Sealed>]
        type EitherFirstRight () =
            inherit EitherRunnable()
            member inline _.Combine(a, b) : Either< ^e, ^a> = if isRight a then a else b


        [<Sealed>]
        type EitherLastRight () =
            inherit EitherRunnable()
            member inline _.Combine(a, b) : Either< ^e, ^a> = if isLeft b then a else b



    open Workflow

    let eitherList = EitherList()
    let eitherFirstRight = EitherFirstRight()
    let eitherLastRight = EitherLastRight()