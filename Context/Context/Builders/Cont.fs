namespace PTR.Context.Builder.Cont

open PTR.Context.Type.Cont
open PTR.Context.Type.Cont.Cont
open PTR.Context.Type.Cont.Cont.Workflow


module Build =

    module Workflow =

        [<AbstractClass>]
        type ContRunnable () =
            inherit ContBuilder()
            member inline _.Delay f : ^a = f ()
            member inline _.Run f : ^f = f
            member inline s.Yield x : Cont< ^r, ^a> = s.Return x
            member inline s.YieldFrom m : Cont< ^r, ^a> = s.ReturnFrom m


        [<Sealed>]
        type ContAppend () =
            inherit ContRunnable()
            member inline _.Combine(a: Cont< ^r, ^a>, b: Cont< ^r, ^a>) : Cont< ^r, ^a> = append a b


        [<Sealed>]
        type ContList () =
            inherit ContRunnable()
            static member inline private cons x xs = x::xs
            static member inline private list a b = [a; b]
            member inline _.Combine(a: Cont< ^r, ^a>, b: Cont< ^r, ^a>) : Cont< ^r, ^a list> = map2 ContList.list a b
            member inline _.Combine(a: Cont< ^r, ^a>, bs: Cont< ^r, ^a list>) : Cont< ^r, ^a list> = map2 ContList.cons a bs
            member inline _.For(seq: #seq< ^a>, f: ^a -> Cont< ^r, ^b>) : Cont< ^r, ^b list> = traverse f seq



    open Workflow

    let contAppend = ContAppend()
    let contList = ContList()