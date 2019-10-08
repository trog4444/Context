namespace PTR.Context.Extension.Builder.State

open PTR.Context.Type.State
open PTR.Context.Type.State.State.Compose


[<RequireQualifiedAccess>]
module Combine =

    module Append =

        type Monad.StateBuilder with

            member inline _.Delay f : ^a = f ()
            member inline _.Run f : ^f = f

            member inline _.Combine(a, b) : State< ^s, ^a>
                when ^a : (static member Append: ^a -> ^a -> ^a) = Semigroup.sappend a b


    module AsList =

        let inline private cons x xs = x::xs
        let inline private merge a b = [a; b]

        type Monad.StateBuilder with

            member inline _.Delay f : ^a = f ()
            member inline _.Run f : ^f = f

            member inline _.Combine(a, b) : State< ^s, ^a list> = Applicative.map2 merge a b
            member inline _.Combine(a, b) : State< ^s, ^a list> = Applicative.map2 cons a b
