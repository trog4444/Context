namespace PTR.Context.Extension.Builder.Lens

open PTR.Context.Type.Lens
open PTR.Context.Type.Lens.Lens.Compose


[<RequireQualifiedAccess>]
module Combine =

    module Append =

        type Monad.LensBuilder with

            member inline _.Delay f : ^a = f ()
            member inline _.Run f : ^f = f

            member inline _.Combine(a, b) : Lens< ^p, ^a>
                when ^a : (static member Append: ^a -> ^a -> ^a) = Semigroup.sappend a b


    module AsList =

        let inline private cons x xs = x::xs
        let inline private merge a b = [a; b]

        type Monad.LensBuilder with

            member inline _.Delay f : ^a = f ()
            member inline _.Run f : ^f = f

            member inline _.Combine(a, b) : Lens< ^p, ^a list> = Applicative.map2 merge a b
            member inline _.Combine(a, b) : Lens< ^p, ^a list> = Applicative.map2 cons a b
