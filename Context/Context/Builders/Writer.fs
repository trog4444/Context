namespace PTR.Context.Extension.Builder.Writer

open PTR.Context.Type.Writer
open PTR.Context.Type.Writer.Writer.Compose


[<RequireQualifiedAccess>]
module Combine =

    module Append =

        type Monad.WriterBuilder with

            member inline _.Delay f : ^a = f ()
            member inline _.Run f : ^f = f

            member inline _.Combine(a, b) : Writer< ^w, ^a>
                when ^a : (static member Append: ^a -> ^a -> ^a)
                and  ^w : (static member Append: ^w -> ^w -> ^w) = Semigroup.sappend a b


    module AsList =

        let inline private cons x xs = x::xs
        let inline private merge a b = [a; b]

        type Monad.WriterBuilder with

            member inline _.Delay f : ^a = f ()
            member inline _.Run f : ^f = f

            member inline _.Combine(a, b) : Writer< ^w, ^a list>
                when ^w : (static member Append: ^w -> ^w -> ^w) = Applicative.map2 merge a b

            member inline _.Combine(a, b) : Writer< ^w, ^a list>
                when ^w : (static member Append: ^w -> ^w -> ^w) = Applicative.map2 cons a b