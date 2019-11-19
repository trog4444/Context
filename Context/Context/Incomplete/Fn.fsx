//namespace Rogz.Context.Data.Fn


[<Struct>]
type Fn<'T, 'U> =
    | Fnf of fs: ('T -> 'U)
    | Fnn of fn: System.Func<'T, 'U> with

    member inline s.Invoke(x: ^T) : ^U =
        match s with Fnf f -> f x | Fnn f -> f.Invoke(x)



module Pattern =

    let inline ( |Fn| ) (f: Fn< ^T, ^U>) =
        match f with
        | Fnf f' -> Fn f'
        | Fnn f' -> Fn f'.Invoke
open Pattern


module Fn =

    let inline invoke x (Fn f) = f x

    //let inline curry f g ...