namespace PTR.Context.Builder.Cont

open PTR.Context.Type.Cont
open PTR.Context.Type.Cont.Cont
open PTR.Context.Type.Cont.Cont.Workflow


/// <summary>Customized builders/workflows for a given context.</summary>
module Build =

    /// <summary>Customized builders/workflows for a given context.</summary>
    module Workflow =

        /// <summary>Generic workflow with minimum added methods to use the 'Combine' method.</summary>
        [<AbstractClass>]
        type ContRunnable =
            new : unit -> ContRunnable
            inherit ContBuilder
            member inline Delay : f: (unit -> ^a) -> ^a
            member inline Run : f: ^f -> ^f
            member inline Yield : x: ^a -> Cont< ^r, ^a>
            member inline YieldFrom : m: Cont< ^r, ^a> -> Cont< ^r, ^a>


        /// <summary>Builder where the 'Combine' method uses the 'append' function from the base context.</summary>
        [<Sealed>]
        type ContAppend =
            new : unit -> ContAppend
            inherit ContRunnable
            member inline Combine : a: Cont< ^r, ^a> * b: Cont< ^r, ^a> -> Cont< ^r, ^a>
                when ^a : (static member Append: ^a -> ^a -> ^a)


        /// <summary>Builder where the 'Combine' method allows for multiple results to be 'yielded' as a list.</summary>
        [<Sealed>]
        type ContList =
            new : unit -> ContList
            inherit ContRunnable
            member inline Combine : a: Cont< ^r, ^a> * b: Cont< ^r, ^a> -> Cont< ^r, ^a list>
            member inline Combine : a: Cont< ^r, ^a> * bs: Cont< ^r, ^a list> -> Cont< ^r, ^a list>
            member inline For : seq: #seq< ^a> * f: (^a -> Cont< ^r, ^b>) -> Cont< ^r, ^b list>



    open Workflow

    /// <summary>Builder where the 'Combine' method uses the 'append' function from the base context.</summary>
    val contAppend : ContAppend

    /// <summary>Builder where the 'Combine' method allows for multiple results to be 'yielded' as a list.</summary>
    val contList : ContList