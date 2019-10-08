namespace PTR.Context.Builder.Either

open PTR.Context.Type.Either
open PTR.Context.Type.Either.Either
open PTR.Context.Type.Either.Either.Workflow


/// <summary>Customized builders/workflows for a given eitherext.</summary>
module Build =

    /// <summary>Customized builders/workflows for a given eitherext.</summary>
    module Workflow =

        /// <summary>Generic workflow with minimum added methods to use the 'Combine' method.</summary>
        [<AbstractClass>]
        type EitherRunnable =
            new : unit -> EitherRunnable
            inherit EitherBuilder
            member inline Delay : f: (unit -> ^a) -> ^a
            member inline Run : f: ^f -> ^f
            member inline Yield : x: ^a -> Either< ^e, ^a>
            member inline YieldFrom : m: Either< ^e, ^a> -> Either< ^e, ^a>


        /// <summary>Builder where the 'Combine' method uses the 'append' function from the base eitherext.</summary>
        [<Sealed>]
        type EitherAppend =
            new : unit -> EitherAppend
            inherit EitherRunnable
            member inline Combine : a: Either< ^e, ^a> * b: Either< ^e, ^a> -> Either< ^e, ^a>
                when ^a : (static member Append: ^a -> ^a -> ^a)


        /// <summary>Builder where the 'Combine' method allows for multiple results to be 'yielded' as a list.</summary>
        [<Sealed>]
        type EitherList =
            new : unit -> EitherList
            inherit EitherRunnable
            member inline Combine : a: Either< ^e, ^a> * b: Either< ^e, ^a> -> Either< ^e, ^a list>
            member inline Combine : a: Either< ^e, ^a> * bs: Either< ^e, ^a list> -> Either< ^e, ^a list>
            member inline For : seq: #seq< ^a> * f: (^a -> Either< ^e, ^b>) -> Either< ^e, ^b list>


        /// <summary>Builder where the 'Combine' method that returns the first 'Right'-value.</summary>
        [<Sealed>]
        type EitherFirstRight =
            new : unit -> EitherFirstRight
            inherit EitherRunnable
            member inline Combine : a: Either< ^e, ^a> * b: Either< ^e, ^a> -> Either< ^e, ^a>

        /// <summary>Builder where the 'Combine' method that returns the last 'Right'-value.</summary>
        [<Sealed>]
        type EitherLastRight =
            new : unit -> EitherLastRight
            inherit EitherRunnable
            member inline Combine : a: Either< ^e, ^a> * b: Either< ^e, ^a> -> Either< ^e, ^a>


    open Workflow

    /// <summary>Builder where the 'Combine' method uses the 'append' function from the base eitherext.</summary>
    val eitherAppend : EitherAppend

    /// <summary>Builder where the 'Combine' method allows for multiple results to be 'yielded' as a list.</summary>
    val eitherList : EitherList

    /// <summary>Builder where the 'Combine' method that returns the first 'Right'-value.</summary>
    val eitherFirstRight : EitherFirstRight

    /// <summary>Builder where the 'Combine' method that returns the last 'Right'-value.</summary>
    val eitherLastRight : EitherLastRight