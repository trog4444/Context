namespace PTR.Context.Extension.Builder.Writer

open PTR.Context.Type
open Writer.Compose.Monad


/// Adds default implementations of the `Combine` method to the specified workflow builder.
///
/// NOTE: Only 'ONE' `Combine` module should be opened at a time in any scope.
[<RequireQualifiedAccess>]
module Combine =

    /// Multiple `returns` result in all of the results being `appended` together.
    module Append =

        type WriterBuilder with

            member inline s.Combine(a: Writer< ^l, ^a>, b: Writer< ^l, ^a>) =
                { Log = (^l: (static member Append: ^l -> ^l -> ^l) (a.Log, b.Log))
                ; Value = (^a: (static member Append: ^a -> ^a -> ^a) (a.Value, b.Value)) }


    /// Multiple `returns` result in the entire workflow returning a list of results.
    module AsList =

        type WriterBuilder with

            member inline s.Combine(a: Writer< ^l, ^a>, b: Writer< ^l, ^a>) =
                { Log = (^l: (static member Append: ^l -> ^l -> ^l) (a.Log, b.Log))
                ; Value = [a.Value; b.Value] }

            member inline s.Combine(a: Writer< ^l, ^a>, b: Writer< ^l, ^a list>) =
                { Log = (^l: (static member Append: ^l -> ^l -> ^l) (a.Log, b.Log))
                ; Value = a.Value :: b.Value }