namespace PTR.Context.Extension.Builder.Writer

open PTR.Context.Type.Writer
open PTR.Context.Type.Writer.Writer.Compose.Monad


/// <summary>Adds default implementations of the 'Combine' method to the specified workflow builder.
///
/// NOTE: Only ONE 'Combine' module should be opened at a time in any scope.</summary>
[<RequireQualifiedAccess>]
module Combine =

    /// <summary>Multiple 'returns' result in all of the results being `appended` together.</summary>
    module Append =

        type WriterBuilder with

            member inline Delay : f: (unit -> ^a) -> ^a
            member inline Run : f: ^f -> ^f

            member inline Combine : Writer< ^w, ^a> * Writer< ^w, ^a> -> Writer< ^w, ^a>
                when ^a : (static member Append: ^a -> ^a -> ^a)
                and  ^w : (static member Append: ^w -> ^w -> ^w)


    /// <summary>Multiple 'returns' result in the entire workflow returning a list results.</summary>
    module AsList =

        type WriterBuilder with

            member inline Delay : f: (unit -> ^a) -> ^a
            member inline Run : f: ^f -> ^f

            member inline Combine : Writer< ^w, ^a> * Writer< ^w, ^a> -> Writer< ^w, ^a list>
                when ^w : (static member Append: ^w -> ^w -> ^w)

            member inline Combine : Writer< ^w, ^a> * Writer< ^w, ^a list> -> Writer< ^w, ^a list>
                when ^w : (static member Append: ^w -> ^w -> ^w)