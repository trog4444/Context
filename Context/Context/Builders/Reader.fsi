namespace PTR.Context.Extension.Builder.Reader

open PTR.Context.Type.Reader
open PTR.Context.Type.Reader.Reader.Compose.Monad


/// <summary>Adds default implementations of the 'Combine' method to the specified workflow builder.
///
/// NOTE: Only ONE 'Combine' module should be opened at a time in any scope.</summary>
[<RequireQualifiedAccess>]
module Combine =

    /// <summary>Multiple 'returns' result in all of the results being `appended` together.</summary>
    module Append =

        type ReaderBuilder with

            member inline Delay : f: (unit -> ^a) -> ^a
            member inline Run : f: ^f -> ^f

            member inline Combine : Reader< ^e, ^a> * Reader< ^e, ^a> -> Reader< ^e, ^a>
                when ^a : (static member Append: ^a -> ^a -> ^a)


    /// <summary>Multiple 'returns' result in the entire workflow returning a list results.</summary>
    module AsList =

        type ReaderBuilder with

            member inline Delay : f: (unit -> ^a) -> ^a
            member inline Run : f: ^f -> ^f

            member inline Combine : Reader< ^e, ^a> * Reader< ^e, ^a> -> Reader< ^e, ^a list>

            member inline Combine : Reader< ^e, ^a> * Reader< ^e, ^a list> -> Reader< ^e, ^a list>