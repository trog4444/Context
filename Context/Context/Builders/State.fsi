namespace PTR.Context.Extension.Builder.State

open PTR.Context.Type.State
open PTR.Context.Type.State.State.Compose.Monad


/// <summary>Adds default implementations of the 'Combine' method to the specified workflow builder.
///
/// NOTE: Only ONE 'Combine' module should be opened at a time in any scope.</summary>
[<RequireQualifiedAccess>]
module Combine =

    /// <summary>Multiple 'returns' result in all of the results being `appended` together.</summary>
    module Append =

        type StateBuilder with

            member inline Delay : f: (unit -> ^a) -> ^a
            member inline Run : f: ^f -> ^f

            member inline Combine : State< ^s, ^a> * State< ^s, ^a> -> State< ^s, ^a>
                when ^a : (static member Append: ^a -> ^a -> ^a)


    /// <summary>Multiple 'returns' result in the entire workflow returning a list results.</summary>
    module AsList =

        type StateBuilder with

            member inline Delay : f: (unit -> ^a) -> ^a
            member inline Run : f: ^f -> ^f

            member inline Combine : State< ^s, ^a> * State< ^s, ^a> -> State< ^s, ^a list>

            member inline Combine : State< ^s, ^a> * State< ^s, ^a list> -> State< ^s, ^a list>