namespace PTR.Context.Extension.Builder.Lens

open PTR.Context.Type.Lens
open PTR.Context.Type.Lens.Lens.Compose.Monad


/// <summary>Adds default implementations of the 'Combine' method to the specified workflow builder.
///
/// NOTE: Only ONE 'Combine' module should be opened at a time in any scope.</summary>
[<RequireQualifiedAccess>]
module Combine =

    /// <summary>Multiple 'returns' result in all of the results being `appended` together.</summary>
    module Append =

        type LensBuilder with

            member inline Delay : f: (unit -> ^a) -> ^a
            member inline Run : f: ^f -> ^f

            member inline Combine : Lens< ^p, ^a> * Lens< ^p, ^a> -> Lens< ^p, ^a>
                when ^a : (static member Append: ^a -> ^a -> ^a)


    /// <summary>Multiple 'returns' result in the entire workflow returning a list results.</summary>
    module AsList =

        type LensBuilder with

            member inline Delay : f: (unit -> ^a) -> ^a
            member inline Run : f: ^f -> ^f

            member inline Combine : Lens< ^p, ^a> * Lens< ^p, ^a> -> Lens< ^p, ^a list>

            member inline Combine : Lens< ^p, ^a> * Lens< ^p, ^a list> -> Lens< ^p, ^a list>