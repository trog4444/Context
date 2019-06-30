namespace Ptr.Context.Extension.Builder.RWS

open Ptr.Context.Type.RWS


/// Adds default implementations of the `Combine` method to the specified workflow builder.
///
/// NOTE: Only 'ONE' `Combine` module should be opened at a time in any scope.
[<RequireQualifiedAccess>]
module Combine =

    /// Multiple `returns` result in all of the results being `appended` together.
    module Append =

        type Compose.Monad.RWSBuilder with

            member inline s.Combine(a, b) =
                Compose.Applicative.map2
                    (fun a b -> (^a: (static member Append: ^a -> ^a -> ^a) (a, b))) a b


    /// Multiple `returns` result in the entire workflow returning a list of results.
    module AsList =

        type Compose.Monad.RWSBuilder with

            member inline s.Combine(a, b) =
                Compose.Applicative.map2 (fun a b -> [a; b]) a b

            member inline s.Combine(a, b) =
                Compose.Applicative.map2 (fun a bs -> a::bs) a b
