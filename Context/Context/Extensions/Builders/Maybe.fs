namespace PTR.Context.Extension.Builder.Maybe

open PTR.Context.Type
open Maybe


/// Adds default implementations of the `Combine` method to the specified workflow builder.
///
/// NOTE: Only 'ONE' `Combine` module should be opened at a time in any scope.
[<RequireQualifiedAccess>]
module Combine =

    /// The first `return` that results in a `Just`-value is returned if at least one `Just` exists, Nothing otherwise.
    module FirstJust =

        type Compose.Monad.MaybeBuilder with

            member inline s.Delay f = f ()
            member inline s.Run f = f

            member inline s.Combine(a, b) = if isJust a then a else b


    /// Multiple `returns` result in all of the 'Just' results being `appended` together.
    module Append =

        type Compose.Monad.MaybeBuilder with

            member inline s.Delay f = f ()
            member inline s.Run f = f

            member inline s.Combine(a, b) =
                Compose.Applicative.map2
                    (fun a b -> (^a: (static member Append: ^a -> ^a -> ^a) (a, b))) a b


    /// Multiple `returns` result in the entire workflow returning a list of 'Just' results.
    module AsList =

        type Compose.Monad.MaybeBuilder with

            member inline s.Delay f = f ()
            member inline s.Run f = f

            member inline s.Combine(a, b) =
                Compose.Applicative.map2 (fun a b -> [a; b]) a b

            member inline s.Combine(a, b) =
                Compose.Applicative.map2 (fun a b -> a::b) a b