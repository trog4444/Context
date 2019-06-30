namespace Ptr.Context.Extension.Builder.Maybe

open Ptr.Context.Type
open Maybe


/// Adds default implementations of the `Combine` method to the specified workflow builder.
///
/// NOTE: Only 'ONE' `Combine` module should be opened at a time in any scope.
[<RequireQualifiedAccess>]
module Combine =

    /// The first `return` that results in a `Just`-value is returned if at least one `Just` exists, Nothing otherwise.
    module FirstJust =

        type Compose.Monad.MaybeBuilder with
            member inline s.Combine(a, b) = if isJust a then a else b


    /// Multiple `returns` result in all of the results being `appended` together.
    module Append =

        type Compose.Monad.MaybeBuilder with

            member inline s.Combine(a, b) =
                Compose.Applicative.map2
                    (fun a b -> (^a: (static member Append: ^a -> ^a -> ^a) (a, b))) a b


    /// Multiple `returns` result in the entire workflow returning a list of results.
    module AsList =

        type Compose.Monad.MaybeBuilder with
            member inline s.Combine(a, b) =
                match a with
                | Nothing -> match b with
                             | Nothing -> Nothing
                             | Just b  -> Just [b]
                | Just a  -> match b with
                             | Nothing -> Just [a]
                             | Just b  -> Just [a; b]

            member inline s.Combine(a, b) =
                match a with
                | Nothing -> match b with
                             | Nothing -> Nothing
                             | Just bs  -> Just bs
                | Just a  -> match b with
                             | Nothing -> Just [a]
                             | Just bs  -> Just (a::bs)