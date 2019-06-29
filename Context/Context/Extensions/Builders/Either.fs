namespace Ptr.Context.Extension.Builder.Either

open Ptr.Context.Type.Either


/// Adds default implementations of the `Combine` method to the specified workflow builder.
///
/// NOTE: Only 'ONE' `Combine` module should be opened at a time in any scope.
[<RequireQualifiedAccess>]
module Combine =

    /// Returns the first `Right`-value if at least one `Right` exists, else the last `Left`-value.
    module FirstRight =

        type Composition.Monad.EitherBuilder with
            member inline s.Combine(a, b) = if Std.isRight a then a else b


    /// Returns the last `Right`-value if at least one `Right` exists, else the first `Left`-value.
    module LastRight =

        type Composition.Monad.EitherBuilder with
            member inline s.Combine(a, b) = if Std.isLeft b then a else b


    /// Multiple `returns` result in all of the results being `appended` together.
    module Append =

        type Composition.Monad.EitherBuilder with

            member inline s.Combine(a, b) =
                match a, b with
                | Left e, _ -> Left e
                | _, Left e -> Left e
                | Right a, Right b -> Right (^a: (static member Append: ^a -> ^a -> ^a) (a, b))


    /// Multiple `returns` result in the entire workflow returning a list of results.
    module AsList =
        
        type Composition.Monad.EitherBuilder with

            member inline s.Combine(a, b) =
                match a, b with
                | Left e, _ -> Left e
                | _, Left e -> Left e
                | Right a, Right b -> Right [a; b]

            member inline s.Combine(a, b) =
                match a, b with
                | Left e, _ -> Left e
                | _, Left e -> Left e
                | Right a, Right bs -> Right (a :: bs)