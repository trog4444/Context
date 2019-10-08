namespace PTR.Context.Builder//.Maybe

////open PTR.Context.Type.Maybe
////open PTR.Context.Type.Maybe.Maybe
////open PTR.Context.Type.Maybe.Maybe.Workflow


/////// <summary>Customized builders/workflows for a given context.</summary>
////module Build =

////    /// <summary>Customized builders/workflows for a given context.</summary>
////    module Workflow =

////        /// <summary>Generic workflow with minimum added methods to use the 'Combine' method.</summary>
////        [<AbstractClass>]
////        type MaybeRunnable =
////            new : unit -> MaybeRunnable
////            inherit MaybeBuilder
////            member inline Delay : f: (unit -> ^a) -> ^a
////            member inline Run : f: ^f -> ^f
////            member inline Yield : x: ^a -> Maybe< ^a>
////            member inline YieldFrom : m: Maybe< ^a> -> Maybe< ^a>


////        /// <summary>Builder where the 'Combine' method uses the 'append' function from the base context.</summary>
////        [<Sealed>]
////        type MaybeAppend =
////            new : unit -> MaybeAppend
////            inherit MaybeRunnable
////            member inline Combine : a: Maybe< ^a> * b: Maybe< ^a> -> Maybe< ^a>
////                when ^a : (static member Append: ^a -> ^a -> ^a)


////        /// <summary>Builder where the 'Combine' method allows for multiple results to be 'yielded' as a list.</summary>
////        [<Sealed>]
////        type MaybeList =
////            new : unit -> MaybeList
////            inherit MaybeRunnable
////            member inline Combine : a: Maybe< ^a> * b: Maybe< ^a> -> Maybe< ^a list>
////            member inline Combine : a: Maybe< ^a list> * bs: Maybe< ^a list> -> Maybe< ^a list>
////            //member inline For : seq: #seq< ^a> * f: (^a -> Maybe< ^b>) -> Maybe< ^b list>


////        /// <summary>Builder where the 'Combine' method that returns the first 'Just'-value.</summary>
////        [<Sealed>]
////        type MaybeFirstJust =
////            new : unit -> MaybeFirstJust
////            inherit MaybeRunnable
////            member inline Combine : a: Maybe< ^a> * b: Maybe< ^a> -> Maybe< ^a>


////        /// <summary>Builder where the 'Combine' method that returns the last 'Just'-value.</summary>
////        [<Sealed>]
////        type MaybeLastJust =
////            new : unit -> MaybeLastJust
////            inherit MaybeRunnable
////            member inline Combine : a: Maybe< ^a> * b: Maybe< ^a> -> Maybe< ^a>



////    open Workflow

////    /// <summary>Builder where the 'Combine' method uses the 'append' function from the base context.</summary>
////    val maybeAppend : MaybeAppend

////    /// <summary>Builder where the 'Combine' method allows for multiple results to be 'yielded' as a list.</summary>
////    val maybeList : MaybeList

////    /// <summary>Builder where the 'Combine' method that returns the first 'Just'-value.</summary>
////    val maybeFirstJust : MaybeFirstJust

////    /// <summary>Builder where the 'Combine' method that returns the last 'Just'-value.</summary>
////    val maybeLastJust : MaybeLastJust