namespace Rogz.Context.Base


[<Struct>]
type Either<'L, 'R> = Left of left: 'L | Right of right: 'R
with

// Union Type

    /// <summary>Tries to return the specified value. A return value indicates whether the operation succeeded.</summary>
    member TryLeft: [<System.Runtime.InteropServices.Out>] value: outref<'L> -> bool

    /// <summary>Tries to return the specified value. A return value indicates whether the operation succeeded.</summary>
    member TryRight: [<System.Runtime.InteropServices.Out>] value: outref<'R> -> bool

    /// <summary>Acts as a pattern-match on a union-type, calling the appropriate function based on the case.</summary>
    member Match: ifLeft: System.Func<'L, 'T> * ifRight: System.Func<'R, 'T> -> 'T

    /// <summary>Acts as a pattern-match on a union-type, calling the appropriate function based on the case.</summary>
    member Match: ifLeft: System.Action<'L> * ifRight: System.Action<'R> -> unit

//// Alternative

//    [<CompiledName("OrElse")>]
//    static member ( <|> ): first: Either<'A,'B> * second: Either<'A,'B> -> Either<'A,'B>

// Semigroup

    //[<CompiledName("Append")>]
    /// <summary>An associative binary operation on monoidal types.</summary>
    static member inline ( + ): first: Either< ^A, ^B> * second: Either< ^A, ^B> -> Either< ^A, ^B>
        when ^B: (static member ( + ): ^B -> ^B -> ^B)


/// <summary>Operations on Eithers.</summary>
module Either =

// Haskell Primitives
    
    /// <summary>Acts as a pattern-match on a union-type, calling the appropriate function based on the case.</summary>
    val inline caseof: left: (^a -> ^c) -> right: (^b -> ^c) -> either: Either< ^a, ^b> -> ^c

    /// <summary>Returns all values held within 'Left'-values, and removes all others.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the input sequence is null.</exception>
    val lefts: source: Either<'l, 'r> seq -> ^l seq

    /// <summary>Returns all values held within 'Right'-values, and removes all others.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the input sequence is null.</exception>
    val rights: source: Either<'l, 'r> seq -> ^r seq

    /// <summary>Returns true if the value is a 'Left'; false otherwise.</summary>    
    val isLeft: either: Either<'l, 'r> -> bool

    /// <summary>Returns true if the value is a 'Right'; false otherwise.</summary>
    val isRight: either: Either<'l, 'r> -> bool

    /// <summary>Return the contents of a Left-value or a default value otherwise.</summary>
    val fromLeft: defaultValue: 'e -> either: Either<'e,'a> -> 'e

    /// <summary>Return the contents of a Right-value or a default value otherwise.</summary>
    val fromRight: defaultValue: 'a -> either: Either<'e,'a> -> 'a

    /// <summary>Partitions a sequence of Eithers into a 'Left'-sequence and a 'Right'-sequence.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the input sequence is null.</exception>
    val partition: source: Either<'l, 'r> seq -> struct (^l seq * ^r seq)


// Functor

    /// <summary>Lift a function onto a context.</summary>
    val inline map: f: (^a -> ^b) -> fa: Either< ^e, ^a> -> Either< ^e, ^b>


// Bifunctor

    /// <summary>Map over both arguments covariantly.</summary>
    val inline bimap: f: (^a -> ^c) -> g: (^b -> ^d) -> bf: Either< ^a, ^b> -> Either< ^c, ^d>

    /// <summary>Map over the first value, leaving the second value as-is.</summary>
    val inline mapFirst: f: (^a -> ^c) -> bf: Either< ^a, ^b> -> Either< ^c, ^b>


// Applicative

    [<CompiledName("Unit")>]
    /// <summary>Lift a value into a context.</summary>
    val unit: value: 'a -> Either<'e, ^a>

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    val inline ap: fv: Either< ^e, ^a> -> ff: Either< ^e, (^a -> ^b)> -> Either< ^e, ^b>

    /// <summary>Lift a binary function onto contexts.</summary>
    val inline map2: f: (^a -> ^b -> ^c) -> fa: Either< ^e, ^a> -> fb: Either< ^e, ^b> -> Either< ^e, ^c>

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val sequence: source: #seq<Either<'e, 'a>> -> Either< ^e, ^a seq>

    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline traverse: f: (^a -> Either< ^e, ^b>) -> source: #seq< ^a> -> Either< ^e, ^b seq>


// Alternative

    /// <summary>An associative operation representing a decision between two structures.</summary>
    val orElse: second: Either<'e, 'a> -> first: Either< ^e, ^a> -> Either< ^e, ^a>

    /// <summary>An associative operation representing a decision between two structures.</summary>
    val orElseWith: second: (unit -> Either<'e, 'a>) -> first: Either< ^e, ^a> -> Either< ^e, ^a>


// Monad

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline bind: f: (^a -> Either< ^e, ^b>) -> m: Either< ^e, ^a> -> Either< ^e, ^b>
    
    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    val flatten: mm: Either<'e, Either< ^e, 'a>> -> Either< ^e, ^a>
    
    /// <summary>Recursively generate a monadic context using up to two continuation functions to produce different effects.</summary>
    val inline fixM:
        loop: ((^a -> Either< ^e, ^b>) -> (Either< ^e, ^a> -> Either< ^e, ^b>) -> ^a -> Either< ^e, ^b>) ->
        em: Choice< ^a, Either< ^e, ^a>> -> Either< ^e, ^b>


    /// <summary>Computation expression / monadic-workflow type and operations for the given context.</summary>
    [<RequireQualifiedAccess>]
    module Workflow =
    
        /// <summary>Computation expression for the given monadic context.</summary>
        type EitherBuilder =
            new: unit -> EitherBuilder
            member Return: x: 'a -> Either<'e, ^a>
            member ReturnFrom: m: Either<'e, 'a> -> Either< ^e, ^a>
            member Zero: unit -> Either<'e, unit>
            member inline Bind: m: Either< ^e, ^a> * f: (^a -> Either< ^e, ^b>) -> Either< ^e, ^b>

    /// <summary>Computation expression instance for the given context.</summary>
    val either: Workflow.EitherBuilder


// Semigroup

    /// <summary>An associative binary operation on monoidal types.</summary>
    val inline append: second: Either< ^a, ^b> -> first: Either< ^a, ^b> -> Either< ^a, ^b>
        when ^b: (static member ( + ): ^b -> ^b -> ^b)


// Foldable

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    val inline fold: folder: (^s -> ^a -> ^s) -> seed: ^s -> source: Either< ^e, ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    val inline foldBack: folder: (^a -> ^s -> ^s) -> seed: ^s -> source: Either< ^e, ^a> -> ^s

    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
    val inline mapFold: mapping: (^s -> ^a -> struct(^b * ^s)) -> seed: ^s -> source: Either< ^e, ^a> -> struct (Either< ^e, ^b> * ^s)

    /// <summary>Combines the functionality of map and foldBack, returning the pair of the final context-value and state.</summary>
    val inline mapFoldBack: mapping: (^a -> ^s -> struct(^b * ^s)) -> seed: ^s -> source: Either< ^e, ^a> -> struct (Either< ^e, ^b> * ^s)    


// Bifoldable

    /// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s).</summary>
    val inline bifold: fold1: (^s -> ^a -> ^s) -> fold2: (^s -> ^b -> ^s) -> seed: ^s -> source: Either< ^a, ^b> -> ^s

    /// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s).</summary>
    val inline bifoldBack: fold1: (^a -> ^s -> ^s) -> fold2: (^b -> ^s -> ^s) -> seed: ^s -> source: Either< ^a, ^b> -> ^s

    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
    val inline bimapFold: mapping1: (^s -> ^a -> struct (^b * ^s)) -> mapping2: (^s -> ^c -> struct (^d * ^s)) -> seed: ^s -> source: Either< ^a, ^c> -> struct (Either< ^b, ^d> * ^s)

    /// <summary>Combines the functionality of map and foldBack, returning the pair of the final context-value and state.</summary>
    val inline bimapFoldBack: mapping1: (^a -> ^s -> struct (^b * ^s)) -> mapping2: (^c -> ^s -> struct (^d * ^s)) -> seed: ^s -> source: Either< ^a, ^c> -> struct (Either< ^b, ^d> * ^s)