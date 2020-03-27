namespace Rogz.Context.Data.Either


/// <summary>Operations on Eithers.</summary>
module Either =

// Primitives

    /// <summary>Acts as an inline pattern-match on a union-type, calling the appropriate function based on the case.</summary>
    val inline caseof: onLeft: (^l -> ^a) -> onRight: (^r -> ^a) -> either: Either< ^l, ^r> -> ^a

    /// <summary>Returns true if the value is a 'Left'; false otherwise.</summary>    
    val isLeft: either: Either<'l, 'r> -> bool

    /// <summary>Returns true if the value is a 'Right'; false otherwise.</summary>
    val isRight: either: Either<'l, 'r> -> bool

    /// <summary>Returns all values held within 'Right'-values, and removes all others.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the input sequence is null.</exception>
    val rights: source: Either<'l, 'r> seq -> ^r seq

    /// <summary>Returns all values held within 'Left'-values, and removes all others.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the input sequence is null.</exception>
    val lefts: source: Either<'l, 'r> seq -> ^l seq

    /// <summary>Partitions a sequence of Eithers into a 'Left'-sequence and a 'Right'-sequence.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the input sequence is null.</exception>
    val partition: source: Either<'l, 'r> seq -> struct (^l seq * ^r seq)


//// Isomorphisms

//    /// <summary>Convert a Right-value into a singleton sequence with that value, otherwise return an empty sequence.</summary>
//    val toSeq: either: Either<'e, 'a> -> ^a seq

//    /// <summary>Convert an Either into a Choice(Of2).</summary>
//    val toChoice: either: Either<'e, 'a> -> Choice< ^a, ^e>

//    /// <summary>Convert a Choice into an Either.</summary>
//    val ofChoice: choice: Choice<'a, 'e> -> Either< ^e, ^a>

//    /// <summary>Convert an Either into a Result.</summary>
//    val toResult: either: Either<'e, 'a> -> Result< ^a, ^e>

//    /// <summary>Convert a Result into an Either.</summary>
//    val ofResult: result: Result<'a, 'e> -> Either< ^e, ^a>


//// Recur

//    /// <summary>Perform a context-based loop using the given seed until some 'exit' condition is reached, and return the final result.</summary>
//    val inline recur: loop: (^a -> Either< ^a, ^b>) -> seed: ^a -> ^b


// Functor

    /// <summary>Lift a function onto a context.</summary>
    val inline map: f: (^a -> ^b) -> fa: Either< ^e, ^a> -> Either< ^e, ^b>


// Bifunctor

    /// <summary>Map over both arguments covariantly.</summary>
    val inline bimap: f: (^a -> ^c) -> g: (^b -> ^d) -> bf: Either< ^a, ^b> -> Either< ^c, ^d>

    /// <summary>Map over the first value, leaving the second value as-is.</summary>
    val inline mapFirst: f: (^a -> ^c) -> bf: Either< ^a, ^b> -> Either< ^c, ^b>


// Applicative

    /// <summary>Lift a value into a context.</summary>
    val unit: value: 'a -> Either<'e, ^a>

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    val inline ap: fv: Either< ^e, ^a> -> ff: Either< ^e, (^a -> ^b)> -> Either< ^e, ^b>

    /// <summary>Lift a binary function onto contexts.</summary>
    val inline map2: f: (^a -> ^b -> ^c) -> fa: Either< ^e, ^a> -> fb: Either< ^e, ^b> -> Either< ^e, ^c>

    ///// <summary>Sequence two contexts, discarding the results of the first.</summary>
    //val andthen: fb: Either<'e, 'b> -> fa: Either< ^e, 'a> -> Either< ^e, ^b>

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val sequence: source: #seq<Either<'e, 'a>> -> Either< ^e, ^a seq>

    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline traverse: f: (^a -> Either< ^e, ^b>) -> source: #seq< ^a> -> Either< ^e, ^b seq>


// Alternative

    /// <summary>A monoidal, associative binary operation representing choice/failure.</summary>
    val orElse: second: Either<'e, 'a> -> first: Either< ^e, ^a> -> Either< ^e, ^a>

    /// <summary>A monoidal, associative binary operation representing choice/failure.</summary>
    val inline orElseWith: second: (unit -> Either< ^e, ^a>) -> first: Either< ^e, ^a> -> Either< ^e, ^a>


// Monad

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline bind: f: (^a -> Either< ^e, ^b>) -> ma: Either< ^e, ^a> -> Either< ^e, ^b>
    
    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    val flatten: mm: Either<'e, Either< ^e, 'a>> -> Either< ^e, ^a>
    
    /// <summary>Recursively generate a monadic context using up to two continuation functions to produce different effects.</summary>
    val inline fixM:
        loop: ((^a -> Either< ^e, ^b>) -> (Either< ^e, ^a> -> Either< ^e, ^b>) -> ^a -> Either< ^e, ^b>) ->
        em: Choice< ^a, Either< ^e, ^a>> -> Either< ^e, ^b>

    // foldlM
    // foldrM
   

    /// <summary>Computation expression / monadic-workflow type and operations for the given context.</summary>
    [<RequireQualifiedAccess>]
    module Workflow =
    
        /// <summary>Computation expression for the given monadic context.</summary>
        type EitherBuilder =
            new: unit -> EitherBuilder
            member Return: x: 'a -> Either<'e, ^a>
            member ReturnFrom: m: Either<'e, 'a> -> Either< ^e, ^a>
            member inline Bind: m: Either< ^e, ^a> * f: (^a -> Either< ^e, ^b>) -> Either< ^e, ^b>
            member Zero: unit -> Either<'e, unit>
            //member inline Using: disp: ^d * f: (^d -> Either< ^e, ^a>) -> Either< ^e, ^a> when ^d :> System.IDisposable
            //member inline TryWith: m: Either< ^e, ^a> * handler: (exn -> Either< ^e, ^a>) -> Either< ^e, ^a>
            //member inline TryFinally: m: Either< ^e, ^a> * finalizer: (unit -> unit) -> Either< ^e, ^a>
            //member Using: disp: 'd * f: ('d -> Either<'e, 'a>) -> Either<'e, 'a> when 'd :> System.IDisposable
            //abstract TryWith: m: Either<'e, 'a> * h: (exn -> Either<'e, 'a>) -> Either<'e, 'a>
            //abstract TryFinally: m: Either<'e, 'a> * f: (unit -> unit) -> Either<'e, 'a>

    
    /// <summary>Computation expression instance for the given context.</summary>
    val either: Workflow.EitherBuilder


// Semigroup

    /// <summary>An associative binary operation on contexts.</summary>
    val inline append: first: Either< ^e, ^a> -> second: Either< ^e, ^a> -> Either< ^e, ^a>
        when ^a : (static member Append: ^a -> ^a -> ^a)


// Foldable

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    val inline fold: folder: (^s -> ^a -> ^s) -> seed: ^s -> ta: Either< ^e, ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    val inline foldBack: folder: (^a -> ^s -> ^s) -> seed: ^s -> ta: Either< ^e, ^a> -> ^s

    ///// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation. The accumulator is a thunk that is only called as needed by the folding function.</summary>
    //val inline foldl: folder: ((unit -> ^s) -> ^a -> ^s) -> seed: (unit -> ^s) -> ta: Either< ^e, ^a> -> ^s

    ///// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation. The accumulator is a thunk that is only called as needed by the folding function.</summary>
    //val inline foldr: folder: (^a -> (unit -> ^s) -> ^s) -> seed: (unit -> ^s) -> ta: Either< ^e, ^a> -> ^s

    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
    val inline mapFold: mapping: (^s -> ^a -> struct(^b * ^s)) -> seed: ^s -> ta: Either< ^e, ^a> -> struct (Either< ^e, ^b> * ^s)

    /// <summary>Combines the functionality of map and foldBack, returning the pair of the final context-value and state.</summary>
    val inline mapFoldBack: mapping: (^a -> ^s -> struct(^b * ^s)) -> seed: ^s -> ta: Either< ^e, ^a> -> struct (Either< ^e, ^b> * ^s)    


// Bifoldable

    /// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s).</summary>
    val inline bifold: fold1: (^s -> ^a -> ^s) -> fold2: (^s -> ^b -> ^s) -> seed: ^s -> t: Either< ^a, ^b> -> ^s

    /// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s).</summary>
    val inline bifoldBack: fold1: (^a -> ^s -> ^s) -> fold2: (^b -> ^s -> ^s) -> seed: ^s -> t: Either< ^a, ^b> -> ^s

    ///// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s). The accumulator is a thunk that is only called as needed by the folding function.</summary>
    //val inline bifoldl: fold1: ((unit -> ^s) -> ^a -> ^s) -> fold2: ((unit -> ^s) -> ^b -> ^s) -> seed: (unit -> ^s) -> t: Either< ^a, ^b> -> ^s

    ///// <summary>Applies a function to all element(s) of up to two possible sources, threading an accumulator argument through the computation(s). The accumulator is a thunk that is only called as needed by the folding function.</summary>
    //val inline bifoldr: fold1: (^a -> (unit -> ^s) -> ^s) -> fold2: (^b -> (unit -> ^s) -> ^s) -> seed: (unit -> ^s) -> t: Either< ^a, ^b> -> ^s

    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
    val inline bimapFold: mapping1: (^s -> ^a -> struct (^b * ^s)) -> mapping2: (^s -> ^c -> struct (^d * ^s)) -> seed: ^s -> t: Either< ^a, ^c> -> struct (Either< ^b, ^d> * ^s)

    /// <summary>Combines the functionality of map and foldBack, returning the pair of the final context-value and state.</summary>
    val inline bimapFoldBack: mapping1: (^a -> ^s -> struct (^b * ^s)) -> mapping2: (^c -> ^s -> struct (^d * ^s)) -> seed: ^s -> t: Either< ^a, ^c> -> struct (Either< ^b, ^d> * ^s)