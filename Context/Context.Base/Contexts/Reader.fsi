namespace Rogz.Context.Base


/// <summary>Represents one or more computations that share an input 'environment'.</summary>
[<Struct; NoComparison; NoEquality>]
type Reader<'Env, 'T> = Reader of ('Env -> 'T)
with

// Function

    /// <summary>The result of running the given computation.</summary>
    member Invoke: env: ^Env -> ^T

    /// <summary>Create a new context using 'System.Func' objects for .NET interop.</summary>
    static member Of: func: System.Func<'Env, 'T> -> Reader<'Env, 'T>

// Semigroup

    //[<CompiledName("Append")>]
    /// <summary>An associative binary operation on monoidal types.</summary>
    static member inline ( + ): first: Reader< ^R, ^A> * second: Reader< ^R, ^A> -> Reader< ^R, ^A>
        when ^A: (static member ( + ): ^A -> ^A -> ^A)


/// <summary>Operations on Readers.</summary>
module Reader =

// Haskell Primitives

    /// <summary>Retreive the current environment.</summary>
    val ask<'e> : Reader< ^e, ^e>

    /// <summary>The selector function to apply to the environment.</summary>
    val asks: func: System.Func<'e,'a> -> Reader<'e,'a>

    /// <summary>Executes a computation in a modified environment.</summary>
    val inline local: localize: (^e -> ^e) -> reader: Reader< ^e, ^a> -> Reader< ^e, ^a>

    /// <summary>Execute the given function with the supplied environment.</summary>
    val runReader: env: 'e -> reader: Reader< ^e, 'a> -> ^a

    // mapReader  = map
    // withReader = mapl


// Functor

    /// <summary>Lift a function onto a context.</summary>
    val inline map: f: (^a -> ^b) -> fa: Reader< ^e, ^a> -> Reader< ^e, ^b>


// Profunctor

    /// <summary>Map over both arguments at the same time, the first (i.e. 'left') contravariantly and the second (i.e. 'right') covariantly.</summary>
    val inline dimap: f: (^c -> ^a) -> g: (^b -> ^d) -> pf: Reader< ^a, ^b> -> Reader< ^c, ^d>

    /// <summary>Map the first (i.e. 'left') argument contravariantly.</summary>
    val inline mapl: f: (^c -> ^a) -> pf: Reader< ^a, ^b> -> Reader< ^c, ^b>


// Applicative

    [<CompiledName("Unit")>]
    /// <summary>Lift a value into a context.</summary>
    val unit: value: 'a -> Reader<'e, ^a>

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    val inline ap: fv: Reader< ^e, ^a> -> ff: Reader< ^e, (^a -> ^b)> -> Reader< ^e, ^b>

    /// <summary>Lift a binary function onto contexts.</summary>
    val inline map2: f: (^a -> ^b -> ^c) -> fa: Reader< ^e, ^a> -> fb: Reader< ^e, ^b> -> Reader< ^e, ^c>

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val sequence: source: #seq<Reader<'e, 'a>> -> Reader< ^e, seq< ^a>>
    
    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline traverse: f: (^a -> Reader< ^e, ^b>) -> source: #seq< ^a> -> Reader< ^e, seq< ^b>>


// Monad

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline bind: f: (^a -> Reader< ^e, ^b>) -> ma: Reader< ^e, ^a> -> Reader< ^e, ^b>

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    val flatten: mm: Reader<'e, Reader< ^e, 'a>> -> Reader< ^e, ^a>

    /// <summary>Recursively generate a monadic context using up to two continuation functions to produce different effects.</summary>
    val inline fixM:
        loop: ((^a -> Reader< ^e, ^b>) -> (Reader< ^e, ^a> -> Reader< ^e, ^b>) -> ^a -> Reader< ^e, ^b>) ->
        em: Choice< ^a, Reader< ^e, ^a>> -> Reader< ^e, ^b>


    /// <summary>Computation expression / monadic-workflow type and operations for the given context.</summary>
    [<RequireQualifiedAccess>]
    module Workflow =

        /// <summary>Computation expression for the given monadic context.</summary>
        type ReaderBuilder =
            new: unit -> ReaderBuilder
            member Return: x: 'a -> Reader<'e, ^a>
            member ReturnFrom: m: Reader<'e, 'a> -> Reader< ^e, ^a>
            member Zero: unit -> Reader<'e, unit>
            member inline Bind: m: Reader< ^e, ^a> * f: (^a -> Reader< ^e, ^b>) -> Reader< ^e, ^b>            

    /// <summary>Computation expression instance for the given context.</summary>
    val reader: Workflow.ReaderBuilder


// Semigroup

    /// <summary>An associative binary operation on monoidal types.</summary>
    val inline append: second: Reader< ^e, ^a> -> first: Reader< ^e, ^a> -> Reader< ^e, ^a>
        when ^a : (static member ( + ): ^a -> ^a -> ^a)


//// Cat

//    /// <summary>Identity element of a category.</summary>
//    val identity<'a> : Reader< ^a, ^a>

//    /// <summary>Compose two members of a category together.</summary>
//    val inline compose: o2: Reader< ^b, ^c> -> o1: Reader< ^a, ^b> -> Reader< ^a, ^c>


//// Arrow

//    [<System.Obsolete("Use Reader constructor instead.")>]
//    /// <summary>Lift a function to an arrow.</summary>
//    val inline arr: f: (^a -> ^b) -> Reader< ^a, ^b>

//    [<System.Obsolete("Use Reader constructor instead.")>]
//    /// <summary>Send the first component of the input through the argument arrow, and copy the rest unchanged to the output.</summary>
//    val inline first: ar: Reader< ^a, ^b> -> Reader< ^a * ^c, ^b * ^c>

//    [<System.Obsolete("Use Reader constructor instead.")>]
//    /// <summary>Send the second component of the input through the argument arrow, and copy the rest unchanged to the output.</summary>
//    val inline second: ar: Reader< ^a, ^b> -> Reader< ^c * ^a, ^c * ^b>

//    [<System.Obsolete("Use Reader constructor instead.")>]
//    /// <summary>Split the input between the two argument arrows and combine their output.</summary>
//    val inline split: a2: Reader< ^c, ^d> -> a1: Reader< ^a, ^b> -> Reader< ^a * ^c, ^b * ^d>

//    [<System.Obsolete("Use Reader constructor instead.")>]
//    /// <summary>Fanout: send the input to both argument arrows and combine their output.</summary>
//    val inline fanout: a2: Reader< ^a, ^c> -> a1: Reader< ^a, ^b> -> Reader< ^a, ^b * ^c>


//// Arrow.Choice

//    open Rogz.Context.Data.Either

//    /// <summary>Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. A mirror of 'feed2'.</summary>
//    val inline feedl: ar: Reader< ^a, ^b> -> Reader<Either< ^a, ^c>, Either< ^b, ^c>>

//    /// <summary>Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. A mirror of 'feed1'.</summary>
//    val inline feedr: ar: Reader< ^a, ^b> -> Reader<Either< ^c, ^a>, Either< ^c, ^b>>

//    /// <summary>Split the input between the two argument arrows, retagging and merging their outputs.</summary>
//    val inline merge: a2: Reader< ^c, ^d> -> a1: Reader< ^a, ^b> -> Reader<Either< ^a, ^c>, Either< ^b, ^d>>

//    /// <summary>Split the input between the two argument arrows and merge their outputs.</summary>
//    val inline fanin: a2: Reader< ^c, ^b> -> a1: Reader< ^a, ^b> -> Reader<Either< ^a, ^c>, ^b>