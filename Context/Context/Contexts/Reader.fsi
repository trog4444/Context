namespace Rogz.Context.Data.Reader


/// <summary>Operations on Readers.</summary>
module Reader =

// Interop

    /// <summary>Create a Reader from the given function.</summary>
    val inline fromFunc: f: System.Func< ^e, ^a> -> Reader< ^e, ^a>


// Minimal

    /// <summary>Retreive the current environment.</summary>
    val ask<'e> : Reader< ^e, ^e>

    /// <summary>Executes a computation in a modified environment.</summary>
    val inline local: localize: (^e -> ^e) -> reader: Reader< ^e, ^a> -> Reader< ^e, ^a>


// Primitives

    /// <summary>Execute the given function with the supplied environment.</summary>
    val inline runReader: env: ^e -> reader: Reader< ^e, ^a> -> ^a

    /// <summary>Flip a function then wrap it inside of a Reader.</summary>
    val inline flip: f: (^a -> ^e-> ^r) -> Reader< ^e, ^a -> ^r>

    /// <summary>Caches the result(s) of a computation.</summary>
    val inline cache: reader: Reader< ^e, ^a> -> Reader< ^e, ^a> when ^e: equality


// Functor

    /// <summary>Lift a function onto a context.</summary>
    val inline map: f: (^a -> ^b) -> fa: Reader< ^e, ^a> -> Reader< ^e, ^b>


// Profunctor

    /// <summary>Map over both arguments at the same time, the first (i.e. 'left') contravariantly and the second (i.e. 'right') covariantly.</summary>
    val inline dimap: f: (^c -> ^a) -> g: (^b -> ^d) -> pf: Reader< ^a, ^b> -> Reader< ^c, ^d>

    /// <summary>Map the first (i.e. 'left') argument contravariantly.</summary>
    val inline mapl: f: (^c -> ^a) -> pf: Reader< ^a, ^b> -> Reader< ^c, ^b>


// Applicative

    /// <summary>Lift a value into a context.</summary>
    val unit: value: 'a -> Reader<'e, ^a>

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    val inline ap: fv: Reader< ^e, ^a> -> ff: Reader< ^e, (^a -> ^b)> -> Reader< ^e, ^b>

    /// <summary>Lift a binary function onto contexts.</summary>
    val inline map2: f: (^a -> ^b -> ^c) -> fa: Reader< ^e, ^a> -> fb: Reader< ^e, ^b> -> Reader< ^e, ^c>

    /// <summary>Sequence two contexts, discarding the results of the first.</summary>
    val inline andthen: second: Reader< ^e, ^b> -> first: Reader< ^e, ^a> -> Reader< ^e, ^b>

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline sequence: source: seq<Reader< ^e, ^a>> -> Reader< ^e, seq< ^a>>
    
    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline traverse: f: (^a -> Reader< ^e, ^b>) -> source: seq< ^a> -> Reader< ^e, seq< ^b>>


// Monad

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline bind: f: (^a -> Reader< ^e, ^b>) -> ma: Reader< ^e, ^a> -> Reader< ^e, ^b>

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    val inline flatten: mm: Reader< ^e, Reader< ^e, ^a>> -> Reader< ^e, ^a>

    /// <summary>Recursively generate a monadic context using up to two continuation functions to produce different effects.</summary>
    val inline fixM:
        loop: ((^a -> Reader< ^e, ^b>) -> (Reader< ^e, ^a> -> Reader< ^e, ^b>) -> ^a -> Reader< ^e, ^b>) ->
        em: Rogz.Context.Data.Either.Either< ^a, Reader< ^e, ^a>> -> Reader< ^e, ^b>

    // foldlM
    // foldrM

    /// <summary>Computation expression / monadic-workflow type and operations for the given context.</summary>
    [<RequireQualifiedAccess>]
    module Workflow =

        /// <summary>Computation expression for the given monadic context.</summary>
        type ReaderBuilder =
            new: unit -> ReaderBuilder
            member inline Return: x: ^a -> Reader< ^e, ^a>
            member inline ReturnFrom: m: Reader< ^e, ^a> -> Reader< ^e, ^a>
            member inline Bind: m: Reader< ^e, ^a> * f: (^a -> Reader< ^e, ^b>) -> Reader< ^e, ^b>
            member inline Zero: unit -> Reader< ^e, unit>
            member inline Using: disp: ^d * f: (^d -> Reader< ^e, ^a>) -> Reader< ^e, ^a> when ^d :> System.IDisposable
            member inline TryWith: m: Reader< ^e, ^a> * handler: (exn -> Reader< ^e, ^a>) -> Reader< ^e, ^a>
            member inline TryFinally: m: Reader< ^e, ^a> * finalizer: (unit -> unit) -> Reader< ^e, ^a>


    /// <summary>Computation expression instance for the given context.</summary>
    val reader: Workflow.ReaderBuilder


// Semigroup

    /// <summary>An associative binary operation on contexts.</summary>
    val inline append: first: Reader< ^e, ^a> -> second: Reader< ^e, ^a> -> Reader< ^e, ^a>
        when ^a : (static member Append: ^a -> ^a -> ^a)


// Cat

    /// <summary>Identity element of a category.</summary>
    val identity<'a> : Reader< ^a, ^a>

    /// <summary>Compose two members of a category together.</summary>
    val inline compose: o2: Reader< ^b, ^c> -> o1: Reader< ^a, ^b> -> Reader< ^a, ^c>


// Arrow

    /// <summary>Lift a function to an arrow.</summary>
    val inline arr: f: (^a -> ^b) -> Reader< ^a, ^b>

    /// <summary>Send the first component of the input through the argument arrow, and copy the rest unchanged to the output.</summary>
    val inline first: ar: Reader< ^a, ^b> -> Reader< ^a * ^c, ^b * ^c>

    /// <summary>Send the second component of the input through the argument arrow, and copy the rest unchanged to the output.</summary>
    val inline second: ar: Reader< ^a, ^b> -> Reader< ^c * ^a, ^c * ^b>

    /// <summary>Split the input between the two argument arrows and combine their output.</summary>
    val inline split: a2: Reader< ^c, ^d> -> a1: Reader< ^a, ^b> -> Reader< ^a * ^c, ^b * ^d>

    /// <summary>Fanout: send the input to both argument arrows and combine their output.</summary>
    val inline fanout: a2: Reader< ^a, ^c> -> a1: Reader< ^a, ^b> -> Reader< ^a, ^b * ^c>


// Arrow.Choice

    open Rogz.Context.Data.Either

    /// <summary>Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. A mirror of 'feed2'.</summary>
    val inline feedl: ar: Reader< ^a, ^b> -> Reader<Either< ^a, ^c>, Either< ^b, ^c>>

    /// <summary>Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. A mirror of 'feed1'.</summary>
    val inline feedr: ar: Reader< ^a, ^b> -> Reader<Either< ^c, ^a>, Either< ^c, ^b>>

    /// <summary>Split the input between the two argument arrows, retagging and merging their outputs.</summary>
    val inline merge: a2: Reader< ^c, ^d> -> a1: Reader< ^a, ^b> -> Reader<Either< ^a, ^c>, Either< ^b, ^d>>

    /// <summary>Split the input between the two argument arrows and merge their outputs.</summary>
    val inline fanin: a2: Reader< ^c, ^b> -> a1: Reader< ^a, ^b> -> Reader<Either< ^a, ^c>, ^b>