namespace Rogz.Context.Data.Cont


/// <summary>Operations on Continuations.</summary>
module Cont =

// Interop

    /// <summary>Create a Continuation from the given function.</summary>
    [<CompiledName("Make")>]
    val inline make: cont: System.Func<System.Func< ^a, ^r>, ^r> -> Cont< ^r, ^a>


// Minimal
    
    /// <summary>'callCC' (call-with-current-continuation) calls a function with the current continuation as its argument.
    /// This provides an escape continuation mechanism for use with Continuations.
    /// Escape continuations allow controlled escape from the current computation to return a value immediately.</summary>
    val inline callCC: f: ((^a -> Cont< ^r, ^``_``>) -> Cont< ^r, ^a>) -> Cont< ^r, ^a>

    /// <summary>'shift f' captures the continuation up to the nearest enclosing 'reset' and passes it to 'f'.</summary>
    val inline shift: f: ((^a -> ^r) -> Cont< ^r, ^r>) -> Cont< ^r, ^a>

    /// <summary>'reset cont' delimits the continuation of any 'shift' inside 'cont'.</summary>
    val inline reset: cont: Cont< ^r, ^r> -> Cont< ^r0, ^r>


// Primitives
    
    /// <summary>The result of running a CPS computation with a given final continuation.</summary>
    val inline runCont: k: (^a -> ^r) -> cont: Cont< ^r, ^a> -> ^r
    
    /// <summary>The result of running a CPS computation with the identity function as the final continuation.</summary>
    val inline evalCont: Cont< ^r, ^r> -> ^r

    /// <summary>Apply a function to transform the result of a continuation-passing computation.</summary>
    val inline mapCont: f: (^r -> ^r) -> cont: Cont< ^r, ^a> -> Cont< ^r, ^a>

    /// <summary>Apply a function to transform the continuation passed to a CPS computation.</summary>
    val inline withCont: f: ((^b -> ^r) -> ^a -> ^r) -> cont: Cont< ^r, ^a> -> Cont< ^r, ^b>
    
    /// <summary>Allows looping with a given continuation function and input.</summary>
    val inline getCC: x0: ^a -> Cont< ^r, struct (^a * (^a -> Cont< ^r, ^``_``>))>

    /// <summary>End the current continuation chain with a specific value.</summary>
    val quitCC: x: 'r -> Cont< ^r, '``_``>    

    /// <summary>Caches the result(s) of a Continuation computation.</summary>
    val inline cache: Cont< ^r, ^a> -> Cont< ^r, ^a> when ^a: equality


//// Isomorphisms
    
//    /// <summary>Apply an action (i.e. continuation) to each element of a sequence.</summary>
//    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//    val inline ofSeq: source: ^a seq -> Cont<unit, ^a>


// Functor

    /// <summary>Lift a function onto a context.</summary>
    val inline map: f: (^a -> ^b) -> fa: Cont< ^r, ^a> -> Cont< ^r, ^b>


// Applicative

    /// <summary>Lift a value into a context.</summary>
    val inline unit: value: ^a -> Cont< ^r, ^a>

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    val inline ap: fv: Cont< ^r, ^a> -> ff: Cont< ^r, (^a -> ^b)> -> Cont< ^r, ^b>

    /// <summary>Lift a binary function onto contexts.</summary>
    val inline map2: f: (^a -> ^b -> ^c) -> fa: Cont< ^r, ^a> -> fb: Cont< ^r, ^b> -> Cont< ^r, ^c>

    /// <summary>Sequence two contexts, discarding the results of the first.</summary>
    val inline andthen: second: Cont< ^r, ^b> -> first: Cont< ^r, ^a> -> Cont< ^r, ^b>


// Monad

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline bind: f: (^a -> Cont< ^r, ^b>) -> ma: Cont< ^r, ^a> -> Cont< ^r, ^b>

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    val inline flatten: mm: Cont< ^r, Cont< ^r, ^a>> -> Cont< ^r, ^a>

    /// <summary>Recursively generate a monadic context using up to two continuation functions to produce different effects.</summary>
    val inline fixM:
        loop: ((^a -> Cont< ^r, ^b>) -> (Cont< ^r, ^a> -> Cont< ^r, ^b>) -> ^a -> Cont< ^r, ^b>) ->
        em: Rogz.Context.Data.Either.Either< ^a, Cont< ^r, ^a>> -> Cont< ^r, ^b>

    // foldlM
    // foldrM

    /// <summary>Computation expression / monadic-workflow type and operations for the given context.</summary>
    [<RequireQualifiedAccess>]
    module Workflow =

        /// <summary>Computation expression for the given monadic context.</summary>
        type ContBuilder =
            new: unit -> ContBuilder
            member inline Return: x: ^a -> Cont< ^r, ^a>
            member inline ReturnFrom: m: Cont< ^r, ^a> -> Cont< ^r, ^a>
            member inline Bind: m: Cont< ^r, ^a> * f: (^a -> Cont< ^r, ^b>) -> Cont< ^r, ^b>
            member inline Zero: unit -> Cont< ^r, unit>
            member inline Using: disp: ^d * f: (^d -> Cont< ^r, ^a>) -> Cont< ^r, ^a> when ^d :> System.IDisposable
            member inline TryWith: m: Cont< ^r, ^a> * handler: (exn -> Cont< ^r, ^a>) -> Cont< ^r, ^a>
            member inline TryFinally: m: Cont< ^r, ^a> * finalizer: (unit -> unit) -> Cont< ^r, ^a>


    /// <summary>Computation expression instance for the given context.</summary>
    val cont: Workflow.ContBuilder


// Comonad

    /// <summary>Retrieve a value from a co-context.</summary>
    val inline extract: w: Cont< ^a, ^a> -> ^a

    /// <summary>Sequentially compose two co-contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline extend: f: (Cont< ^r, ^a> -> ^b) -> w: Cont< ^r, ^a> -> Cont< ^r, ^b>

    /// <summary>Adds a layer of co-context onto an existing co-context.</summary>
    val inline duplicate: w: Cont< ^r, ^a> -> Cont< ^r, Cont< ^r, ^a>>


//// Foldable

//    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
//    val inline fold: folder: (^s -> ^a -> ^s) -> seed: ^s -> ta: Cont<unit, ^a> -> ^s

//    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
//    val inline foldBack: folder: (^a -> ^s -> ^s) -> seed: ^s -> ta: Cont<unit, ^a> -> ^s

//    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation. The accumulator is a thunk that is only called as needed by the folding function.</summary>
//    val inline foldl: folder: ((unit -> ^s) -> ^a -> ^s) -> seed: (unit -> ^s) -> ta: Cont<unit, ^a> -> ^s
    
//    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation. The accumulator is a thunk that is only called as needed by the folding function.</summary>
//    val inline foldr: folder: (^a -> (unit -> ^s) -> ^s) -> seed: (unit -> ^s) -> ta: Cont<unit, ^a> -> ^s

//    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
//    val inline mapFold: mapping: (^s -> ^a -> struct (^b * ^s)) -> seed: ^s -> ta: Cont<unit, ^a> -> struct (Cont<unit, ^b> * ^s)

//    /// <summary>Combines the functionality of map and foldBack, returning the pair of the final context-value and state.</summary>
//    val inline mapFoldBack: mapping: (^a -> ^s -> struct (^b * ^s)) -> seed: ^s -> ta: Cont<unit, ^a> -> struct (Cont<unit, ^b> * ^s)

//    /// <summary>The concatenation of all the elements of a container of sequences.</summary>
//    /// <exception cref="System.ArgumentNullException">Thrown when the sequence is null.</exception>
//    val fail here !!!: ta: Cont<unit, ^a seq> -> ^a seq

//    /// <summary>Map a function over all the elements of a container and concatenate the resulting sequences.</summary>
//    /// <exception cref="System.ArgumentNullException">Thrown when the sequence is null.</exception>
//    val fail here !!!Map: f: (^a -> ^b seq) -> ta: Cont<unit, ^a> -> ^b seq


// Traversable

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline sequence: source: seq<Cont< ^r, ^a>> -> Cont< ^r, seq< ^a>>

    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline traverse: f: (^a -> Cont< ^r, ^b>) -> source: seq< ^a> -> Cont< ^r, seq< ^b>>