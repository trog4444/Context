namespace Rogz.Context.Base


/// <summary>The Continuation type represents computations in continuation-passing style (CPS).
/// In CPS, a function's result is not returned immediately, but instead is passed to another function, received as a parameter (continuation).
/// Computations are built up from sequences of nested continuations, terminated by a final continuation which produces the final result.</summary>
[<Struct; NoComparison; NoEquality>]
type Cont<'R, 'T> = Cont of (('T -> 'R) -> 'R)
with

// Function

    /// <summary>The result of running a CPS computation with a given final continuation.</summary>
    member inline Invoke: k: System.Func< ^T, ^R> -> ^R

    /// <summary>Create a new context using 'System.Func' objects for .NET interop.</summary>
    static member Of: func: System.Func<System.Func<'T, 'R>, 'R> -> Cont<'R, 'T>


/// <summary>Operations on Continuations.</summary>
module Cont =

// Haskell Primitives

    /// <summary>The result of running a CPS computation with a given final continuation.</summary>
    val runCont: k: ('a -> 'r) -> cont: Cont< ^r, ^a> -> ^r

    /// <summary>The result of running a CPS computation with the identity function as the final continuation.</summary>
    val evalCont: cont: Cont<'r, ^r> -> ^r

    /// <summary>Apply a function to transform the result of a continuation-passing computation.</summary>
    val inline mapCont: f: (^r -> ^r) -> cont: Cont< ^r, ^a> -> Cont< ^r, ^a>

    /// <summary>Apply a function to transform the continuation passed to a CPS computation.</summary>
    val inline withCont: f: ((^b -> ^r) -> ^a -> ^r) -> cont: Cont< ^r, ^a> -> Cont< ^r, ^b>

    /// <summary>'shift f' captures the continuation up to the nearest enclosing 'reset' and passes it to 'f'.</summary>
    val inline shift: f: (( ^a -> ^r) -> Cont< ^r, ^r>) -> Cont< ^r, ^a>

    /// <summary>'reset cont' delimits the continuation of any 'shift' inside 'cont'.</summary>
    val reset: cont: Cont<'r, ^r> -> Cont<'r0, ^r>

    /// <summary>'callCC' (call-with-current-continuation) calls a function with the current continuation as its argument.
    /// This provides an escape continuation mechanism for use with Continuations.
    /// Escape continuations allow controlled escape from the current computation to return a value immediately.</summary>
    val inline callCC: f: ((^a -> Cont< ^r, ^``_``>) -> Cont< ^r, ^a>) -> Cont< ^r, ^a>

    /// <summary>Allows looping with a given continuation function and input.</summary>
    val getCC: x0: 'a -> Cont< 'r, struct ('a * ('a -> Cont<'r, '``_``>))>


// Functor

    /// <summary>Lift a function onto a context.</summary>
    val inline map: f: (^a -> ^b) -> fa: Cont< ^r, ^a> -> Cont< ^r, ^b>


// Applicative

    [<CompiledName("Unit")>]
    /// <summary>Lift a value into a context.</summary>
    val unit: value: 'a -> Cont<'r, ^a>

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    val inline ap: fv: Cont< ^r, ^a> -> ff: Cont< ^r, (^a -> ^b)> -> Cont< ^r, ^b>

    /// <summary>Lift a binary function onto contexts.</summary>
    val inline map2: f: (^a -> ^b -> ^c) -> fa: Cont< ^r, ^a> -> fb: Cont< ^r, ^b> -> Cont< ^r, ^c>

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val sequence: source: #seq<Cont<'r, 'a>> -> Cont< ^r, seq< ^a>>

    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline traverse: f: ( ^a -> Cont< ^r, ^b>) -> source: #seq< ^a> -> Cont< ^r, seq< ^b>>


// Monad

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline bind: f: (^a -> Cont< ^r, ^b>) -> m: Cont< ^r, ^a> -> Cont< ^r, ^b>

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    val flatten: mm: Cont<'r, Cont< ^r, 'a>> -> Cont< ^r, ^a>

    /// <summary>Recursively generate a monadic context using up to two continuation functions to produce different effects.</summary>
    val inline fixM:
        loop: ((^a -> Cont< ^r, ^b>) -> (Cont< ^r, ^a> -> Cont< ^r, ^b>) -> ^a -> Cont< ^r, ^b>) ->
        em: Choice< ^a, Cont< ^r, ^a>> -> Cont< ^r, ^b>


    /// <summary>Computation expression / monadic-workflow type and operations for the given context.</summary>
    [<RequireQualifiedAccess>]
    module Workflow =

        /// <summary>Computation expression for the given monadic context.</summary>
        type ContBuilder =
            new: unit -> ContBuilder
            member Return: x: 'a -> Cont<'r, ^a>
            member ReturnFrom: m: Cont<'r, 'a> -> Cont<'r, 'a>
            member Zero: unit -> Cont<'r, unit>
            member inline Bind: m: Cont< ^r, ^a> * f: (^a -> Cont< ^r, ^b>) -> Cont< ^r, ^b>

    /// <summary>Computation expression instance for the given context.</summary>
    val cont: Workflow.ContBuilder