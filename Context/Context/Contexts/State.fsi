namespace Rogz.Context.Data.State


/// <summary>Operations on States.</summary>
module State =

// Interop

    /// <summary>Create a State from the given function.</summary>
    val inline fromFunc: f: System.Func< ^s, SVPair< ^s, ^a>> -> State< ^s, ^a>


// Minimal

    /// <summary>Return the current state as a value.</summary>
    val get<'s> : State< ^s, ^s>

    /// <summary>Ignore the current state and put a new state into a computation.</summary>
    val put: state: 's -> State< ^s, unit>

    /// <summary>Maps an old state to a new state, discarding the old state.</summary>
    val inline modify: modification: (^s -> ^s) -> State< ^s, unit>

    /// <summary>Gets specific component of the state, using a projection function supplied.</summary>
    val inline gets: proj: (^s -> ^a) -> State< ^s, ^a>


// Primitives

    /// <summary>Execute the given stateful computation.</summary>
    val inline runState: initial: ^s -> state: State< ^s, ^a> -> SVPair< ^s, ^a>

    /// <summary>Evaluate a state computation with the given initial state and return the final value, discarding the final state.</summary>
    val inline evalState: initial: ^s -> state: State< ^s, ^a> -> ^a

    /// <summary>Evaluate a state computation with the given initial state and return the final state, discarding the final value.</summary>
    val inline execState: initial: ^s -> state: State< ^s, ^a> -> ^s

    /// <summary>Map both the return value and final state of a computation using the given function.</summary>
    val inline mapState: mapping: (SVPair< ^s, ^a> -> SVPair< ^s, ^b>) -> state: State< ^s, ^a> -> State< ^s, ^b>

    /// <summary>Executes a stateful computation on a state modified by 'f'.</summary>
    val inline withState: f: (^s -> ^s) -> state: State< ^s, ^a> -> State< ^s, ^a>

    ///// <summary>Caches the result(s) of a stateful computation.</summary>
    //val inline cache: state: State< ^s, ^a> -> State< ^s, ^a> when ^s: equality


// Functor

    /// <summary>Lift a function onto a context.</summary>
    val inline map: f: (^a -> ^b) -> fa: State< ^s, ^a> -> State< ^s, ^b>


// Applicative

    /// <summary>Lift a value into a context.</summary>
    val unit: value: 'a -> State<'s, ^a>

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    val inline ap: fv: State< ^s, ^a> -> ff: State< ^s, (^a -> ^b)> -> State< ^s, ^b>

    /// <summary>Lift a binary function onto contexts.</summary>
    val inline map2: f: (^a -> ^b -> ^c) -> fa: State< ^s, ^a> -> fb: State< ^s, ^b> -> State< ^s, ^c>

    ///// <summary>Sequence two contexts, discarding the results of the first.</summary>
    //val inline andthen: second: State< ^s, ^b> -> first: State< ^s, ^a> -> State< ^s, ^b>

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline sequence: source: seq<State< ^s, ^a>> -> State< ^s, seq< ^a>>

    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline traverse: f: (^a -> State< ^s, ^b>) -> source: seq< ^a> -> State< ^s, seq< ^b>>


// Monad

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline bind: f: (^a -> State< ^s, ^b>) -> ma: State< ^s, ^a> -> State< ^s, ^b>

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    val inline flatten: mm: State< ^s, State< ^s, ^a>> -> State< ^s, ^a>

    /// <summary>Recursively generate a monadic context using up to two continuation functions to produce different effects.</summary>
    val inline fixM:
        loop: ((^a -> State< ^s, ^b>) -> (State< ^s, ^a> -> State< ^s, ^b>) -> ^a -> State< ^s, ^b>) ->
        em: Choice< ^a, State< ^s, ^a>> -> State< ^s, ^b>

    // foldlM
    // foldrM


    /// <summary>Computation expression / monadic-workflow type and operations for the given context.</summary>
    [<RequireQualifiedAccess>]
    module Workflow =

        /// <summary>Computation expression for the given monadic context.</summary>
        type StateBuilder =
            new: unit -> StateBuilder
            member Return: x: 'a -> State<'s, ^a>
            member ReturnFrom: m: State<'s, 'a> -> State< ^s, ^a>
            member inline Bind: m: State< ^s, ^a> * f: (^a -> State< ^s, ^b>) -> State< ^s, ^b>
            member Zero: unit -> State<'s, unit>
            //member inline Using: disp: ^d * f: (^d -> State< ^s, ^a>) -> State< ^s, ^a> when ^d :> System.IDisposable
            //member inline TryWith: m: State< ^s, ^a> * handler: (exn -> State< ^s, ^a>) -> State< ^s, ^a>
            //member inline TryFinally: m: State< ^s, ^a> * finalizer: (unit -> unit) -> State< ^s, ^a>
            member Using: disp: 'd * f: ('d -> State<'s, 'a>) -> State<'s, 'a> when 'd :> System.IDisposable
            //abstract TryWith: m: State<'s, 'a> * h: (exn -> State<'s, 'a>) -> State<'s, 'a>
            //abstract TryFinally: m: State<'s, 'a> * f: (unit -> unit) -> State<'s, 'a>


    /// <summary>Computation expression instance for the given context.</summary>
    val state: Workflow.StateBuilder