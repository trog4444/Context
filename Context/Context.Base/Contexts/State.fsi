namespace Rogz.Context.Base


/// <summary>Represents a 'stateful'-computation, threading each new state as an argument in a resultant state-value pair.</summary>
[<Struct; NoComparison; NoEquality>]
type State<'S, 'T> = State of ('S -> struct('S * 'T))
with

// Function

    /// <summary>Execute the given stateful computation.</summary>
    member Invoke: state: ^S -> struct(^S * ^T)

    /// <summary>Create a new context using 'System.Func' objects for .NET interop.</summary>
    static member Of: func: System.Func<'s, struct ('s * 'a)> -> State<'s, 'a>

// Semigroup

    //[<CompiledName("Append")>]
    /// <summary>An associative binary operation on monoidal types.</summary>
    static member inline ( + ): first: State< ^s, ^a> * second: State< ^s, ^a> -> State< ^s, ^a>
        when ^a: (static member ( + ): ^a -> ^a -> ^a)


/// <summary>Operations on States.</summary>
module State =    

// Haskell Primitives

    /// <summary>Return the current state as a value.</summary>
    val get<'s> : State< ^s, ^s>

    /// <summary>Ignore the current state and put a new state into a computation.</summary>
    val put: newstate: 's -> State< ^s, unit>

    /// <summary>Embed a simple state action.</summary>
    val stateful: func: System.Func<'s, struct('s * 'a)> -> State<'s,'a>

    /// <summary>Maps an old state to a new state, discarding the old state.</summary>
    val inline modify: modification: (^s -> ^s) -> State< ^s, unit>

    /// <summary>Gets specific component of the state, using a projection function supplied.</summary>
    val inline gets: proj: (^s -> ^a) -> State< ^s, ^a>

    /// <summary>Execute the given stateful computation.</summary>
    val runState: initial: 's -> state: State< ^s, 'a> -> struct(^s * ^a)

    /// <summary>Evaluate a state computation with the given initial state and return the final value, discarding the final state.</summary>
    val evalState: initial: 's -> state: State< ^s, 'a> -> ^a

    /// <summary>Evaluate a state computation with the given initial state and return the final state, discarding the final value.</summary>
    val execState: initial: 's -> state: State< ^s, 'a> -> ^s

    /// <summary>Map both the return value and final state of a computation using the given function.</summary>
    val inline mapState: mapping: (struct(^s * ^a) -> struct(^s * ^b)) -> state: State< ^s, ^a> -> State< ^s, ^b>

    /// <summary>Executes a stateful computation on a state modified by 'f'.</summary>
    val inline withState: f: (^s -> ^s) -> state: State< ^s, ^a> -> State< ^s, ^a>


// Functor

    /// <summary>Lift a function onto a context.</summary>
    val inline map: f: (^a -> ^b) -> fa: State< ^s, ^a> -> State< ^s, ^b>


// Applicative

    [<CompiledName("Unit")>]
    /// <summary>Lift a value into a context.</summary>
    val unit: value: 'a -> State<'s, ^a>

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    val inline ap: fv: State< ^s, ^a> -> ff: State< ^s, (^a -> ^b)> -> State< ^s, ^b>

    /// <summary>Lift a binary function onto contexts.</summary>
    val inline map2: f: (^a -> ^b -> ^c) -> fa: State< ^s, ^a> -> fb: State< ^s, ^b> -> State< ^s, ^c>

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val sequence: source: #seq<State<'s, 'a>> -> State< ^s, seq< ^a>>

    ///// <summary>Evaluate each context in a sequence from left to right, and discard the results.</summary>
    ///// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    //val sequence_: source: #seq<State<'s, 'a>> -> State< ^s, unit>

    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline traverse: f: (^a -> State< ^s, ^b>) -> source: #seq< ^a> -> State< ^s, seq< ^b>>

    ///// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and discard the results.</summary>
    ///// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    //val inline traverse_: f: (^a -> State< ^s, ^b>) -> source: #seq< ^a> -> State< ^s, unit>


// Monad

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline bind: f: (^a -> State< ^s, ^b>) -> m: State< ^s, ^a> -> State< ^s, ^b>

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    val flatten: mm: State<'s, State< ^s, 'a>> -> State< ^s, ^a>

    /// <summary>Recursively generate a monadic context using up to two continuation functions to produce different effects.</summary>
    val inline fixM:
        loop: ((^a -> State< ^s, ^b>) -> (State< ^s, ^a> -> State< ^s, ^b>) -> ^a -> State< ^s, ^b>) ->
        em: Choice< ^a, State< ^s, ^a>> -> State< ^s, ^b>


    /// <summary>Computation expression / monadic-workflow type and operations for the given context.</summary>
    [<RequireQualifiedAccess>]
    module Workflow =

        /// <summary>Computation expression for the given monadic context.</summary>
        type StateBuilder =
            new: unit -> StateBuilder
            member Return: x: 'a -> State<'s, ^a>
            member ReturnFrom: m: State<'s, 'a> -> State< ^s, ^a>
            member Zero: unit -> State<'s, unit>
            member inline Bind: m: State< ^s, ^a> * f: (^a -> State< ^s, ^b>) -> State< ^s, ^b>            

    /// <summary>Computation expression instance for the given context.</summary>
    val state: Workflow.StateBuilder


// Semigroup

    /// <summary>An associative binary operation on monoidal types.</summary>
    val inline append: second: State< ^s, ^a> -> first: State< ^s, ^a> -> State< ^s, ^a>
        when ^a: (static member ( + ): ^a -> ^a -> ^a)