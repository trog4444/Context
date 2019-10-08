namespace PTR.Context.Type.State


/// <summary>Result of a stateful computation.</summary>
[<Struct>]
type StateValue<'State, 'Value> = { State: 'State ; Value: 'Value } with

    member inline With : state: ^State -> StateValue< ^State, ^Value>
    member inline With : value: ^Value -> StateValue< ^State, ^Value>


/// <summary>Stateful computations, i.e. computations that consume an initial state and return a value along with a new state.</summary>
[<Struct; NoComparison; NoEquality>]
type State<'State, 'Value> = State of ('State -> StateValue<'State, 'Value>) with

    member inline Invoke : state: ^State -> StateValue< ^State, ^Value>

    static member inline Unit : x: ^a -> State< ^s, ^a>
    
    member inline Select : f: System.Func< ^Value, ^NextValue> -> State< ^State, ^NextValue>
    member inline Select2 : second: State< ^State, ^NextValue> * f: System.Func< ^Value, ^NextValue, ^FinalValue> -> State< ^State, ^FinalValue>
    member inline SelectMany : f: System.Func< ^Value, State< ^State, ^NextValue>> -> State< ^State, ^NextValue>
    member inline SelectMany : f: System.Func< ^Value, State< ^State, ^NextValue>> * g: System.Func< ^Value, ^NextValue, ^FinalValue> -> State< ^State, ^FinalValue>
    
    member inline Join : t: State< ^State, ^NextValue> * kt: System.Func< ^Value, ^K> * ku: System.Func< ^NextValue, ^K> * rs: System.Func< ^Value, ^NextValue, ^FinalValue> -> State< ^State, ^FinalValue>

    member inline ContinueWith : f: System.Func<State< ^State, ^Value>, ^NewValue> -> State< ^State, ^NewValue>

    static member inline Append : first: State< ^s, ^a> * second: State< ^s, ^a> -> State< ^s, ^a>
        when ^a : (static member Append: ^a -> ^a -> ^a)


/// <summary>Operations on States.</summary>
module State =

    /// <summary>Active patterns on State values.</summary>
    module Pattern =
  
        /// <summary>Returns the state and value of a Stateful computation result as a pair (tuple).</summary>
        val inline ( |SVPair| ) : sv: StateValue< ^s, ^a> -> struct (^s * ^a)


// Primitives

    /// <summary>Create a new State from a .Net Func (primary use is for interop).</summary>
    [<CompiledName("Make")>]
    val inline make : state: System.Func< ^s, System.ValueTuple< ^s, ^a>> -> State< ^s, ^a>

    /// <summary>Run a state computation with the given initial state and return the final state and value from it.</summary>
    [<CompiledName("RunState")>]
    val runState : s: 's -> State< ^s, 'a> -> StateValue< ^s, ^a>

    /// <summary>Evaluate a state computation with the given initial state and return the final value, discarding the final state.</summary>
    [<CompiledName("EvalState")>]
    val evalState : s: 's -> State< ^s, 'a> -> ^a

    /// <summary>Execute a state computation with the given initial state and return the final state, discarding the final value.</summary>
    [<CompiledName("ExecState")>]
    val execState : s: 's -> State< ^s, 'a> -> ^s

    /// <summary>Map both the return value and final state of a computation using the given function.</summary>
    [<CompiledName("MapState")>]
    val inline mapState : f: (^s -> ^a -> ^s * ^b) -> State< ^s, ^a> -> State< ^s, ^b>

    /// <summary>'withState f st' executes action 'st' on a state modified by applying 'f'.</summary>
    [<CompiledName("WithState")>]
    val inline withState : f: (^s -> ^s) -> State< ^s, ^a> -> State< ^s, ^a>

    /// <summary>Retrieves the current state.</summary>
    [<CompiledName("Get")>]
    val get<'s> : State< ^s, ^s>

    /// <summary>Replace the state inside the computation.</summary>
    [<CompiledName("Put")>]
    val put : s: 's -> State< ^s, unit>

    /// <summary>Maps an old state to a new state inside a state computation.</summary>
    [<CompiledName("Modify")>]
    val inline modify : f: (^s -> ^s) -> State< ^s, unit>

    /// <summary>Store computed results to prevent recomputation on the same inputs.</summary>
    [<CompiledName("CacheState")>]
    val inline cacheState : State< ^s, ^a> -> State< ^s, ^a> when ^s : equality


// Isomorphisms

    /// <summary>Create a new StateValue-pair from two provided values.</summary>
    [<CompiledName("NewSV")>]
    val inline newSV : state: ^s -> value: ^a -> StateValue< ^s, ^a>


// Monad

    /// <summary>Inject a value into the context type.</summary>
    val inline unit : x: ^a -> State< ^s, ^a>

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    [<CompiledName("Bind")>]
    val inline bind : f: (^a -> State< ^s, ^b>) -> m: State< ^s, ^a> -> State< ^s, ^b>

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    [<CompiledName("Flatten")>]
    val inline flatten : mm: State< ^s, State< ^s, ^a>> -> State< ^s, ^a>

    /// <summary>Recursively generate a context using a continuation.</summary>
    [<CompiledName("RecM")>]
    val inline recM : f: ((^a -> State< ^s, ^b>) -> ^a -> State< ^s, ^b>) -> x: ^a -> State< ^s, ^b>

    /// <summary>Recursively generate a context using a continuation.</summary>
    [<CompiledName("RecM1")>]
    val inline recM1 : f: ((State< ^s, ^a> -> State< ^s, ^b>) -> ^a -> State< ^s, ^b>) -> x: ^a -> State< ^s, ^b>


    /// <summary>Monadic workflow-related types and values.</summary>
    module Workflow =

        /// <summary>Monadic workflow builder.</summary>
        type StateBuilder =
            new : unit -> StateBuilder
        
            member inline Return : x: ^a -> State< ^s, ^a>
            member inline ReturnFrom : m: State< ^s, ^a> -> State< ^s, ^a>
            member inline Bind: m: State< ^s, ^a> * f: (^a -> State< ^s, ^b>) -> State< ^s, ^b>

            member inline Zero : unit -> State< ^s, unit>

            member inline Using : disp: ^d * body: (^d -> State< ^s, ^a>) -> State< ^s, ^a> when ^d :> System.IDisposable

            member inline TryWith : body: State< ^s, ^a> * handler: (exn -> State< ^s, ^a>) -> State< ^s, ^a>
            member inline TryFinally : body: State< ^s, ^a> * finalizer: (unit -> unit) -> State< ^s, ^a>

            member inline While : guard: (unit -> bool) * body: (unit -> State< ^s, unit>) -> State< ^s, unit>
        
            member inline For : seq: #seq< ^a> * body: (^a -> State< ^s, unit>) -> State< ^s, unit>


    /// <summary>Monadic workflow object.</summary>
    val state : Workflow.StateBuilder


// Applicative

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    [<CompiledName("Ap")>]
    val inline ap : fv: State< ^s, ^a> -> ff: State< ^s, (^a -> ^b)> -> State< ^s, ^b>

    /// <summary>Lift a binary function onto contexts.</summary>
    [<CompiledName("Map2")>]
    val inline map2 : f: (^a -> ^b -> ^c) -> State< ^s, ^a> -> State< ^s, ^b> -> State< ^s, ^c>

    /// <summary>Sequence two contexts.</summary>
    [<CompiledName("AndThen")>]
    val inline andThen : second: State< ^s, ^b> -> first: State< ^s, ^a> -> State< ^s, ^b>

    /// <summary>Conditional execution of contextual expressions.</summary>
    [<CompiledName("When")>]
    val inline when_: condition: bool -> f: (unit -> State< ^s, unit>) -> State< ^s, unit>


// Functor

    /// <summary>Lift a function onto a context.</summary>
    [<CompiledName("Map")>]
    val inline map : f: (^a -> ^b) -> fa: State< ^s, ^a> -> State< ^s, ^b>


// Semigroup

    /// <summary>An associative binary operation on contexts.</summary>
    val inline append : first: State< ^s, ^a> -> second: State< ^s, ^a> -> State< ^s, ^a>
        when ^a : (static member Append: ^a -> ^a -> ^a)


// Traversable

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("Sequence")>]
    val inline sequence : source: #seq<State< ^s, ^a>> -> State< ^s, seq< ^a>>

    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("Traverse")>]
    val inline traverse : f: (^a -> State< ^s, ^b>) -> source: #seq< ^a> -> State< ^s, seq< ^b>>