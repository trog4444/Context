namespace PTR.Context.Type.RWS


/// <summary>Result of an 'RWS' computation.</summary>
[<Struct>]
type RWSResult<'State, 'Log, 'Value> =
    { State: 'State
    ; Log:   'Log
    ; Value: 'Value } with
    member inline With : state: ^State -> RWSResult< ^State, ^Log, ^Value>
    member inline With : state: ^State * log: ^Log -> RWSResult< ^State, ^Log, ^Value>
    member inline With : log: ^Log -> RWSResult< ^State, ^Log, ^Value>
    member inline With : log: ^Log * value: ^Value -> RWSResult< ^State, ^Log, ^Value>
    member inline With : value: ^Value -> RWSResult< ^State, ^Log, ^Value>

    member inline Apply : f: System.Func< ^State, ^Log, ^Value, ^Result> -> ^Result


/// <summary>A computation that, when given an input environment and an initial state, returns a modified state, a 'log', and an output.</summary>
[<Struct; NoComparison; NoEquality>]
type RWS<'Env, 'State, 'Log, 'Result> =
    RWS of ('Env -> 'State -> RWSResult<'State, 'Log, 'Result>) with

    member inline Invoke : env: ^Env * state: ^State -> RWSResult< ^State, ^Log, ^Result>
            
    static member inline Unit : x: ^a -> RWS< ^e, ^s, ^w, ^a> when ^w : (static member Empty: unit -> ^w)

    member inline Select : f: System.Func< ^Result, ^NextResult> -> RWS< ^Env, ^State, ^Log, ^NextResult>


/// <summary>Operations on RWS's.</summary>
module RWS =


    /// <summary>Create a new RWS from a .Net Func (primary use is for interop).</summary>
    [<CompiledName("Make")>]
    val inline make : rws: System.Func< ^e, ^s, System.ValueTuple< ^s, ^w, ^a>> -> RWS< ^e, ^s, ^w, ^a>

    /// <summary>Unwrap an 'RWS' computation as a function.</summary>
    [<CompiledName("RunRWS")>]
    val inline runRWS : e: ^e -> s: ^s -> RWS< ^e, ^s, ^w, ^r> -> RWSResult< ^s, ^w, ^r>

    /// <summary>Evaluate a computation with the given initial state and environment, returning the final value and output, discarding the final state.</summary>
    [<CompiledName("EvalRWS")>]
    val inline evalRWS : e: ^e -> s: ^s -> RWS< ^e, ^s, ^w, ^r> -> ^w * ^r

    /// <summary>Evaluate a computation with the given initial state and environment, returning the final state and output, discarding the final value.</summary>
    [<CompiledName("ExecRWS")>]
    val inline execRWS : e: ^e -> s: ^s -> RWS< ^e, ^s, ^w, ^r> -> ^s * ^w

    /// <summary>Map the return value, final state, and output of a computation using the given function.</summary>
    [<CompiledName("MapRWS")>]
    val inline mapRWS : f: (^s -> ^w1 -> ^a -> RWSResult< ^s, ^w2, ^b>) -> RWS< ^e, ^s, ^w1, ^a> -> RWS< ^e, ^s, ^w2, ^b>

    /// <summary>Executes action with an initial environment and state modified by applying 'f'.</summary>
    [<CompiledName("WithRWS")>]
    val inline withRWS : f: (^e0 -> ^s -> ^e * ^s) -> RWS< ^e, ^s, ^w, ^a> -> RWS< ^e0, ^s, ^w, ^a>

    /// <summary>Retrieves the current state.
    [<CompiledName("Get")>]
    val inline get : unit -> RWS< ^e, ^s, ^w, ^s>  when ^w : (static member Empty: unit -> ^w)

    /// <summary>Replace the state inside the computation.</summary>
    [<CompiledName("Put")>]
    val inline put : s: ^s -> RWS< ^e, ^s, ^w, unit>  when ^w : (static member Empty: unit -> ^w)

    /// <summary>Maps an old state to a new state inside a state computation.</summary>
    [<CompiledName("Modify")>]
    val inline modify : f: (^s -> ^s) -> RWS< ^e, ^s, ^w, unit>  when ^w : (static member Empty: unit -> ^w)

    /// <summary>Retrieves the current environment.</summary>
    [<CompiledName("Ask")>]
    val inline ask : unit -> RWS< ^e, ^s, ^w, ^e> when ^w : (static member Empty: unit -> ^w)

    /// <summary>Executes a computation in a modified environment.</summary>
    [<CompiledName("Local")>]
    val inline local : localize: (^e -> ^e) -> RWS< ^e, ^s, ^w, ^a> -> RWS< ^e, ^s, ^w, ^a>

    /// <summary>Create an RWS with just a log.</summary>
    [<CompiledName("Tell")>]
    val tell : entry: 'w -> RWS<'e, 's, ^w, unit>
        
    /// <summary>Cache the results of a 'RWS' computation.</summary>
    [<CompiledName("CacheRWS")>]
    val inline cacheRWS : RWS< ^e, ^s, ^w, ^r> -> RWS< ^e, ^s, ^w, ^r> when ^e : equality and ^s : equality


// Monad

    /// <summary>Inject a value into the context type.</summary>
    val inline unit : x: ^a -> RWS< ^e, ^s, ^w, ^a>
        when ^w : (static member Empty: unit -> ^w)

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    [<CompiledName("Bind")>]
    val inline bind : f: (^a -> RWS< ^e, ^s, ^w, ^b>) -> RWS< ^e, ^s, ^w, ^a> -> RWS< ^e, ^s, ^w, ^b>
        when ^w : (static member Append: ^w -> ^w -> ^w)

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    [<CompiledName("Flatten")>]
    val inline flatten : RWS< ^e, ^s, ^w, RWS< ^e, ^s, ^w, ^a>> -> RWS< ^e, ^s, ^w, ^a>
        when ^w : (static member Append: ^w -> ^w -> ^w)

    /// <summary>Recursively generate a context using a continuation.</summary>
    [<CompiledName("RecM")>]
    val inline recM : f: ((^a -> RWS< ^e, ^s, ^w, ^b>) -> ^a -> RWS< ^e, ^s, ^w, ^b>) -> x: ^a -> RWS< ^e, ^s, ^w, ^b>
        when ^w : (static member Append: ^w -> ^w -> ^w)
        and  ^w : (static member Empty: unit -> ^w)

    /// <summary>Recursively generate a context using a continuation.</summary>
    [<CompiledName("RecM1")>]
    val inline recM1 : f: ((RWS< ^e, ^s, ^w, ^a> -> RWS< ^e, ^s, ^w, ^b>) -> ^a -> RWS< ^e, ^s, ^w, ^b>) -> x: ^a -> RWS< ^e, ^s, ^w, ^b>
        when ^w : (static member Append: ^w -> ^w -> ^w)
        and  ^w : (static member Empty: unit -> ^w)


    /// <summary>Monadic workflow-related types and values.</summary>
    module Workflow =

        /// <summary>Monadic workflow builder.</summary>
        type RWSBuilder =
            new : unit -> RWSBuilder
            
            member inline Return : x: ^a -> RWS< ^e, ^s, ^w, ^a> when ^w : (static member Empty: unit -> ^w)
            member inline ReturnFrom : m: RWS< ^e, ^s, ^w, ^a> -> RWS< ^e, ^s, ^w, ^a>
            member inline Bind: m: RWS< ^e, ^s, ^w, ^a> * (^a -> RWS< ^e, ^s, ^w, ^b>) -> RWS< ^e, ^s, ^w, ^b>
                when ^w : (static member Append: ^w -> ^w -> ^w)

            member inline Zero : unit -> RWS< ^e, ^s, ^w, unit>
                when ^w : (static member Empty: unit -> ^w)

            member inline Using : disp: ^d * body: (^d -> RWS< ^e, ^s, ^w, ^a>) -> RWS< ^e, ^s, ^w, ^a> when ^d :> System.IDisposable

            member inline TryWith : body: RWS< ^e, ^s, ^w, ^a> * handler: (exn -> RWS< ^e, ^s, ^w, ^a>) -> RWS< ^e, ^s, ^w, ^a>
            member inline TryFinally : body: RWS< ^e, ^s, ^w, ^a> * finalizer: (unit -> unit) -> RWS< ^e, ^s, ^w, ^a>

            member inline While : guard: (unit -> bool) * body: (unit -> RWS< ^e, ^s, ^w, unit>) -> RWS< ^e, ^s, ^w, unit>
                when ^w : (static member Append: ^w -> ^w -> ^w)
                and  ^w : (static member Empty: unit -> ^w)
            
            member inline For : seq: #seq< ^a> * body: (^a -> RWS< ^e, ^s, ^w, unit>) -> RWS< ^e, ^s, ^w, unit>
                when ^w : (static member Append: ^w -> ^w -> ^w)
                and  ^w : (static member Empty: unit -> ^w)


    /// <summary>Monadic workflow object.</summary>
    val rws : Workflow.RWSBuilder


// Applicative

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    [<CompiledName("Ap")>]
    val inline ap : fv: RWS< ^e, ^s, ^w, ^a> -> ff: RWS< ^e, ^s, ^w, (^a -> ^b)> -> RWS< ^e, ^s, ^w, ^b>
        when ^w : (static member Append: ^w -> ^w -> ^w)

    /// <summary>Lift a binary function onto contexts.</summary>
    [<CompiledName("Map2")>]
    val inline map2 : f: (^a -> ^b -> ^c) -> RWS< ^e, ^s, ^w, ^a> -> RWS< ^e, ^s, ^w, ^b> -> RWS< ^e, ^s, ^w, ^c>
        when ^w : (static member Append: ^w -> ^w -> ^w)

    /// <summary>Sequence two contexts.</summary>
    [<CompiledName("AndThen")>]
    val inline andThen : second: RWS< ^e, ^s, ^w, ^b> -> first: RWS< ^e, ^s, ^w, ^a> -> RWS< ^e, ^s, ^w, ^b>
        when ^w : (static member Append: ^w -> ^w -> ^w)

    /// <summary>Conditional execution of contextual expressions.</summary>
    [<CompiledName("When")>]
    val inline when_: condition: bool -> f: (unit -> RWS< ^e, ^s, ^w, unit>) -> RWS< ^e, ^s, ^w, unit>
        when ^w : (static member Empty: unit -> ^w)


// Functor

    /// <summary>Lift a function onto a context.</summary>
    [<CompiledName("Map")>]
    val inline map : f: (^a -> ^b) -> RWS< ^e, ^s, ^w, ^a> -> RWS< ^e, ^s, ^w, ^b>


// Bifunctor

    /// <summary>Map over both arguments at the same time.</summary>
    [<CompiledName("Bimap")>]
    val inline bimap: f: (^a -> ^c) -> g: (^b -> ^d) -> bf: RWS< ^e, ^s, ^a, ^b> -> RWS< ^e, ^s, ^c, ^d>

    /// <summary>Map covariantly over the first argument.</summary>
    [<CompiledName("MapFst")>]
    val inline mapFst: f: (^a -> ^c) -> bf: RWS< ^e, ^s, ^a, ^b> -> RWS< ^e, ^s, ^c, ^b>

    /// <summary>Map covariantly over the second argument.</summary>
    [<CompiledName("MapSnd")>]
    val inline mapSnd: g: (^b -> ^c) -> bf: RWS< ^e, ^s, ^a, ^b> -> RWS< ^e, ^s, ^a, ^c>


// Semigroup

    /// <summary>An associative binary operation on contexts.</summary>
    val inline append : first: RWS< ^e, ^s, ^w, ^a> -> second: RWS< ^e, ^s, ^w, ^a> -> RWS< ^e, ^s, ^w, ^a>
        when ^a : (static member Append: ^a -> ^a -> ^a)
        and  ^w : (static member Append: ^w -> ^w -> ^w)

// Traversable

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("Sequence")>]
    val inline sequence : source: #seq<RWS< ^e, ^s, ^w, ^a>> -> RWS< ^e, ^s, ^w, seq< ^a>>
        when ^w : (static member Append: ^w -> ^w -> ^w)
        and  ^w : (static member Empty: unit -> ^w)

    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("Traverse")>]
    val inline traverse : f: (^a -> RWS< ^e, ^s, ^w, ^b>) -> source: #seq< ^a> -> RWS< ^e, ^s, ^w, seq< ^b>>
        when ^w : (static member Append: ^w -> ^w -> ^w)
        and  ^w : (static member Empty: unit -> ^w)