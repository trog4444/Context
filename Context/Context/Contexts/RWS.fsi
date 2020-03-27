namespace Rogz.Context.Data.RWS


/// <summary>Operations on RWSs.</summary>
module RWS =

// Interop

    /// <summary>Create a RWS from the given function.</summary>
    val inline fromFunc: f: System.Func< ^e, ^s, RWSResult< ^s, ^w, ^a>> -> RWS< ^e, ^s, ^w, ^a>


// Minimal

    // Reader

    /// <summary>Retreive the current environment.</summary>
    val inline ask: unit -> RWS< ^e, ^s, ^w, ^e>
        when ^w: (static member Empty: unit -> ^w)

    /// <summary>Gets specific component of the environment, using a projection function supplied.</summary>
    val inline asks: query: (^e -> ^a) -> RWS< ^e, ^s, ^w, ^a>
        when ^w: (static member Empty: unit -> ^w)

    /// <summary>Executes a computation in a modified environment.</summary>
    val inline local: localize: (^e -> ^e) -> rws: RWS< ^e, ^s, ^w, ^a> -> RWS< ^e, ^s, ^w, ^a>


    // Writer

    /// <summary>Insert a 'record' into the accumulation.</summary>
    val tell: record: 'w -> RWS<'e, 's, ^w, unit>

    /// <summary>Attaches the accumulated value to the output.</summary>
    val listen: rws: RWS<'e, 's, 'w, 'a> -> RWS< ^e, ^s, ^w, struct (^w * ^a)>

    /// <summary>Applies a function to the accumulator then attaches the result to the output.</summary>
    val inline listens: f: (^w -> ^a -> ^b) -> rws: RWS< ^e, ^s, ^w, ^a> -> RWS< ^e, ^s, ^w, ^b>


    // State

    /// <summary>Return the current state as a value.</summary>
    val inline get: unit -> RWS< ^e, ^s, ^w, ^s>
        when ^w: (static member Empty: unit -> ^w)

    /// <summary>Ignore the current state and put a new state into a computation.</summary>
    val inline put: state: ^s -> RWS< ^e, ^s, ^w, unit>
        when ^w: (static member Empty: unit -> ^w)

    /// <summary>Maps an old state to a new state, discarding the old state.</summary>
    val inline modify: modification: (^s -> ^s) -> RWS< ^e, ^s, ^w, unit>
        when ^w: (static member Empty: unit -> ^w)

    /// <summary>Gets specific component of the state, using a projection function supplied.</summary>
    val inline gets: proj: (^s -> ^a) -> RWS< ^e, ^s, ^w, ^a>
        when ^w: (static member Empty: unit -> ^w)


// Primitives

    /// <summary>Execute the given function with the supplied environment and initial state.</summary>
    val inline runRWS: env: ^e -> state: ^s -> rws: RWS< ^e, ^s, ^w, ^a> -> RWSResult< ^s, ^w, ^a>

    /// <summary>Map the return log, value, and final state of a computation using the given function.</summary>
    val inline mapRWS: mapping: (RWSResult< ^s, ^w0, ^a> -> RWSResult< ^s, ^w1, ^b>) -> rws: RWS< ^e, ^s, ^w0, ^a> -> RWS< ^e, ^s, ^w1, ^b>
    
    /// <summary>Executes a computation on an environment and state modified by 'f'.</summary>
    val inline withRWS: f: (^e1 -> ^s -> struct (^e0 * ^s)) -> rws: RWS< ^e0, ^s, ^w, ^a> -> RWS< ^e1, ^s, ^w, ^a>
  
    ///// <summary>Caches the result(s) of an RWS computation.</summary>
    //val inline cache: rws: RWS< ^e, ^s, ^w, ^a>  -> RWS< ^e, ^s, ^w, ^a>
    //    when ^e: equality
    //    and  ^s: equality


// Functor

    /// <summary>Lift a function onto a context.</summary>
    val inline map: f: (^a -> ^b) -> fa: RWS< ^e, ^s, ^w, ^a> -> RWS< ^e, ^s, ^w, ^b>


// Bifunctor (over Log and Value)

    /// <summary>Map over both arguments covariantly.</summary>
    val inline bimap: f: (^a -> ^c) -> g: (^b -> ^d) -> bf: RWS< ^e, ^s, ^a, ^b> -> RWS< ^e, ^s, ^c, ^d>

    /// <summary>Map over the first value, leaving the second value as-is.</summary>
    val inline mapFirst: f: (^a -> ^c) -> bf: RWS< ^e, ^s, ^a, ^b> -> RWS< ^e, ^s, ^c, ^b>


// Applicative

    /// <summary>Lift a value into a context.</summary>
    val inline unit: value: ^a -> RWS< ^e, ^s, ^w, ^a>
        when ^w: (static member Empty: unit -> ^w)

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    val inline ap: fv: RWS< ^e, ^s, ^w, ^a> -> ff: RWS< ^e, ^s, ^w, (^a -> ^b)> -> RWS< ^e, ^s, ^w, ^b>
        when ^w: (static member Append: ^w -> ^w -> ^w)

    /// <summary>Lift a binary function onto contexts.</summary>
    val inline map2: f: (^a -> ^b -> ^c) -> fa: RWS< ^e, ^s, ^w, ^a> -> fb: RWS< ^e, ^s, ^w, ^b> -> RWS< ^e, ^s, ^w, ^c>
        when ^w: (static member Append: ^w -> ^w -> ^w)

    ///// <summary>Sequence two contexts, discarding the results of the first.</summary>
    //val inline andthen: fb: RWS< ^e, ^s, ^w, ^b> -> fa: RWS< ^e, ^s, ^w, ^a> -> RWS< ^e, ^s, ^w, ^b>
    //    when ^w: (static member Append: ^w -> ^w -> ^w)

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline sequence: source: seq<RWS< ^e, ^s, ^w, ^a>> -> RWS< ^e, ^s, ^w, ^a seq>
        when ^w: (static member Empty: unit -> ^w)
        and  ^w: (static member Append: ^w -> ^w -> ^w)

    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline traverse: f: (^a -> RWS< ^e, ^s, ^w, ^b>) -> source: seq< ^a> -> RWS< ^e, ^s, ^w, ^b seq>
        when ^w: (static member Empty: unit -> ^w)
        and  ^w: (static member Append: ^w -> ^w -> ^w)


// Biapplicative (over Log and Value)

    /// <summary>Lift two values into a context.</summary>
    val biunit: a: 'a -> b: 'b -> RWS<'e, 's, ^a, ^b>

    /// <summary>Lift two binary functions onto contexts.</summary>
    val inline bimap2: f: (^a -> ^b -> ^c) -> g: (^d -> ^e -> ^f) -> fad: RWS< ^env, ^s, ^a, ^d> -> fbe: RWS< ^env, ^s, ^b, ^e> -> RWS< ^env, ^s, ^c, ^f>


// Monad

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline bind: f: (^a -> RWS< ^e, ^s, ^w, ^b>) -> ma: RWS< ^e, ^s, ^w, ^a> -> RWS< ^e, ^s, ^w, ^b>
        when ^w: (static member Append: ^w -> ^w -> ^w)

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    val inline flatten: mm: RWS< ^e, ^s, ^w, RWS< ^e, ^s, ^w, ^a>> -> RWS< ^e, ^s, ^w, ^a>
        when ^w: (static member Append: ^w -> ^w -> ^w)

    /// <summary>Recursively generate a monadic context using up to two continuation functions to produce different effects.</summary>
    val inline fixM:
        loop: ((^a -> RWS< ^e, ^s, ^w, ^b>) -> (RWS< ^e, ^s, ^w, ^a> -> RWS< ^e, ^s, ^w, ^b>) -> ^a -> RWS< ^e, ^s, ^w, ^b>) ->
        em: Choice< ^a, RWS< ^e, ^s, ^w, ^a>> -> RWS< ^e, ^s, ^w, ^b>
        when ^w: (static member Empty: unit -> ^w)
        and  ^w: (static member Append: ^w -> ^w -> ^w)

    // foldlM
    // foldrM


    /// <summary>Computation expression / monadic-workflow type and operations for the given context.</summary>
    [<RequireQualifiedAccess>]
    module Workflow =

        /// <summary>Computation expression for the given monadic context.</summary>
        type RWSBuilder =
            new: unit -> RWSBuilder
            member inline Return: x: ^a -> RWS< ^e, ^s, ^w, ^a> when ^w: (static member Empty: unit -> ^w)
            member ReturnFrom: m: RWS<'e, 's, 'w, 'a> -> RWS< ^e, ^s, ^w, ^a>
            member inline Bind: m: RWS< ^e, ^s, ^w, ^a> * f: (^a -> RWS< ^e, ^s, ^w, ^b>) -> RWS< ^e, ^s, ^w, ^b> when ^w: (static member Append: ^w -> ^w -> ^w)
            member inline Zero: unit -> RWS< ^e, ^s, ^w, unit> when ^w: (static member Empty: unit -> ^w)
            //member inline Using: disp: ^d * f: (^d -> RWS< ^e, ^s, ^w, ^a>) -> RWS< ^e, ^s, ^w, ^a> when ^d :> System.IDisposable
            //member inline TryWith: m: RWS< ^e, ^s, ^w, ^a> * handler: (exn -> RWS< ^e, ^s, ^w, ^a>) -> RWS< ^e, ^s, ^w, ^a>
            //member inline TryFinally: m: RWS< ^e, ^s, ^w, ^a> * finalizer: (unit -> unit) -> RWS< ^e, ^s, ^w, ^a>
            //member Using: disp: 'd * f: ('d -> RWS<'e, 's, 'w, 'a>) -> RWS<'e, 's, 'w, 'a> when 'd :> System.IDisposable
            //abstract TryWith: m: RWS<'e, 's, 'w, 'a> * h: (exn -> RWS<'e, 's, 'w, 'a>) -> RWS<'e, 's, 'w, 'a>
            //abstract TryFinally: m: RWS<'e, 's, 'w, 'a> * f: (unit -> unit) -> RWS<'e, 's, 'w, 'a>


    /// <summary>Computation expression instance for the given context.</summary>
    val rws: Workflow.RWSBuilder


// Semigroup

    /// <summary>An associative binary operation on contexts.</summary>
    val inline append: first: RWS< ^e, ^s, ^w, ^a> -> second: RWS< ^e, ^s, ^w, ^a> -> RWS< ^e, ^s, ^w, ^a>
        when ^w: (static member Append: ^w -> ^w -> ^w)
        and  ^a: (static member Append: ^a -> ^a -> ^a)