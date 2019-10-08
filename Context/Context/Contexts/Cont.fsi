namespace PTR.Context.Type.Cont


/// <summary>A "continuation-passing style" (CPS) computation that uses an intermediate
/// value of type 'T' within a continuation to produce a final result of type 'R'.</summary>
[<Struct; NoComparison; NoEquality>]
type Cont<'R, 'T> = Cont of (('T -> 'R) -> 'R) with

    member inline Invoke : cont: System.Func< ^T, ^R> -> ^R

    static member inline Unit : x: ^a -> Cont< ^r, ^a>
    
    member inline Select : f: System.Func< ^T, ^U> -> Cont< ^R, ^U>
    member inline Select2 : second: Cont< ^R, ^U> * f: System.Func< ^T, ^U, ^V> -> Cont< ^R, ^V>
    member inline SelectMany : f: System.Func< ^T, Cont< ^R, ^U>> -> Cont< ^R, ^U>
    member inline SelectMany : f: System.Func< ^T, Cont< ^R, ^U>> * g: System.Func< ^T, ^U, ^V> -> Cont< ^R, ^V>
    
    member inline Join : t: Cont< ^R, ^U> * kt: System.Func< ^T, ^K> * ku: System.Func< ^U, ^K> * rs: System.Func< ^T, ^U, ^V> -> Cont< ^R, ^V>

    member inline ContinueWith : f: System.Func<Cont< ^R, ^T>, ^U> -> Cont< ^R, ^U>

    static member inline Append : first: Cont< ^r, ^a> * second: Cont< ^r, ^a> -> Cont< ^r, ^a>
        when ^a : (static member Append: ^a -> ^a -> ^a)


/// <summary>Operations on Conts.</summary>
module Cont =

// Primitives

    /// <summary>Create a new Continuation from a .Net Func (primary use is for interop).</summary>
    [<CompiledName("Make")>]
    val inline make : f: System.Func<System.Func< ^T, ^R>, ^R> -> Cont< ^R, ^T>

    /// <summary>The result of running a CPS computation with a given final continuation.</summary>
    [<CompiledName("RunCont")>]
    val inline runCont: k: (^a -> ^r) -> Cont< ^r, ^a> -> ^r

    /// <summary>The result of running a CPS computation with the identity as the final continuation.</summary>
    [<CompiledName("EvalCont")>]
    val evalCont: Cont<'r, ^r> -> ^r

    /// <summary>Apply a function to transform the result of a continuation-passing computation.</summary>
    [<CompiledName("MapCont")>]
    val inline mapCont: f: (^r -> ^r) -> Cont< ^r, ^a> -> Cont< ^r, ^a>

    /// <summary>Apply a function to transform the continuation passed to a CPS computation.</summary>
    [<CompiledName("WithCont")>]
    val inline withCont: f: ((^b -> ^r) -> ^a -> ^r) -> Cont< ^r, ^a> -> Cont< ^r, ^b>

    /// <summary>End the current continuation chain with a specific value.</summary>
    [<CompiledName("Exit")>]
    val exit: x: 'r -> Cont< ^r, '``_``>

    /// <summary>shift 'f' captures the continuation up to the nearest enclosing 'reset' and passes it to 'f'.</summary>
    [<CompiledName("Shift")>]
    val inline shift: f: ((^a -> ^r) -> Cont< ^r, ^r>) -> Cont< ^r, ^a>

    /// <summary>reset 'm' delimits the continuation of any shift inside 'm'.</summary>
    [<CompiledName("Reset")>]
    val inline reset: Cont< ^r, ^r> -> Cont< ^r0, ^r>

    /// <summary>Call with current continuation.</summary>
    [<CompiledName("CallCC")>]
    val inline callCC: f: ((^a -> Cont< ^r, ^``_``>) -> Cont< ^r, ^a>) -> Cont< ^r, ^a>

    /// <summary>Allows looping with a given continuation function and input.</summary>
    [<CompiledName("GetCC")>]
    val inline getCC: x0: ^a -> Cont< ^r, struct (^a * (^a -> Cont< ^r, ^``_``>))>

    /// <summary>Attempt to apply `fOk` on an input and return a continuation. If the function fails,
    /// return a continuation with the result of applying `fErr` to the input and exception.</summary>
    [<CompiledName("TryCC")>]
    val inline tryCC: fOk: (^a -> ^b) -> fErr: (^a -> exn -> ^b) -> input: ^a -> Cont< ^r, ^b>

    /// <summary>Caches the result(s) of a `Cont` computation.</summary>
    [<CompiledName("CacheCont")>]
    val inline cacheCont: Cont< ^r, ^a> -> Cont< ^r, ^a> when ^a : equality


// Monad

    /// <summary>Inject a value into the context type.</summary>
    val inline unit : x: ^a -> Cont< ^r, ^a>

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    [<CompiledName("Bind")>]
    val inline bind : f: (^a -> Cont< ^r, ^b>) -> m: Cont< ^r, ^a> -> Cont< ^r, ^b>

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    [<CompiledName("Flatten")>]
    val inline flatten : mm: Cont< ^r, Cont< ^r, ^a>> -> Cont< ^r, ^a>

    /// <summary>Recursively generate a context using a continuation.</summary>
    [<CompiledName("RecM")>]
    val inline recM : f: ((^a -> Cont< ^r, ^b>) -> ^a -> Cont< ^r, ^b>) -> x: ^a -> Cont< ^r, ^b>

    /// <summary>Recursively generate a context using a continuation.</summary>
    [<CompiledName("RecM1")>]
    val inline recM1 : f: ((Cont< ^r, ^a> -> Cont< ^r, ^b>) -> ^a -> Cont< ^r, ^b>) -> x: ^a -> Cont< ^r, ^b>


    /// <summary>Monadic workflow-related types and values.</summary>
    module Workflow =

        /// <summary>Monadic workflow builder.</summary>
        type ContBuilder =
            new : unit -> ContBuilder
            
            member inline Return : x: ^a -> Cont< ^r, ^a>
            member inline ReturnFrom : m: Cont< ^r, ^a> -> Cont< ^r, ^a>
            member inline Bind: m: Cont< ^r, ^a> * (^a -> Cont< ^r, ^b>) -> Cont< ^r, ^b>
            
            member inline Zero : unit -> Cont< ^r,unit>

            member inline Using : disp: ^d * body: (^d -> Cont< ^r, ^a>) -> Cont< ^r, ^a> when ^d :> System.IDisposable

            member inline TryWith : body: Cont< ^r, ^a> * handler: (exn -> Cont< ^r, ^a>) -> Cont< ^r, ^a>
            member inline TryFinally : body: Cont< ^r, ^a> * finalizer: (unit -> unit) -> Cont< ^r, ^a>

            member inline While : guard: (unit -> bool) * body: (unit -> Cont< ^r,unit>) -> Cont< ^r,unit>
            
            member inline For : seq: #seq< ^a> * body: (^a -> Cont< ^r,unit>) -> Cont< ^r,unit>


    /// <summary>Monadic workflow object.</summary>
    val cont : Workflow.ContBuilder


// Applicative

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    [<CompiledName("Ap")>]
    val inline ap : fv: Cont< ^r, ^a> -> ff: Cont< ^r,(^a -> ^b)> -> Cont< ^r, ^b>

    /// <summary>Lift a binary function onto contexts.</summary>
    [<CompiledName("Map2")>]
    val inline map2 : f: (^a -> ^b -> ^c) -> fa: Cont< ^r, ^a> -> fb: Cont< ^r, ^b> -> Cont< ^r, ^c>

    /// <summary>Sequence two contexts.</summary>
    [<CompiledName("AndThen")>]
    val inline andThen : second: Cont< ^r, ^b> -> first: Cont< ^r, ^a> -> Cont< ^r, ^b>

    /// <summary>Conditional execution of contextual expressions.</summary>
    [<CompiledName("When")>]
    val inline when_: condition: bool -> f: (unit -> Cont< ^r, unit>) -> Cont< ^r, unit>


// Functor

    /// <summary>Lift a function onto a context.</summary>
    [<CompiledName("Map")>]
    val inline map : f: (^a -> ^b) -> fa: Cont< ^r, ^a> -> Cont< ^r, ^b>


// Comonad

    /// <summary>Retrieve a value out of a context.</summary>
    [<CompiledName("Extract")>]
    val extract : w: Cont<'a, ^a> -> ^a

    /// <summary>Sequentially compose two co-contexts, passing any value produced by the first as an argument to the second.</summary>
    [<CompiledName("Extend")>]
    val inline extend : j: (Cont< ^r, ^a> -> ^b) -> w: Cont< ^r, ^a> -> Cont< ^r, ^b>

    /// <summary>Takes a comonadic container and produces a container of containers.</summary>
    [<CompiledName("Duplicate")>]
    val duplicate : w: Cont<'r, 'a> -> Cont< ^r, Cont< ^r, ^a>>

    /// <summary>Deconstructs a comonad through recursive (effectful) computations. Computation proceeds through the use of a continuation function.</summary>
    [<CompiledName("RecW")>]
    val inline recW : f: ((Cont< ^a, ^a> -> ^a) -> Cont< ^a, ^a> -> ^a) -> w: Cont< ^a, ^a> -> ^a


// Semigroup

    /// <summary>An associative binary operation on contexts.</summary>
    val inline append : first: Cont< ^r, ^a> -> second: Cont< ^r, ^a> -> Cont< ^r, ^a>
        when ^a : (static member Append: ^a -> ^a -> ^a)


// Foldable

    [<CompiledName("Fold")>]
    val inline fold : folder: (^s -> ^a -> ^s) -> seed: ^s -> source: Cont<unit, ^a> -> ^s

    [<CompiledName("FoldBack")>]
    val inline foldBack : folder: (^a -> ^s -> ^s) -> seed: ^s -> source: Cont<unit, ^a> -> ^s
    
    [<CompiledName("Foldr")>]
    val inline foldr : folder: (^a -> (unit -> ^s) -> ^s) -> seed: (unit -> ^s) -> source: Cont<unit, ^a> -> ^s

    [<CompiledName("Foldl")>]
    val inline foldl : folder: ((unit -> ^s) -> ^a -> ^s) -> seed: (unit -> ^s) -> source: Cont<unit, ^a> -> ^s

    [<CompiledName("Foldm")>]
    val inline foldm : f: (^a -> ^m) -> source: Cont<unit, ^a> -> ^m
        when ^m : (static member Append: ^m -> ^m -> ^m)
        and  ^m : (static member Empty: unit -> ^m)

    [<CompiledName("MapFold")>]
    val inline mapFold : mapping: (^s -> ^a -> ^b * ^s) -> seed: ^s -> source: Cont<unit, ^a> -> Cont<unit, ^b> * ^s

    [<CompiledName("MapFoldBack")>]
    val inline mapFoldBack : mapping: (^a -> ^s -> ^b * ^s) -> seed: ^s -> source: Cont<unit, ^a> -> Cont<unit, ^b> * ^s


// Traversable

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("Sequence")>]
    val inline sequence : source: seq<Cont< ^r, ^a>> -> Cont< ^r, seq< ^a>>

    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("Traverse")>]
    val inline traverse : f: (^a -> Cont< ^r, ^b>) -> source: #seq< ^a> -> Cont< ^r, seq< ^b>>