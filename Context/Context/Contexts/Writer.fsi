namespace PTR.Context.Type.Writer


/// <summary>Type that holds both a 'log' and 'value'.</summary>
[<Struct>]
type Writer<'Log, 'Value> = { Log: 'Log ;  Value: 'Value } with

    member inline With : log: ^Log -> Writer< ^Log, ^Value>
    member inline With : value: ^Value -> Writer< ^Log, ^Value>

    member inline Apply : f: System.Func< ^Log, ^Value, ^Result> -> ^Result

    static member inline Unit : x: ^a -> Writer< ^w, ^a> when ^w : (static member Empty: unit -> ^w)

    member inline Select : f: System.Func< ^Value, ^NextValue> -> Writer< ^Log, ^NextValue>

    member inline Extend : f: System.Func<Writer< ^Log, ^Value>, ^NextValue> -> Writer< ^Log, ^NextValue>

    static member inline Append : first: Writer< ^w, ^a> * second: Writer< ^w, ^a> -> Writer< ^w, ^a>
        when ^a : (static member Append: ^a -> ^a -> ^a)
        and  ^w : (static member Append: ^w -> ^w -> ^w)


/// <summary>Operations on Writers.</summary>
module Writer =

    /// <summary>Active patterns on Writers.</summary>
    module Pattern =
  
        /// <summary>Return a 'Writer'-value as a log/value pair.</summary>
        val inline ( |Writer| ) : w: Writer< ^w, ^v> -> struct (^w * ^v)
  
        /// <summary>Return the log from a 'Writer'.</summary>
        val inline ( |WriterLog| ) : w: Writer< ^w, ^v> -> ^w
  
        /// <summary>Return the value from a 'Writer'.</summary>
        val inline ( |WriterValue| ) : w: Writer< ^w, ^v> -> ^v


// Primitives

    /// <summary>Unwrap a Writer as a (log, value) pair.</summary>
    [<CompiledName("RunWriter")>]
    val inline runWriter : f: (^w -> ^a -> ^r) -> Writer< ^w, ^a> -> ^r

    /// <summary>Create a Writer with just a log.</summary>
    [<CompiledName("Tell")>]
    val tell : log: 'w -> Writer< ^w, unit>

    /// <summary>Takes a Writer and combines its value and logger into a new value pair, while maintaining the same logger.</summary>
    [<CompiledName("Listen")>]
    val listen : Writer<'w, 'v> -> Writer< ^w, (^w * ^v)>

    /// <summary>Adds the result of applying 'f' to the logger and combines it with the original value.</summary>
    [<CompiledName("Listens")>]
    val inline listens : f: (^w -> ^b) -> Writer< ^w, ^a> -> Writer< ^w, (^a * ^b)>


// Isomorphisms

    /// <summary>Convert two values to a 'Writer'.</summary>
    [<CompiledName("Write")>]
    val write : log: 'w -> value: 'a -> Writer< ^w, ^a>

    /// <summary>Convert a 'Writer' to a pair.</summary>
    [<CompiledName("ToPair")>]
    val toPair : Writer<'w, 'a> -> ^w * ^a

    /// <summary>Convert a 'Writer' to a pair.</summary>
    [<CompiledName("ToPair1")>]
    val toPair1 : Writer<'w, 'a> -> struct (^w * ^a)


// Monad

    /// <summary>Inject a value into the context type.</summary>
    val inline unit : x: ^a -> Writer< ^w, ^a>
        when ^w : (static member Empty: unit -> ^w)

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    [<CompiledName("Bind")>]
    val inline bind : f: (^a -> Writer< ^w, ^b>) -> Writer< ^w, ^a> -> Writer< ^w, ^b>
        when ^w : (static member Append: ^w -> ^w -> ^w)

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    [<CompiledName("Flatten")>]
    val inline flatten : Writer< ^w, Writer< ^w, ^a>> -> Writer< ^w, ^a>
        when ^w : (static member Append: ^w -> ^w -> ^w)

    /// <summary>Recursively generate a context using a continuation.</summary>
    [<CompiledName("RecM")>]
    val inline recM : f: ((^a -> Writer< ^w, ^b>) -> ^a -> Writer< ^w, ^b>) -> x: ^a -> Writer< ^w, ^b>
        when ^w : (static member Append: ^w -> ^w -> ^w)
        and  ^w : (static member Empty: unit -> ^w)

    /// <summary>Recursively generate a context using a continuation.</summary>
    [<CompiledName("RecM1")>]
    val inline recM1 : f: ((Writer< ^w, ^a> -> Writer< ^w, ^b>) -> ^a -> Writer< ^w, ^b>) -> x: ^a -> Writer< ^w, ^b>
        when ^w : (static member Append: ^w -> ^w -> ^w)
        and  ^w : (static member Empty: unit -> ^w)


    /// <summary>Monadic workflow-related types and values.</summary>
    module Workflow =

        /// <summary>Monadic workflow builder.</summary>
        type WriterBuilder =
            new : unit -> WriterBuilder
            
            member inline Return : x: ^a -> Writer< ^w, ^a> when ^w : (static member Empty: unit -> ^w)
            member inline ReturnFrom : m: Writer< ^w, ^a> -> Writer< ^w, ^a>
            member inline Bind: m: Writer< ^w, ^a> * f: (^a -> Writer< ^w, ^b>) -> Writer< ^w, ^b>
                when ^w : (static member Append: ^w -> ^w -> ^w)

            member inline Zero : unit -> Writer< ^w, unit>
                when ^w : (static member Empty: unit -> ^w)

            member inline Using : disp: ^d * body: (^d -> Writer< ^w, ^a>) -> Writer< ^w, ^a> when ^d :> System.IDisposable

            member inline TryWith : body: Writer< ^w, ^a> * handler: (exn -> Writer< ^w, ^a>) -> Writer< ^w, ^a>
            member inline TryFinally : body: Writer< ^w, ^a> * finalizer: (unit -> unit) -> Writer< ^w, ^a>

            member inline While : guard: (unit -> bool) * body: (unit -> Writer< ^w, unit>) -> Writer< ^w, unit>
                when ^w : (static member Append: ^w -> ^w -> ^w)
                and  ^w : (static member Empty: unit -> ^w)
            
            member inline For : seq: #seq< ^a> * body: (^a -> Writer< ^w, unit>) -> Writer< ^w, unit>
                when ^w : (static member Append: ^w -> ^w -> ^w)
                and  ^w : (static member Empty: unit -> ^w)


    /// <summary>Monadic workflow object.</summary>
    val writer : Workflow.WriterBuilder


// Applicative

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    [<CompiledName("Ap")>]
    val inline ap : fv: Writer< ^w, ^a> -> ff: Writer< ^w, (^a -> ^b)> -> Writer< ^w, ^b>
        when ^w : (static member Append: ^w -> ^w -> ^w)

    /// <summary>Lift a binary function onto contexts.</summary>
    [<CompiledName("Map2")>]
    val inline map2 : f: (^a -> ^b -> ^c) -> fa: Writer< ^w, ^a> -> fb: Writer< ^w, ^b> -> Writer< ^w, ^c>
        when ^w : (static member Append: ^w -> ^w -> ^w)

    /// <summary>Sequence two contexts.</summary>
    [<CompiledName("AndThen")>]
    val inline andThen : second: Writer< ^w, ^b> -> first: Writer< ^w, ^a> -> Writer< ^w, ^b>
        when ^w : (static member Append: ^w -> ^w -> ^w)

    /// <summary>Conditional execution of contextual expressions.</summary>
    [<CompiledName("When")>]
    val inline when_: condition: bool -> f: (unit -> Writer< ^w, unit>) -> Writer< ^w, unit>
        when ^w : (static member Empty: unit -> ^w)


// Functor

    /// <summary>Lift a function onto a context.</summary>
    [<CompiledName("Map")>]
    val inline map : f: (^a -> ^b) -> fa: Writer< ^w, ^a> -> Writer< ^w, ^b>


// Bifunctor

    /// <summary>Map over both arguments at the same time.</summary>
    [<CompiledName("Bimap")>]
    val inline bimap: f: (^a -> ^c) -> g: (^b -> ^d) -> bf: Writer< ^a, ^b> -> Writer< ^c, ^d>

    /// <summary>Map covariantly over the first argument.</summary>
    [<CompiledName("MapFst")>]
    val inline mapFst: f: (^a -> ^c) -> bf: Writer< ^a, ^b> -> Writer< ^c, ^b>

    /// <summary>Map covariantly over the second argument.</summary>
    [<CompiledName("MapSnd")>]
    val inline mapSnd: g: (^b -> ^c) -> bf: Writer< ^a, ^b> -> Writer< ^a, ^c>


// Comonad

    /// <summary>Retrieve a value out of a context.</summary>
    [<CompiledName("Extract")>]
    val extract : w: Writer<'w, 'a> -> ^a

    /// <summary>Sequentially compose two co-contexts, passing any value produced by the first as an argument to the second.</summary>
    [<CompiledName("Extend")>]
    val inline extend : j: (Writer< ^w, ^a> -> ^b) -> w: Writer< ^w, ^a> -> Writer< ^w, ^b>

    /// <summary>Takes a comonadic container and produces a container of containers.</summary>
    [<CompiledName("Duplicate")>]
    val duplicate : w: Writer<'w, 'a> -> Writer< ^w, Writer< ^w, ^a>>

    /// <summary>Deconstructs a comonad through recursive (effectful) computations. Computation proceeds through the use of a continuation function.</summary>
    [<CompiledName("RecW")>]
    val inline recW : f: ((Writer< ^w, ^a> -> ^a) -> Writer< ^w, ^a> -> ^a) -> w: Writer< ^w, ^a> -> ^a


// Semigroup

    /// <summary>An associative binary operation on contexts.</summary>
    val inline append : first: Writer< ^w, ^a> -> second: Writer< ^w, ^a> -> Writer< ^w, ^a>
        when ^a : (static member Append: ^a -> ^a -> ^a)
        and  ^w : (static member Append: ^w -> ^w -> ^w)


// Foldable

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    [<CompiledName("Fold")>]
    val inline fold : folder: (^s -> ^a -> ^s) -> seed: ^s -> source: Writer< ^w, ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    [<CompiledName("FoldBack")>]
    val inline foldBack : folder: (^a -> ^s -> ^s) -> seed: ^s -> source: Writer< ^w, ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation. The accumulator is a thunk that is only called as needed by the folding function.</summary>
    [<CompiledName("Foldl")>]
    val inline foldl : folder: ((unit -> ^s) -> ^a -> ^s) -> seed: (unit -> ^s) -> source: Writer< ^w, ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation. The accumulator is a thunk that is only called as needed by the folding function.</summary>
    [<CompiledName("Foldr")>]
    val inline foldr : folder: (^a -> (unit -> ^s) -> ^s) -> seed: (unit -> ^s) -> source: Writer< ^w, ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation. The accumulator type is a monoid, and the folding function is the 'append' function for the given type. The default 'seed' is the monoid's 'empty' value.</summary>
    [<CompiledName("Foldm")>]
    val inline foldm : f: (^a -> ^m) -> source: Writer< ^w, ^a> -> ^m
        when ^m : (static member Append: ^m -> ^m -> ^m)

    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
    [<CompiledName("MapFold")>]
    val inline mapFold : mapping: (^s -> ^a -> ^b * ^s) -> seed: ^s -> source: Writer< ^w, ^a> -> Writer< ^w, ^b> * ^s

    /// <summary>Combines the functionality of map and foldBack, returning the pair of the final context-value and state.</summary
    [<CompiledName("MapFoldBack")>]
    val inline mapFoldBack : mapping: (^a -> ^s -> ^b * ^s) -> seed: ^s -> source: Writer< ^w, ^a> -> Writer< ^w, ^b> * ^s


// Bifoldable

    /// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s).</summary>
    [<CompiledName("Bifold")>]
    val inline bifold : fold1: (^s -> ^a -> ^s) -> fold2: (^s -> ^b -> ^s) -> seed: ^s -> source: Writer< ^a, ^b> -> ^s

    /// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s).</summary>
    [<CompiledName("BifoldBack")>]
    val inline bifoldBack : fold1: (^a -> ^s -> ^s) -> fold2: (^b -> ^s -> ^s) -> seed: ^s -> source: Writer< ^a, ^b> -> ^s

    /// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s). The accumulator is a thunk that is only called as needed by the folding function.</summary>
    [<CompiledName("Bifoldl")>]
    val inline bifoldl : fold1: ((unit -> ^s) -> ^a -> ^s) -> fold2: ((unit -> ^s) -> ^b -> ^s) -> seed: (unit -> ^s) -> source: Writer< ^a, ^b> -> ^s

    /// <summary>Applies a function to all element(s) of up to two possible sources, threading an accumulator argument through the computation(s). The accumulator is a thunk that is only called as needed by the folding function.</summary>
    [<CompiledName("Bifoldr")>]
    val inline bifoldr : fold1: (^a -> (unit -> ^s) -> ^s) -> fold2: (^b -> (unit -> ^s) -> ^s) -> seed: (unit -> ^s) -> source: Writer< ^a, ^b> -> ^s

    /// <summary>Applies a function to all element(s) of up to two possible source(s), threading an accumulator argument through the computation. The accumulator type is a monoid, and the folding function is the 'append' function for the given type. The default 'seed' is the monoid's 'empty' value.</summary>
    [<CompiledName("Bifoldm")>]
    val inline bifoldm : f1: (^a -> ^m) -> f2: (^b -> ^m) -> source: Writer< ^a, ^b> -> ^m
        when ^m : (static member Append: ^m -> ^m -> ^m)

    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
    [<CompiledName("BimapFold")>]
    val inline bimapFold : mapping1: (^s -> ^a -> ^b * ^s) -> mapping2: (^s -> ^c -> ^d * ^s) -> seed: ^s -> source: Writer< ^a, ^c> -> Writer< ^b, ^d> * ^s

    /// <summary>Combines the functionality of map and foldBack, returning the pair of the final context-value and state.</summary
    [<CompiledName("BimapFoldBack")>]
    val inline bimapFoldBack : mapping1: (^a -> ^s -> ^b * ^s) -> mapping2: (^c -> ^s -> ^d * ^s) -> seed: ^s -> source: Writer< ^a, ^c> -> Writer< ^b, ^d> * ^s


// Traversable

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("Sequence")>]
    val inline sequence : source: #seq<Writer< ^w, ^a>> -> Writer< ^w, seq< ^a>>
        when ^w : (static member Append: ^w -> ^w -> ^w)
        and  ^w : (static member Empty: unit -> ^w)

    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("Traverse")>]
    val inline traverse : f: (^a -> Writer< ^w, ^b>) -> source: #seq< ^a> -> Writer< ^w, seq< ^b>>
        when ^w : (static member Append: ^w -> ^w -> ^w)
        and  ^w : (static member Empty: unit -> ^w)