namespace PTR.Context.Type.Reader


/// <summary>Computations which read values from a shared environment.</summary>
[<Struct; NoComparison; NoEquality>]
type Reader<'Env, 'Result> = Reader of ('Env -> 'Result) with

   member inline Invoke : env: ^Env -> ^Result

   static member inline Unit : x: ^a -> Reader< ^e, ^a>

   member inline Select : f: System.Func< ^Result, ^NextResult> -> Reader< ^Env, ^NextResult>
   member inline Select2 : first: Reader< ^Env, ^NextResult> * f: System.Func< ^Result, ^NextResult, ^FinalResult> -> Reader< ^Env, ^FinalResult>
   member inline SelectMany : f: System.Func< ^Result, Reader< ^Env, ^NextResult>> -> Reader< ^Env, ^NextResult>
   member inline SelectMany : f: System.Func< ^Result, Reader< ^Env, ^NextResult>> * g: System.Func< ^Result, ^NextResult, ^FinalResult> -> Reader< ^Env, ^FinalResult>   

   member inline Join : t: Reader< ^Env, ^NextResult> * kt: System.Func< ^Result, ^K> * ku: System.Func< ^NextResult, ^K> * rs: System.Func< ^Result, ^NextResult, ^FinalResult> -> Reader< ^Env, ^FinalResult>

   static member inline Append : first: Reader< ^e, ^a> * second: Reader< ^e, ^a> -> Reader< ^e, ^a>
       when ^a : (static member Append: ^a -> ^a -> ^a)


/// <summary>Operations on Readers.</summary>
module Reader =

// Primitives

    
    /// <summary>Create a new Reader from a .Net Func (primary use is for interop).</summary>
    [<CompiledName("Make")>]
    val inline make : reader: System.Func< ^e, ^a> -> Reader< ^e, ^a>

    /// <summary>Runs a Reader and extracts the final value from it.</summary>
    [<CompiledName("RunReader")>]
    val inline runReader : env: ^e -> Reader< ^e, ^r> -> ^r

    /// <summary>Execute a computation in a modified environment.</summary>
    [<CompiledName("WithReader")>]
    val inline withReader : f: (^e0 -> ^e) -> Reader< ^e,  ^r> -> Reader< ^e0, ^r>

    /// <summary>Retrieves the current environment.</summary>
    [<CompiledName("Ask")>]
    val ask<'e> : Reader< ^e, ^e>

    /// <summary>Executes a computation in a modified environment.</summary>
    [<CompiledName("Local")>]
    val inline local : localize: (^e -> ^e) -> Reader< ^e, ^r> -> Reader< ^e, ^r>    

    /// <summary>Flip a function then wrap it inside of a Reader.</summary>
    [<CompiledName("Flip")>]
    val inline flip : f: (^a -> ^e-> ^r) -> Reader< ^e, ^a -> ^r>

    /// <summary>Convert a function on a 2-tuple to a 2-arity, curried function.</summary>
    [<CompiledName("Curry")>]
    val inline curry : f: (^a * ^b -> ^c) -> Reader< ^a, ^b -> ^c>

    /// <summary>Convert a function on a struct 2-tuple to a 2-arity, curried function.</summary>
    [<CompiledName("Curry1")>]
    val inline curry1 : f: (struct (^a * ^b) -> ^c) -> Reader< ^a, ^b -> ^c>

    /// <summary>Convert a 2-arity, curried function into a function on a 2-tuple.</summary>
    [<CompiledName("Uncurry")>]
    val inline uncurry : f: (^a -> ^b -> ^c) -> Reader< ^a * ^b, ^c>

    /// <summary>Convert a 2-arity, curried function into a function on a struct 2-tuple.</summary>
    [<CompiledName("Uncurry1")>]
    val inline uncurry1 : f: (^a -> ^b -> ^c) -> Reader< struct (^a * ^b), ^c>

    /// <summary>Store computed results to prevent recomputation on the same inputs.</summary>
    [<CompiledName("CacheReader")>]
    val inline cacheReader : Reader< ^e, ^r> -> Reader< ^e, ^r> when ^e : equality


// Monad

    /// <summary>Inject a value into the context type.</summary>
    val unit : x: 'a -> Reader<'e, ^a>

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    [<CompiledName("Bind")>]
    val inline bind : f: (^a -> Reader< ^e, ^b>) -> Reader< ^e, ^a> -> Reader< ^e, ^b>

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    [<CompiledName("Flatten")>]
    val flatten : Reader<'e, Reader< ^e, 'a>> -> Reader< ^e, ^a>

    /// <summary>Recursively generate a context using a continuation.</summary>
    [<CompiledName("RecM")>]
    val inline recM : f: ((^a -> Reader< ^e, ^b>) -> ^a -> Reader< ^e, ^b>) -> x: ^a -> Reader< ^e, ^b>

    /// <summary>Recursively generate a context using a continuation.</summary>
    [<CompiledName("RecM1")>]
    val inline recM1 : f: ((Reader< ^e, ^a> -> Reader< ^e, ^b>) -> ^a -> Reader< ^e, ^b>) -> x: ^a -> Reader< ^e, ^b>


    /// <summary>Monadic workflow-related types and values.</summary>
    module Workflow =

        /// <summary>Monadic workflow builder.</summary>
        type ReaderBuilder =
            new : unit -> ReaderBuilder
            
            member inline Return : x: ^a -> Reader< ^e, ^a>
            member inline ReturnFrom : m: Reader< ^e, ^a> -> Reader< ^e, ^a>
            member inline Bind: m: Reader< ^e, ^a> * (^a -> Reader< ^e, ^b>) -> Reader< ^e, ^b>
            
            member inline Zero : unit -> Reader< ^e, unit>

            member inline Using : disp: ^d * body: (^d -> Reader< ^e, ^a>) -> Reader< ^e, ^a> when ^d :> System.IDisposable

            member inline TryWith : body: Reader< ^e, ^a> * handler: (exn -> Reader< ^e, ^a>) -> Reader< ^e, ^a>
            member inline TryFinally : body: Reader< ^e, ^a> * finalizer: (unit -> unit) -> Reader< ^e, ^a>

            member inline While : guard: (unit -> bool) * body: (unit -> Reader< ^e, unit>) -> Reader< ^e, unit>
            
            member inline For : seq: #seq< ^a> * body: (^a -> Reader< ^e, unit>) -> Reader< ^e, unit>


    /// <summary>Monadic workflow object.</summary>
    val reader : Workflow.ReaderBuilder


// Applicative

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    [<CompiledName("Ap")>]
    val inline ap : fv: Reader< ^e, ^a> -> ff: Reader< ^e, (^a -> ^b)> -> Reader< ^e, ^b>

    /// <summary>Lift a binary function onto contexts.</summary>
    [<CompiledName("Map2")>]
    val inline map2 : f: (^a -> ^b -> ^c) -> Reader< ^e, ^a> -> Reader< ^e, ^b> -> Reader< ^e, ^c>

    /// <summary>Sequence two contexts.</summary>
    [<CompiledName("AndThen")>]
    val inline andThen : second: Reader< ^e, ^b> -> first: Reader< ^e, ^a> -> Reader< ^e, ^b>

    /// <summary>Conditional execution of contextual expressions.</summary>
    [<CompiledName("When")>]
    val inline when_: condition: bool -> f: (unit -> Reader< ^e, unit>) -> Reader< ^e, unit>


// Functor

    /// <summary>Lift a function onto a context.</summary>
    [<CompiledName("Map")>]
    val inline map : f: (^a -> ^b) -> Reader< ^e, ^a> -> Reader< ^e, ^b>


// Profunctor

    /// <summary>Map over both arguments at the same time.</summary>
    [<CompiledName("Dimap")>]
    val inline dimap : f: (^a0 -> ^a) -> g: (^b -> ^c) -> pf: Reader< ^a, ^b> -> Reader< ^a0, ^c>

    /// <summary>Map the first argument contravariantly.</summary>
    [<CompiledName("LMap")>]
    val inline lmap : f: (^a0 -> ^a) -> pf: Reader< ^a, ^b> -> Reader< ^a0, ^b>    
    
    /// <summary>Map the second argument covariantly.</summary>
    [<CompiledName("RMap")>]
    val inline rmap : g: (^b -> ^c) -> pf: Reader< ^a, ^b> -> Reader< ^a, ^c>


// Semigroup

    /// <summary>An associative binary operation on contexts.</summary>
    val inline append : first: Reader< ^e, ^a> -> second: Reader< ^e, ^a> -> Reader< ^e, ^a>
        when ^a : (static member Append: ^a -> ^a -> ^a)


// Traversable

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("Sequence")>]
    val inline sequence : source: #seq<Reader< ^e, ^a>> -> Reader< ^e, seq< ^a>>

    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("Traverse")>]
    val inline traverse : f: (^a -> Reader< ^e, ^b>) -> source: #seq< ^a> -> Reader< ^e, seq< ^b>>


// Cat

    /// <summary>Identity element of a category.</summary>
    [<CompiledName("Identity")>]
    val identity<'a> : Reader< ^a, ^a>

    /// <summary>Compose two members of a category together.</summary>
    [<CompiledName("Compose")>]
    val inline compose : o2: Reader< ^b, ^c> -> o1: Reader< ^a, ^b> -> Reader< ^a, ^c>


// Arrow

    /// <summary>Lift a function to an arrow.</summary>
    [<CompiledName("Arr")>]
    val inline arr : f: (^a -> ^b) -> Reader< ^a, ^b>

    /// <summary>Send the first component of the input through the argument arrow, and copy the rest unchanged to the output.</summary>
    [<CompiledName("ArrFst")>]
    val inline arrFst : ar: Reader< ^a, ^b> -> Reader< ^a * ^c, ^b * ^c>

    /// <summary>Send the second component of the input through the argument arrow, and copy the rest unchanged to the output.</summary>
    [<CompiledName("ArrSnd")>]
    val inline arrSnd : ar: Reader< ^a, ^b> -> Reader< ^c * ^a, ^c * ^b>

    /// <summary>Split the input between the two argument arrows and combine their output.</summary>
    [<CompiledName("Split")>]
    val inline split : a2: Reader< ^c, ^d> -> a1: Reader< ^a, ^b> -> Reader< ^a * ^c, ^b * ^d>

    /// <summary>Fanout: send the input to both argument arrows and combine their output.</summary>
    [<CompiledName("Fanout")>]
    val inline fanout : a2: Reader< ^a, ^c> -> a1: Reader< ^a, ^b> -> Reader< ^a, ^b * ^c>


// Choice

    /// <summary>Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. A mirror of 'feed2'.</summary>
    [<CompiledName("Feed1")>]
    val inline feed1 : ar: Reader< ^a, ^b> -> Reader<Choice< ^a, ^c>, Choice< ^b, ^c>>

    /// <summary>Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. A mirror of 'feed1'.</summary>
    [<CompiledName("Feed2")>]
    val inline feed2 : ar: Reader< ^a, ^b> -> Reader<Choice< ^c, ^a>, Choice< ^c, ^b>>

    /// <summary>Split the input between the two argument arrows, retagging and merging their outputs.</summary>
    [<CompiledName("Merge")>]
    val inline merge : a2: Reader< ^c, ^d> -> a1: Reader< ^a, ^b> -> Reader<Choice< ^a, ^c>, Choice< ^b, ^d>>

    /// <summary>Split the input between the two argument arrows and merge their outputs.</summary>
    [<CompiledName("Fanin")>]
    val inline fanin : a2: Reader< ^c, ^b> -> a1: Reader< ^a, ^b> -> Reader<Choice< ^a, ^c>, ^b>


// Apply

    /// <summary>Arrow that allows application of arrow inputs to other inputs.</summary>
    [<CompiledName("App")>]
    val app<'a, 'b> : Reader<Reader< ^a, ^b> * ^a, ^b>