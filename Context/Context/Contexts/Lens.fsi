namespace PTR.Context.Type.Lens


/// <summary>Acts like a property of an object that can be accessed through a `getter` and/or `setter`.</summary>
[<Struct; NoComparison; NoEquality>]
type Lens<'P, 'V> = { Get: ('P -> 'V) ; Set: ('P -> 'V -> 'P) } with

    member inline Invoke : property: ^P -> ^P

    member inline With : get: System.Func< ^P, ^V> -> Lens< ^P, ^V>
    member inline With : set: System.Func< ^P, ^V, ^P> -> Lens< ^P, ^V>

    static member inline Unit : x: ^a -> Lens< ^p, ^a>
                
    member inline Select : f: System.Func< ^V, ^W> -> Lens< ^P, ^W>
    member inline Select2 : second: Lens< ^P, ^W> * f: System.Func< ^V, ^W, ^X> -> Lens< ^P, ^X>
    member inline SelectMany : f: System.Func< ^V, Lens< ^P, ^W>> -> Lens< ^P, ^W>
    member inline SelectMany : f: System.Func< ^V, Lens< ^P, ^W>> * g: System.Func< ^V, ^W, ^X> -> Lens< ^P, ^X>

    member inline Join : t: Lens< ^P, ^W> * kt: System.Func< ^V, ^K> * ku: System.Func< ^W, ^K> * rs: System.Func< ^V, ^W, ^X> -> Lens< ^P, ^X>

    static member inline Append : first: Lens< ^p, ^a> * second: Lens< ^p, ^a> -> Lens< ^p, ^a>
        when ^a : (static member Append: ^a -> ^a -> ^a)
    

/// <summary>Operations on Lenses.</summary>
module Lens =

    /// <summary>Active patterns on Lens values.</summary>
    module Pattern =
  
        /// <summary>Active pattern that can be used to extract both the "getter" and "setter" within a Lens.</summary>
        val inline ( |Lens| ) : lens: Lens< ^p, ^v> -> Lens< ^p, ^v>
  
        /// <summary>Active pattern that can be used to extract the "getter" within a Lens.</summary>
        val inline ( |LensGet| ) : lens: Lens< ^p, ^v> -> (^p -> ^v)
  
        /// <summary>Active pattern that can be used to extract the "setter" within a Lens.</summary>
        val inline ( |LensSet| ) : lens: Lens< ^p, ^v> -> (^p -> ^v -> ^p)    

  
    ////// <summary>Functions between Lenses and collections.</summary>
    ///[<RequireQualifiedAccess>]
    ///module Lenses =
    ///  
        /// <summary>Functions between Lenses and Maps.</summary>
        ///[<RequireQualifiedAccess>]
        ///module Map =  
            /// <summary>'Get' attempts to retrieve a value from a Map given a key.
            ///
            /// 'Set' attempts to add a value to a Map.</summary>
            ///[<CompiledName("MemberL")>]
            ///val inline memberL : key: ^k -> Lens<Map< ^k, ^v>, ^v option>
            ////
            ////
            /////// <summary>Stateful Lens functions.</summary>
            ////[<RequireQualifiedAccess>]
            ////module State =
            ////
            ////    /// <summary>'Stateful' version of setL -- when the 'state' value is left out, the function takes the form of 'state -> state'.</summary>
            ////    val inline modifyL : lens: Lens< ^p, ^s> -> property: ^p -> state: ^s -> ^s
            ////
            ////    /// <summary>Use a Lens as a stateful function.</summary>
            ////    val inline putL : lens: Lens< ^s, ^p> -> state: ^s -> ^s
            ////
            ////    /// <summary>Convert a stateful function of one state-type to another state-type.</summary>
            ////    val inline focus : f: (^s -> ^s * ^a) -> lens: Lens< ^s, ^p> -> property: ^p -> ^p * ^a


// Primitives

    /// <summary>Create a Lens given an accessor function and a setter function.</summary>
    [<CompiledName("Make")>]
    val inline make : getter: System.Func< ^p, ^v> -> setter: System.Func< ^p, ^v, ^p> -> Lens< ^p, ^v>

    /// <summary>Create a Lens given an accessor function and a setter function.</summary>
    val inline newLens : getter: (^p -> ^v) -> setter: (^p -> ^v -> ^p) -> Lens< ^p, ^v>
  
    /// <summary>Run a Lens by applying a `getter` to the given value, then apply both to the `setter` to retrieve a final value.</summary>
    val inline runLens : property: ^p -> lens: Lens< ^p, ^v> -> ^p

    /// <summary>Retrieve the "getter" from a Lens.</summary>
    val inline getL : lens: Lens< ^p, ^v> -> (^p -> ^v)

    /// <summary>Retrieve the "setter" from a Lens.</summary>
    val inline setL : lens: Lens< ^p, ^v> -> (^p -> ^v -> ^p)

    /// <summary>Retrieve a value with a 'getter', apply a function to it, then apply a 'setter' to the result and the initial value.</summary>
    [<CompiledName("ModL")>]
    val inline modL : f: (^v -> ^v) -> property: ^p -> lens: Lens< ^p, ^v> -> ^p

    /// <summary>Lens' equivalent of the standard 'fst' function on tuples.</summary>
    [<CompiledName("FstL")>]
    val fstL<'a, 'b> : Lens< ^a * ^b, ^a>

    /// <summary>Lens' equivalent of the standard 'snd' function on tuples.</summary>
    [<CompiledName("SndL")>]
    val sndL<'a, 'b> : Lens< ^a * ^b, ^b>

    /// <summary>Caches both the 'getter' and 'setter' functions inside a Lens.</summary>
    [<CompiledName("CacheLens")>]
    val inline cacheLens : lens: Lens< ^p, ^v> -> Lens< ^p, ^v>
        when ^p : equality and ^v : equality


// Monad

    /// <summary>Inject a value into the context type.</summary>
    val inline unit : x: ^a -> Lens< ^p, ^a>

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    [<CompiledName("Bind")>]
    val inline bind : f: (^a -> Lens< ^p, ^b>) -> m: Lens< ^p, ^a> -> Lens< ^p, ^b>

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    [<CompiledName("Flatten")>]
    val inline flatten : mm: Lens< ^p, Lens< ^p, ^a>> -> Lens< ^p, ^a>

    /// <summary>Recursively generate a context using a continuation.</summary>
    [<CompiledName("RecM")>]
    val inline recM : f: ((^a -> Lens< ^p, ^b>) -> ^a -> Lens< ^p, ^b>) -> x: ^a -> Lens< ^p, ^b>

    /// <summary>Recursively generate a context using a continuation.</summary>
    [<CompiledName("RecM1")>]
    val inline recM1 : f: ((Lens< ^p, ^a> -> Lens< ^p, ^b>) -> ^a -> Lens< ^p, ^b>) -> x: ^a -> Lens< ^p, ^b>


    /// <summary>Monadic workflow-related types and values.</summary>
    module Workflow =

        /// <summary>Monadic workflow builder.</summary>
        type LensBuilder =
            new : unit -> LensBuilder
            
            member inline Return : x: ^a -> Lens< ^p, ^a>
            member inline ReturnFrom : m: Lens< ^p, ^a> -> Lens< ^p, ^a>
            member inline Bind: m: Lens< ^p, ^a> * f: (^a -> Lens< ^p, ^b>) -> Lens< ^p, ^b>
            
            member inline Zero : unit -> Lens< ^p, unit>

            member inline Using : disp: ^d * body: (^d -> Lens< ^p, ^a>) -> Lens< ^p, ^a> when ^d :> System.IDisposable

            member inline TryWith : body: Lens< ^p, ^a> * handler: (exn -> Lens< ^p, ^a>) -> Lens< ^p, ^a>
            member inline TryFinally : body: Lens< ^p, ^a> * finalizer: (unit -> unit) -> Lens< ^p, ^a>

            member inline While : guard: (unit -> bool) * body: (unit -> Lens< ^p, unit>) -> Lens< ^p, unit>
            
            member inline For : seq: #seq< ^a> * body: (^a -> Lens< ^p, unit>) -> Lens< ^p, unit>


    /// <summary>Monadic workflow object.</summary>
    val lens : Workflow.LensBuilder


// Applicative

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    [<CompiledName("Ap")>]
    val inline ap : fv: Lens< ^p, ^a> -> ff: Lens< ^p, (^a -> ^b)> -> Lens< ^p, ^b>

    /// <summary>Lift a binary function onto contexts.</summary>
    [<CompiledName("Map2")>]
    val inline map2 : f: (^a -> ^b -> ^c) -> fa: Lens< ^p, ^a> -> fb: Lens< ^p, ^b> -> Lens< ^p, ^c>

    /// <summary>Sequence two contexts.</summary>
    [<CompiledName("AndThen")>]
    val inline andThen : second: Lens< ^p, ^b> -> first: Lens< ^p, ^a> -> Lens< ^p, ^b>

    /// <summary>Conditional execution of contextual expressions.</summary>
    [<CompiledName("When")>]
    val inline when_: condition: bool -> f: (unit -> Lens< ^p, unit>) -> Lens< ^p, unit>


// Functor

    /// <summary>Lift a function onto a context.</summary>
    [<CompiledName("Map")>]
    val inline map : f: (^a -> ^b) -> fa: Lens< ^p, ^a> -> Lens< ^p, ^b>


// Profunctor

    /// <summary>Map over both arguments at the same time.</summary>
    [<CompiledName("Dimap")>]
    val inline dimap : f: (^c -> ^a) -> g: (^b -> ^d) -> pf: Lens< ^a, ^b> -> Lens< ^c, ^d>

    /// <summary>Map the first argument contravariantly.</summary>
    [<CompiledName("LMap")>]
    val inline lmap : f: (^c -> ^a) -> pf: Lens< ^a, ^b> -> Lens< ^c, ^b>    
    
    /// <summary>Map the second argument covariantly.</summary>
    [<CompiledName("RMap")>]
    val inline rmap : g: (^b -> ^c) -> pf: Lens< ^a, ^b> -> Lens< ^a, ^c>


// Semigroup

    /// <summary>An associative binary operation on contexts.</summary>
    val inline append : first: Lens< ^p, ^a> -> second: Lens< ^p, ^a> -> Lens< ^p, ^a>
        when ^a : (static member Append: ^a -> ^a -> ^a)


// Traversable

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("Sequence")>]
    val inline sequence : source: seq<Lens< ^p, ^a>> -> Lens< ^p, seq< ^a>>

    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("Traverse")>]
    val inline traverse : f: (^a -> Lens< ^p, ^b>) -> source: seq< ^a> -> Lens< ^p, seq< ^b>>


// Cat

    /// <summary>Identity element of a category.</summary>
    [<CompiledName("Identity")>]
    val identity<'a> : Lens< ^a, ^a>

    /// <summary>Compose two members of a category together.</summary>
    [<CompiledName("Compose")>]
    val inline compose : o2: Lens< ^b, ^c> -> o1: Lens< ^a, ^b> -> Lens< ^a, ^c>


// Arrow

    /// <summary>Lift a function to an arrow.</summary>
    [<CompiledName("Arr")>]
    val inline arr : f: (^a -> ^b) -> Lens< ^a, ^b>

    /// <summary>Send the first component of the input through the argument arrow, and copy the rest unchanged to the output.</summary>
    [<CompiledName("ArrFst")>]
    val inline arrFst : arrow: Lens< ^a, ^b> -> Lens< ^a * ^c, ^b * ^c>

    /// <summary>Send the second component of the input through the argument arrow, and copy the rest unchanged to the output.</summary>
    [<CompiledName("ArrSnd")>]
    val inline arrSnd : arrow: Lens< ^a, ^b> -> Lens< ^c * ^a, ^c * ^b>

    /// <summary>Split the input between the two argument arrows and combine their output.</summary>
    [<CompiledName("Split")>]
    val inline split : a2: Lens< ^c, ^d> -> a1: Lens< ^a, ^b> -> Lens< ^a * ^c, ^b * ^d>

    /// <summary>Fanout: send the input to both argument arrows and combine their output.</summary>
    [<CompiledName("Fanout")>]
    val inline fanout : a2: Lens< ^a, ^c> -> a1: Lens< ^a, ^b> -> Lens< ^a, ^b * ^c>


// Choice

    /// <summary>Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. A mirror of 'feed2'.</summary>
    [<CompiledName("Feed1")>]
    val inline feed1 : arrow: Lens< ^a, ^b> -> Lens<Choice< ^a, ^c>, Choice< ^b, ^c>>

    /// <summary>Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. A mirror of 'feed1'.</summary>
    [<CompiledName("Feed2")>]
    val inline feed2 : arrow: Lens< ^a, ^b> -> Lens<Choice< ^c, ^a>, Choice< ^c, ^b>>

    /// <summary>Split the input between the two argument arrows, retagging and merging their outputs.</summary>
    [<CompiledName("Merge")>]
    val inline merge : a2: Lens< ^c, ^d> -> a1: Lens< ^a, ^b> -> Lens<Choice< ^a, ^c>, Choice< ^b, ^d>>

    /// <summary>Split the input between the two argument arrows and merge their outputs.</summary>
    [<CompiledName("Fanin")>]
    val inline fanin : a2: Lens< ^c, ^b> -> a1: Lens< ^a, ^b> -> Lens<Choice< ^a, ^c>, ^b>


// Apply

    /// <summary>Arrow that allows application of arrow inputs to other inputs.</summary>
    [<CompiledName("App")>]
    val app<'a, 'b> : Lens<Lens< ^a, ^b> * ^a, ^b>