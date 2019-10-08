namespace PTR.Context.Type.Maybe


/// <summary>The Maybe type encapsulates an optional value.
/// A value of type Maybe a either contains a value of type a (represented as Just a),
/// or it is empty (represented as Nothing).
/// Using Maybe is a good way to deal with cases where a valid result 'may' not exist.</summary>
[<Struct>]
type Maybe<'T> = Nothing | Just of 'T with

    member inline Match : onNothing: System.Func< ^U> * onJust: System.Func< ^T, ^U> -> ^U

    static member inline Unit : x: ^a -> Maybe< ^a>
    
    member inline Select : f: System.Func< ^T, ^U> -> Maybe< ^U>
    member inline Select2 : second: Maybe< ^U> * f: System.Func< ^T, ^U, ^V> -> Maybe< ^V>
    member inline SelectMany : f: System.Func< ^T, Maybe< ^U>> -> Maybe< ^U>
    member inline SelectMany : f: System.Func< ^T, Maybe< ^U>> * g: System.Func< ^T, ^U, ^V> -> Maybe< ^V>
    
    member inline Join : t: Maybe< ^U> * kt: System.Func< ^T, ^K> * ku: System.Func< ^U, ^K> * rs: System.Func< ^T, ^U, ^V> -> Maybe< ^V> when ^K : equality

    member inline Where : p: System.Func< ^T, bool> -> Maybe< ^T>    

    member inline OrElse : second: Maybe< ^T> -> Maybe< ^T>
    
    static member inline Append : first: Maybe< ^a> * second: Maybe< ^a> -> Maybe< ^a>
    static member inline Empty : unit -> Maybe< ^a>


/// <summary>Operations on Maybe values.</summary>
module Maybe =

// Primitive

    /// <summary>Performs an action based on case-analysis of the given union-type.</summary>
    val inline case : onNothing: (unit -> ^b) -> onJust: (^a -> ^b) -> maybe: Maybe< ^a> -> ^b

    /// <summary>Return true if the given Maybe-value is a 'Just'.</summary>
    [<CompiledName("IsJust")>]
    val isJust : maybe: Maybe<'T> -> bool

    /// <summary>Return true if the given Maybe-value is a 'Nothing'.</summary>
    [<CompiledName("IsNothing")>]
    val isNothing : maybe: Maybe<'T> -> bool

    /// <summary>If the given Maybe-value is a 'Just', return its value with the given function applied to it, otherwise return the default.</summary>
    [<CompiledName("FromMaybe")>]
    val inline fromMaybe : def: ^b -> f: (^a -> ^b) -> maybe: Maybe< ^a> -> ^b

    /// <summary>If the given Maybe-value is a 'Just', return its value with the given function applied to it, otherwise return the default.</summary>
    [<CompiledName("FromMaybeWith")>]
    val inline fromMaybeWith : defThunk: (unit -> ^b) -> f: (^a -> ^b) -> maybe: Maybe< ^a> -> ^b

    /// <summary>If the given Maybe-value is a 'Just', return its value, otherwise return the default.</summary>
    [<CompiledName("DefaultValue")>]
    val defaultValue : def: 'a -> maybe: Maybe< ^a> -> ^a

    /// <summary>If the given Maybe-value is a 'Just', return its value, otherwise return the default.</summary>
    [<CompiledName("DefaultWith")>]
    val inline defaultWith : defThunk: (unit -> ^a) -> maybe: Maybe< ^a> -> ^a

    /// <summary>Filters a sequence of Maybes, and returns the 'x' values from all 'Just(x)'s.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("Justs")>]
    val justs : maybes: #seq<Maybe<'a>> -> seq< ^a>


// Isomorphisms

    ///// <summary>Convert a Maybe-value into an array. a 'Just'-value will yield a singleton sequence while a 'Nothing'-value will yield an empty.</summary>
    //[<CompiledName("ToArray")>]
    //val inline toArray : maybe: Maybe< ^a> -> ^a []

    ///// <summary>Convert a Maybe-value into a list. a 'Just'-value will yield a singleton sequence while a 'Nothing'-value will yield an empty.</summary>
    //[<CompiledName("ToList")>]
    //val inline toList : maybe: Maybe< ^a> -> ^a list

    /// <summary>Convert a Maybe-value into a sequence. a 'Just'-value will yield a singleton sequence while a 'Nothing'-value will yield an empty sequence.</summary>
    [<CompiledName("ToSeq")>]
    val inline toSeq : maybe: Maybe< ^a> -> seq< ^a>

    /// <summary>Convert a Maybe-value into an option. A Just will yield a Some while a Nothing will yield a None.</summary>
    [<CompiledName("ToOption")>]
    val inline toOption : maybe: Maybe< ^a> -> Option< ^a>

    /// <summary>Convert an Option-value into a Maybe. A Some will yield a Just while a None will yield a Nothing.</summary>
    [<CompiledName("OfOption")>]
    val inline ofOption : option: Option< ^a> -> Maybe< ^a>

    /// <summary>Convert a Maybe-value into an option. A Just will yield a Some while a Nothing will yield a None.</summary>
    [<CompiledName("ToVOption")>]
    val inline toVOption : maybe: Maybe< ^a> -> ValueOption< ^a>

    /// <summary>Convert an Option-value into a Maybe. A Some will yield a Just while a None will yield a Nothing.</summary>
    [<CompiledName("OfVOption")>]
    val inline ofVOption : voption: ValueOption< ^a> -> Maybe< ^a>

    /// <summary>Convert a Maybe into a Nullable-value.</summary>
    [<CompiledName("ToNullable")>]
    val inline toNullable : maybe: Maybe< ^a> -> System.Nullable< ^a>

    /// <summary>Convert a Nullable-value into a Maybe.</summary>
    [<CompiledName("OfNullable")>]
    val inline ofNullable : nullable: System.Nullable< ^a> -> Maybe< ^a>

    /// <summary>Convert an object (or any nullable-type) into a Maybe. A 'null' value will yield a Nothing.</summary>
    [<CompiledName("OfObj")>]
    val inline ofObj : obj: ^a -> Maybe< ^a> when ^a : null


// Monad

    /// <summary>Inject a value into the context type.</summary>
    val unit : x: 'a -> Maybe< ^a>

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    [<CompiledName("Bind")>]
    val inline bind : f: (^a -> Maybe< ^b>) -> m: Maybe< ^a> -> Maybe< ^b>

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    [<CompiledName("Flatten")>]
    val flatten : mm: Maybe<Maybe<'a>> -> Maybe< ^a>

    /// <summary>Recursively generate a context using a continuation.</summary>
    [<CompiledName("RecM")>]
    val inline recM : f: ((^a -> Maybe< ^b>) -> ^a -> Maybe< ^b>) -> x: ^a -> Maybe< ^b>

    /// <summary>Recursively generate a context using a continuation.</summary>
    [<CompiledName("RecM1")>]
    val inline recM1 : f: ((Maybe< ^a> -> Maybe< ^b>) -> ^a -> Maybe< ^b>) -> x: ^a -> Maybe< ^b>


    /// <summary>Monadic workflow-related types and values.</summary>
    module Workflow =

        /// <summary>Monadic workflow builder.</summary>
        type MaybeBuilder =
            new : unit -> MaybeBuilder
            
            member inline Return : x: ^a -> Maybe< ^a>
            member inline ReturnFrom : m: Maybe< ^a> -> Maybe< ^a>
            member inline Bind: m: Maybe< ^a> * f: (^a -> Maybe< ^b>) -> Maybe< ^b>

            member inline Zero : unit -> Maybe<unit>

            member inline Using : d: ^d * body: (^d -> Maybe< ^a>) -> Maybe< ^a> when ^d :> System.IDisposable

            member inline TryWith : body: Maybe< ^a> * handler: (exn -> Maybe< ^a>) -> Maybe< ^a>
            member inline TryFinally : body: Maybe< ^a> * finalizer: (unit -> unit) -> Maybe< ^a>

            member inline While : guard: (unit -> bool) * body: (unit -> Maybe<unit>) -> Maybe<unit>
            
            member inline For : seq: #seq< ^a> * body: (^a -> Maybe<unit>) -> Maybe<unit>


    /// <summary>Monadic workflow object.</summary>
    val maybe : Workflow.MaybeBuilder


// MonadPlus

    /// <summary>An empty context.</summary>
    val empty<'a> : Maybe<'a>

    /// <summary>Conditional blocking of contextual computations.</summary>
    [<CompiledName("Guard")>]
    val guard: condition: bool -> Maybe<unit>

    /// <summary>Combine two monads using a 'SQL style' inner join function.</summary>
    val inline join: joiner: (^a -> ^b -> ^c) -> k1: (^a -> ^k) -> k2: (^b -> ^k) -> Maybe< ^a> -> Maybe< ^b> -> Maybe< ^c> when ^k : equality

    /// <summary>Combine two monads using a function similar to a 'SQL style' inner join function.</summary>
    val inline joinBy : joiner: (^a -> ^b -> ^c) -> matcher: (^a -> ^b -> bool) -> ma: Maybe< ^a> -> mb: Maybe< ^b> -> Maybe< ^c>


// General

    /// <summary>Filter out element(s) that do not satisfy the predicate.</summary>
    [<CompiledName("Filter")>]
    val inline filter : p: (^a -> bool) -> Maybe< ^a> -> Maybe< ^a>


// Applicative

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    [<CompiledName("Ap")>]
    val inline ap : fv: Maybe< ^a> -> ff: Maybe<(^a -> ^b)> -> Maybe< ^b>

    /// <summary>Lift a binary function onto contexts.</summary>
    [<CompiledName("Map2")>]
    val inline map2 : f: (^a -> ^b -> ^c) -> fa: Maybe< ^a> -> fb: Maybe< ^b> -> Maybe< ^c>

    /// <summary>Sequence two contexts.</summary>
    [<CompiledName("AndThen")>]
    val andThen : second: Maybe<'b> -> first: Maybe<'a> -> Maybe< ^b>

    /// <summary>Conditional execution of contextual expressions.</summary>
    [<CompiledName("When")>]
    val inline when_: condition: bool -> f: (unit -> Maybe<unit>) -> Maybe<unit>


// Alternative

    /// <summary>An associative binary operation on contexts providing for choice and failure.</summary>
    val orElse : second: Maybe<'a> -> first: Maybe< ^a> -> Maybe< ^a>

    /// <summary>An associative binary operation on contexts providing for choice and failure.</summary>
    val inline orElseWith : second: (unit -> Maybe< ^a>) -> first: Maybe< ^a> -> Maybe< ^a>


// Functor

    /// <summary>Lift a function onto a context.</summary>
    [<CompiledName("Map")>]
    val inline map : f: (^a -> ^b) -> fa: Maybe< ^a> -> Maybe< ^b>


// Semigroup

    /// <summary>An associative binary operation on contexts.</summary>
    val append : first: Maybe<'a> -> second: Maybe< ^a> -> Maybe< ^a>

// Monoid

    /// <summary>Repeat an operation on a contect a specified number of times.</summary>
    [<CompiledName("MTimes")>]
    val mtimes : count: int -> elem: Maybe<'a> -> Maybe< ^a>

    /// <summary>Combine elements of a contextual sequence using monoidal composition.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("MConcat")>]
    val inline mconcat : source: #seq<Maybe< ^a>> -> Maybe< ^a>


// Traversable

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("Sequence")>]
    val inline sequence : source: #seq<Maybe< ^a>> -> Maybe<seq< ^a>>

    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("Traverse")>]
    val inline traverse : f: (^a -> Maybe< ^b>) -> source: #seq< ^a> -> Maybe<seq< ^b>>


// Foldable

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    [<CompiledName("Fold")>]
    val inline fold : folder: (^s -> ^a -> ^s) -> seed: ^s -> source: Maybe< ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    [<CompiledName("FoldBack")>]
    val inline foldBack : folder: (^a -> ^s -> ^s) -> seed: ^s -> source: Maybe< ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation. The accumulator is a thunk that is only called as needed by the folding function.</summary>
    [<CompiledName("Foldl")>]
    val inline foldl : folder: ((unit -> ^s) -> ^a -> ^s) -> seed: (unit -> ^s) -> source: Maybe< ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation. The accumulator is a thunk that is only called as needed by the folding function.</summary>
    [<CompiledName("Foldr")>]
    val inline foldr : folder: (^a -> (unit -> ^s) -> ^s) -> seed: (unit -> ^s) -> source: Maybe< ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation. The accumulator type is a monoid, and the folding function is the 'append' function for the given type. The default 'seed' is the monoid's 'empty' value.</summary>
    [<CompiledName("Foldm")>]
    val inline foldm : f: (^a -> ^m) -> source: Maybe< ^a> -> ^m
        when ^m : (static member Append: ^m -> ^m -> ^m)
        and  ^m : (static member Empty: unit -> ^m)

    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
    [<CompiledName("MapFold")>]
    val inline mapFold : mapping: (^s -> ^a -> ^b * ^s) -> seed: ^s -> source: Maybe< ^a> -> Maybe< ^b> * ^s

    /// <summary>Combines the functionality of map and foldBack, returning the pair of the final context-value and state.</summary
    [<CompiledName("MapFoldBack")>]
    val inline mapFoldBack : mapping: (^a -> ^s -> ^b * ^s) -> seed: ^s -> source: Maybe< ^a> -> Maybe< ^b> * ^s