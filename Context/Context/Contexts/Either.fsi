namespace PTR.Context.Type.Either


/// <summary>The Either type represents values with two possibilities:
/// a value of type Either<A, B> is either 'Left L' or 'Right R'.</summary>
[<Struct>]
type Either<'L, 'R> = Left of L: 'L | Right of R: 'R with

    member inline Match : onLeft: System.Func< ^L, ^T> * onRight: System.Func< ^R , ^T> -> ^T

    static member inline Unit : x: ^a -> Either< ^e, ^a>
    
    member inline Select : f: System.Func< ^R, ^S> -> Either< ^L, ^S>
    member inline Select2 : second: Either< ^L, ^S> * f: System.Func< ^R, ^S, ^T> -> Either< ^L, ^T>        
    member inline SelectMany : f: System.Func< ^R, Either< ^L, ^S>> -> Either< ^L, ^S>
    member inline SelectMany : f: System.Func< ^R, Either< ^L, ^S>> * g: System.Func< ^R, ^S, ^T> -> Either< ^L, ^T>
    
    member inline Join : t: Either< ^L, ^S> * kt: System.Func< ^R, ^K> * ku: System.Func< ^S, ^K> * rs: System.Func< ^R, ^S, ^T> -> Either< ^L, ^T>

    member inline OrElse : second: Either< ^L, ^R> -> Either< ^L, ^R>

    static member inline Append : first: Either< ^e, ^a> * second: Either< ^e, ^a> -> Either< ^e, ^a>
        when ^a : (static member Append: ^a -> ^a -> ^a)


/// <summary>Operations on Either values.</summary>
module Either =

// Primitive

    /// <summary>Performs an action based on case-analysis of the given union-type.</summary>
    val inline case : onLeft: (^e -> ^r) -> onRight: (^a -> ^r) -> either: Either< ^e, ^a> -> ^r

    /// <summary>Return true if the given Either-value is a 'Left'.</summary>
    [<CompiledName("IsLeft")>]
    val isLeft : either: Either<'e, 'a> -> bool

    /// <summary>Return true if the given Either-value is a 'Right'.</summary>
    [<CompiledName("IsRight")>]
    val isRight : either: Either<'e, 'a> -> bool

    /// <summary>Return the contents of a Left-value or a default value otherwise.</summary>
    [<CompiledName("FromLeft")>]
    val fromLeft: def: 'a -> either: Either< ^a, 'b> -> ^a

    /// <summary>Return the contents of a Left-value or a default value otherwise.</summary>
    [<CompiledName("FromLeftWith")>]
    val inline fromLeftWith: defThunk: (unit -> ^a) -> either: Either< ^a, ^b> -> ^a

    /// <summary>Return the contents of a Right-value or a default value otherwise.</summary>
    [<CompiledName("FromRight")>]
    val fromRight: def: 'b -> either: Either<'a, ^b> -> ^b

    /// <summary>Return the contents of a Right-value or a default value otherwise.</summary>
    [<CompiledName("FromRightWith")>]
    val inline fromRightWith: defThunk: (unit -> ^b) -> either: Either< ^a, ^b> -> ^b


// Isomorphisms

    ///// <summary>Convert a Either-value into an array. a 'Right'-value will yield a singleton sequence while a 'Left'-value will yield an empty.</summary>
    //[<CompiledName("ToArray")>]
    //val inline toArray : either: Either< ^e, ^a> -> ^a []

    ///// <summary>Convert a Either-value into a list. a 'Right'-value will yield a singleton sequence while a 'Left'-value will yield an empty.</summary>
    //[<CompiledName("ToList")>]
    //val inline toList : either: Either< ^e, ^a> -> ^a list

    /// <summary>Convert a Either-value into a sequence. a 'Right'-value will yield a singleton sequence while a 'Left'-value will yield an empty sequence.</summary>
    [<CompiledName("ToSeq")>]
    val inline toSeq : either: Either< ^e, ^a> -> seq< ^a>
    
    /// <summary>Convert a Either-value into a Choice. A Right will yield a Choice1Of2 while a Left will yield a Choice2Of2.</summary>
    [<CompiledName("ToChoice")>]
    val inline toChoice : either: Either< ^e, ^a> -> Choice< ^a, ^e>

    /// <summary>Convert a Choice-value into an Either. A Choice1Of2 will yield a Right while a Choice2Of2 will yield a Left.</summary>
    [<CompiledName("OfChoice")>]
    val inline ofChoice : choice: Choice< ^a, ^e> -> Either< ^e, ^a>

    /// <summary>Convert a Either-value into a Result. A Right will yield an 'Ok' while a Left will yield an 'Error'.</summary>
    [<CompiledName("ToResult")>]
    val inline toResult : either: Either< ^e, ^a> -> Result< ^a, ^e>

    /// <summary>Convert a Result-value into an Either. An 'Ok' will yield a Right while an 'Error' will yield a Left.</summary>
    [<CompiledName("OfResult")>]
    val inline ofResult : result: Result< ^a, ^e> -> Either< ^e, ^a>


// Collections

    /// <summary>Extracts from a sequence of Eithers all the Left elements in order.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("Lefts")>]
    val lefts: eithers: #seq<Either<'a, 'b>> -> ^a seq

    /// <summary>Extracts from a sequence of Eithers all the Right elements in order.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("Rights")>]
    val rights: eithers: #seq<Either<'a, 'b>> -> ^b seq
  
    /// <summary>Partitions a list of Either into two sequences. All the Left elements are extracted, in order, to the first component of the output. Similarly the Right elements are extracted to the second component of the output.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("Partition")>]
    val partition: eithers: #seq<Either<'a, 'b>> -> ^a seq * ^b seq


// Monad

    /// <summary>Inject a value into the context type.</summary>
    val unit : x: 'a -> Either<'e, ^a>

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    [<CompiledName("Bind")>]
    val inline bind : f: (^a -> Either< ^e, ^b>) -> m: Either< ^e, ^a> -> Either< ^e, ^b>

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    [<CompiledName("Flatten")>]
    val flatten : mm: Either<'e, Either< ^e, 'a>> -> Either< ^e, ^a>

    /// <summary>Recursively generate a context using a continuation.</summary>
    [<CompiledName("RecM")>]
    val inline recM : f: ((^a -> Either< ^e, ^b>) -> ^a -> Either< ^e, ^b>) -> x: ^a -> Either< ^e, ^b>

    /// <summary>Recursively generate a context using a continuation.</summary>
    [<CompiledName("RecM1")>]
    val inline recM1 : f: ((Either< ^e, ^a> -> Either< ^e, ^b>) -> ^a -> Either< ^e, ^b>) -> x: ^a -> Either< ^e, ^b>


    /// <summary>Monadic workflow-related types and values.</summary>
    module Workflow =

        /// <summary>Monadic workflow builder.</summary>
        type EitherBuilder =
            new : unit -> EitherBuilder
            
            member inline Return : x: ^a -> Either< ^e, ^a>
            member inline ReturnFrom : m: Either< ^e, ^a> -> Either< ^e, ^a>
            member inline Bind: m: Either< ^e, ^a> * f: (^a -> Either< ^e, ^b>) -> Either< ^e, ^b>
            
            member inline Zero : unit -> Either< ^e, unit>

            member inline Using : disp: ^d * body: (^d -> Either< ^e, ^a>) -> Either< ^e, ^a> when ^d :> System.IDisposable

            member inline TryWith : body: Either< ^e, ^a> * handler: (exn -> Either< ^e, ^a>) -> Either< ^e, ^a>
            member inline TryFinally : body: Either< ^e, ^a> * finalizer: (unit -> unit) -> Either< ^e, ^a>

            member inline While : guard: (unit -> bool) * body: (unit -> Either< ^e, unit>) -> Either< ^e, unit>
            
            member inline For : seq: #seq< ^a> * body: (^a -> Either< ^e, unit>) -> Either< ^e, unit>


    /// <summary>Monadic workflow object.</summary>
    val either : Workflow.EitherBuilder


// Applicative

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    [<CompiledName("Ap")>]
    val inline ap : fv: Either< ^e, ^a> -> ff: Either< ^e, (^a -> ^b)> -> Either< ^e, ^b>

    /// <summary>Lift a binary function onto contexts.</summary>
    [<CompiledName("Map2")>]
    val inline map2 : f: (^a -> ^b -> ^c) -> fa: Either< ^e, ^a> -> fb: Either< ^e, ^b> -> Either< ^e, ^c>

    /// <summary>Sequence two contexts.</summary>
    [<CompiledName("AndThen")>]
    val andThen : second: Either<'e, 'b> -> first: Either< ^e, 'a> -> Either< ^e, ^b>

    /// <summary>Conditional execution of contextual expressions.</summary>
    [<CompiledName("When")>]
    val inline when_: condition: bool -> f: (unit -> Either< ^e, unit>) -> Either< ^e, unit>


// Alternative

    /// <summary>An associative binary operation on contexts providing for choice and failure.</summary>
    val orElse : second: Either<'e, 'a> -> first: Either< ^e, ^a> -> Either< ^e, ^a>

    /// <summary>An associative binary operation on contexts providing for choice and failure.</summary>
    val inline orElseWith : second: (unit -> Either< ^e, ^a>) -> first: Either< ^e, ^a> -> Either< ^e, ^a>


// Functor

    /// <summary>Lift a function onto a context.</summary>
    [<CompiledName("Map")>]
    val inline map : f: (^a -> ^b) -> fa: Either< ^e, ^a> -> Either< ^e, ^b>


// Bifunctor

    /// <summary>Map over both arguments at the same time.</summary>
    [<CompiledName("Bimap")>]
    val inline bimap: f: (^a -> ^c) -> g: (^b -> ^d) -> bf: Either< ^a, ^b> -> Either< ^c, ^d>

    /// <summary>Map covariantly over the first argument.</summary>
    [<CompiledName("MapFst")>]
    val inline mapFst: f: (^a -> ^c) -> bf: Either< ^a, ^b> -> Either< ^c, ^b>

    /// <summary>Map covariantly over the second argument.</summary>
    [<CompiledName("MapSnd")>]
    val inline mapSnd: g: (^b -> ^c) -> bf: Either< ^a, ^b> -> Either< ^a, ^c>


// Semigroup

    /// <summary>An associative binary operation on contexts.</summary>
    val append : first: Either<'e, 'a> -> second: Either< ^e, ^a> -> Either< ^e, ^a>


// Foldable

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    [<CompiledName("Fold")>]
    val inline fold : folder: (^s -> ^a -> ^s) -> seed: ^s -> source: Either< ^e, ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    [<CompiledName("FoldBack")>]
    val inline foldBack : folder: (^a -> ^s -> ^s) -> seed: ^s -> source: Either< ^e, ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation. The accumulator is a thunk that is only called as needed by the folding function.</summary>
    [<CompiledName("Foldl")>]
    val inline foldl : folder: ((unit -> ^s) -> ^a -> ^s) -> seed: (unit -> ^s) -> source: Either< ^e, ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation. The accumulator is a thunk that is only called as needed by the folding function.</summary>
    [<CompiledName("Foldr")>]
    val inline foldr : folder: (^a -> (unit -> ^s) -> ^s) -> seed: (unit -> ^s) -> source: Either< ^e, ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation. The accumulator type is a monoid, and the folding function is the 'append' function for the given type. The default 'seed' is the monoid's 'empty' value.</summary>
    [<CompiledName("Foldm")>]
    val inline foldm : f: (^a -> ^m) -> source: Either< ^e, ^a> -> ^m
        when ^m : (static member Append: ^m -> ^m -> ^m)
        and  ^m : (static member Empty: unit -> ^m)

    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
    [<CompiledName("MapFold")>]
    val inline mapFold : mapping: (^s -> ^a -> ^b * ^s) -> seed: ^s -> source: Either< ^e, ^a> -> Either< ^e, ^b> * ^s

    /// <summary>Combines the functionality of map and foldBack, returning the pair of the final context-value and state.</summary
    [<CompiledName("MapFoldBack")>]
    val inline mapFoldBack : mapping: (^a -> ^s -> ^b * ^s) -> seed: ^s -> source: Either< ^e, ^a> -> Either< ^e, ^b> * ^s


// Bifoldable

    /// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s).</summary>
    [<CompiledName("Bifold")>]
    val inline bifold : fold1: (^s -> ^a -> ^s) -> fold2: (^s -> ^b -> ^s) -> seed: ^s -> source: Either< ^a, ^b> -> ^s

    /// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s).</summary>
    [<CompiledName("BifoldBack")>]
    val inline bifoldBack : fold1: (^a -> ^s -> ^s) -> fold2: (^b -> ^s -> ^s) -> seed: ^s -> source: Either< ^a, ^b> -> ^s

    /// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s). The accumulator is a thunk that is only called as needed by the folding function.</summary>
    [<CompiledName("Bifoldl")>]
    val inline bifoldl : fold1: ((unit -> ^s) -> ^a -> ^s) -> fold2: ((unit -> ^s) -> ^b -> ^s) -> seed: (unit -> ^s) -> source: Either< ^a, ^b> -> ^s

    /// <summary>Applies a function to all element(s) of up to two possible sources, threading an accumulator argument through the computation(s). The accumulator is a thunk that is only called as needed by the folding function.</summary>
    [<CompiledName("Bifoldr")>]
    val inline bifoldr : fold1: (^a -> (unit -> ^s) -> ^s) -> fold2: (^b -> (unit -> ^s) -> ^s) -> seed: (unit -> ^s) -> source: Either< ^a, ^b> -> ^s

    /// <summary>Applies a function to all element(s) of up to two possible source(s), threading an accumulator argument through the computation. The accumulator type is a monoid, and the folding function is the 'append' function for the given type. The default 'seed' is the monoid's 'empty' value.</summary>
    [<CompiledName("Bifoldm")>]
    val inline bifoldm : f1: (^a -> ^m) -> f2: (^b -> ^m) -> source: Either< ^a, ^b> -> ^m
        when ^m : (static member Append: ^m -> ^m -> ^m)

    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
    [<CompiledName("BimapFold")>]
    val inline bimapFold : mapping1: (^s -> ^a -> ^b * ^s) -> mapping2: (^s -> ^c -> ^d * ^s) -> seed: ^s -> source: Either< ^a, ^c> -> Either< ^b, ^d> * ^s

    /// <summary>Combines the functionality of map and foldBack, returning the pair of the final context-value and state.</summary
    [<CompiledName("BimapFoldBack")>]
    val inline bimapFoldBack : mapping1: (^a -> ^s -> ^b * ^s) -> mapping2: (^c -> ^s -> ^d * ^s) -> seed: ^s -> source: Either< ^a, ^c> -> Either< ^b, ^d> * ^s


// Traversable

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("Sequence")>]
    val inline sequence : source: #seq<Either< ^e, ^a>> -> Either< ^e, seq< ^a>>

    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    [<CompiledName("Traverse")>]
    val inline traverse : f: (^a -> Either< ^e, ^b>) -> source: #seq< ^a> -> Either< ^e, seq< ^b>>