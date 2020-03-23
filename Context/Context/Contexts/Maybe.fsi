namespace Rogz.Context.Data.Maybe


/// <summary>Operations on Maybe</summary>
module Maybe =

// Primitives

    /// <summary>Acts as an inline pattern-match on a union-type, calling the appropriate function based on the case.</summary>
    val inline caseof: onNothing: (unit -> ^b) -> onJust: (^a -> ^b) -> Maybe< ^a> -> ^b

    /// <summary>Returns true if the value is a 'Just'; false otherwise.</summary>
    val isJust: maybe: Maybe<'a> -> bool

    /// <summary>Returns true if the value is a 'Nothing'; false otherwise.</summary>
    val isNothing: maybe: Maybe<'a> -> bool

    /// <summary>Returns all values held within 'Just'-values, and removes all others.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the input sequence is null.</exception>
    val justs: source: Maybe<'a> seq -> ^a seq

    /// <summary>Map a 'Maybe'-producing function across a sequence and return only the 'Just'-values.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline mapMaybes: f: (^a -> Maybe< ^b>) -> source: ^a seq -> ^b seq


// Isomorphisms

    ///// <summary>Returns a singleton sequence if the given Maybe is a Just-value, containing that value, otherwise returns an empty sequence.</summary>
    //val toSeq: maybe: Maybe<'a> -> ^a seq

    /// <summary>Returns Nothing if the given object is null, otherwise returns the object wrapped in a 'Just'.</summary>
    val inline ofObj: obj: ^a -> Maybe< ^a> when ^a : null

    /// <summary>Convert a Nullable to a Maybe.</summary>
    val ofNullable: nullable: System.Nullable<'a> -> Maybe< ^a>

    /// <summary>Convert a Maybe to a Nullable.</summary>
    val toNullable: maybe: Maybe<'a> -> System.Nullable< ^a>

    ///// <summary>Convert an Option into a Maybe.</summary>
    //val ofOption: option: Option<'a>-> Maybe< ^a>

    ///// <summary>Convert a Maybe into an Option.</summary>
    //val toOption: maybe: Maybe<'a> -> Option< ^a>

    ///// <summary>Convert a ValueOption into a Maybe.</summary>
    //val ofVOption: voption: ValueOption<'a> -> Maybe< ^a>

    ///// <summary>Convert a Maybe into a ValueOption.</summary>
    //val toVOption: maybe: Maybe<'a> -> ValueOption< ^a>


//// Recur

//    /// <summary>Perform a context-based loop using the given seed until some 'exit' condition is reached, and return the final result.</summary>
//    val inline recur: loop: (^a -> Maybe< ^a>) -> seed: ^a -> ^a


// Functor

    /// <summary>Lift a function onto a context.</summary>
    val inline map: f: (^a -> ^b) -> fa: Maybe< ^a> -> Maybe< ^b>


// Applicative

    /// <summary>Lift a value into a context.</summary>
    val unit: value: 'a -> Maybe< ^a>

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    val inline ap: fv: Maybe< ^a> -> ff: Maybe<(^a -> ^b)> -> Maybe< ^b>

    /// <summary>Lift a binary function onto contexts.</summary>
    val inline map2: f: (^a -> ^b -> ^c) -> fa: Maybe< ^a> -> fb: Maybe< ^b> -> Maybe< ^c>

    ///// <summary>Sequence two contexts, discarding the results of the first.</summary>
    //val andthen: second: Maybe<'b> -> first: Maybe<'a> -> Maybe< ^b>    

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline sequence: source: Maybe< ^a> seq -> Maybe< ^a seq>

    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline traverse: f: (^a -> Maybe< ^b>) -> source: ^a seq -> Maybe< ^b seq>


// Alternative

    /// <summary>A monoidal, associative binary operation representing choice/failure.</summary>
    val orElse: second: Maybe<'a> -> first: Maybe< ^a> -> Maybe< ^a>

    /// <summary>A monoidal, associative binary operation representing choice/failure.</summary>
    val inline orElseWith: second: (unit -> Maybe< ^a>) -> first: Maybe< ^a> -> Maybe< ^a>   

    /// <summary>Generalizes the sequence-based 'concat' function.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline concat: source: Maybe< ^a> seq -> Maybe< ^a>


// Monad

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline bind: f: (^a -> Maybe< ^b>) -> ma: Maybe< ^a> -> Maybe< ^b>

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    val flatten: mm: Maybe<Maybe<'a>> -> Maybe< ^a>

    /// <summary>Recursively generate a monadic context using up to two continuation functions to produce different effects.</summary>
    val inline fixM:
        loop: ((^a -> Maybe< ^b>) -> (Maybe< ^a> -> Maybe< ^b>) -> ^a -> Maybe< ^b>) ->
        em: Choice< ^a, Maybe< ^a>> -> Maybe< ^b>

    // foldlM
    // foldrM

    /// <summary>Computation expression / monadic-workflow type and operations for the given context.</summary>
    [<RequireQualifiedAccess>]
    module Workflow =

        /// <summary>Computation expression for the given monadic context.</summary>
        type MaybeBuilder =
            new: unit -> MaybeBuilder
            member Return: x: 'a -> Maybe< ^a>
            member ReturnFrom: m: Maybe<'a> -> Maybe< ^a>
            member inline Bind: m: Maybe< ^a> * f: (^a -> Maybe< ^b>) -> Maybe< ^b>
            member Zero: unit -> Maybe<unit>
            //member inline Using: disp: ^d * f: (^d -> Maybe< ^a>) -> Maybe< ^a> when ^d :> System.IDisposable
            //member inline TryWith: m: Maybe< ^a> * handler: (exn -> Maybe< ^a>) -> Maybe< ^a>
            //member inline TryFinally: m: Maybe< ^a> * finalizer: (unit -> unit) -> Maybe< ^a>
            member Using: disp: 'd * f: ('d -> Maybe<'a>) -> Maybe<'a> when 'd :> System.IDisposable
            //abstract TryWith: m: Maybe<'a> * h: (exn -> Maybe<'a>) -> Maybe<'a>
            //abstract TryFinally: m: Maybe<'a> * f: (unit -> unit) -> Maybe<'a>
            //member inline Run: f: ^a -> ^a
            //member inline Delay: f: (unit -> ^a) -> ^a


    /// <summary>Computation expression instance for the given context.</summary>
    val maybe: Workflow.MaybeBuilder


// MonadPlus

    /// <summary>If the condition is true, do 'nothing', otherwise 'choose/fail' in the context of the monad.</summary>
    val guard: condition: bool -> Maybe<unit>

    /// <summary>Acts similar to a SQL 'inner join', combining elements of each given monad when the elements satisfy a predicate.</summary>
    val inline join: pred: (^a -> ^b -> bool) -> f: (^a -> ^b -> ^c) -> ma: Maybe< ^a> -> mb: Maybe< ^b> -> Maybe< ^c>

    /// <summary>Generalizes the sequence-based 'filter' function.</summary>
    val inline filter: pred: (^a -> bool) -> ma: Maybe< ^a> -> Maybe< ^a>


// Semigroup

    /// <summary>An associative binary operation on contexts.</summary>
    val inline append: first: Maybe< ^a> -> second: Maybe< ^a> -> Maybe< ^a>
        when ^a: (static member Append: ^a -> ^a -> ^a)


// Monoid

    /// <summary>The identity element of the 'append' operation.</summary>
    val empty<'a> : Maybe< ^a>

    /// <summary>Generalizes the 'concat' operation on sequences to monoids.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline mconcat: source: Maybe< ^a> seq -> Maybe< ^a>
        when ^a: (static member Append: ^a -> ^a -> ^a)

    /// <summary>'Repeat' a value a specified number of times by 'appending' it to itself.</summary>
    val inline repeat: count: int -> elem: Maybe< ^a> -> Maybe< ^a>
        when ^a: (static member Append: ^a -> ^a -> ^a)


// Foldable

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    val inline fold: folder: (^s -> ^a -> ^s) -> seed: ^s -> ta: Maybe< ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    val inline foldBack: folder: (^a -> ^s -> ^s) -> seed: ^s -> ta: Maybe< ^a> -> ^s

    ///// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation. The accumulator is a thunk that is only called as needed by the folding function.</summary>
    //val inline foldl: folder: ((unit -> ^s) -> ^a -> ^s) -> seed: (unit -> ^s) -> ta: Maybe< ^a> -> ^s

    ///// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation. The accumulator is a thunk that is only called as needed by the folding function.</summary>
    //val inline foldr: folder: (^a -> (unit -> ^s) -> ^s) -> seed: (unit -> ^s) -> ta: Maybe< ^a> -> ^s

    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
    val inline mapFold: mapping: (^s -> ^a -> struct(^b * ^s)) -> seed: ^s -> ta: Maybe< ^a> -> struct (Maybe< ^b> * ^s)

    /// <summary>Combines the functionality of map and foldBack, returning the pair of the final context-value and state.</summary>
    val inline mapFoldBack: mapping: (^a -> ^s -> struct(^b * ^s)) -> seed: ^s -> ta: Maybe< ^a> -> struct (Maybe< ^b> * ^s)