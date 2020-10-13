namespace Rogz.Context.Base


/// <summary>The type of optional value. 'Just a' represents a value 'a',
/// while 'Nothing' represents cases such as when a value does not exist or can't be reached.</summary>
[<Struct>]
type Maybe<'T> = Nothing | Just of 'T
with

// Union Type

    /// <summary>Tries to return the specified value. A return value indicates whether the operation succeeded.</summary>
    member TryJust: [<System.Runtime.InteropServices.Out>] value: outref<'T> -> bool

    /// <summary>Acts as a pattern-match on a union-type, calling the appropriate function based on the case.</summary>
    member Match: ifNothing: System.Func<'U> * ifJust: System.Func<'T, 'U> -> 'U

    /// <summary>Acts as a pattern-match on a union-type, calling the appropriate function based on the case.</summary>
    member Match: ifNothing: System.Action * ifJust: System.Action<'T> -> unit

//// Alternative

//    [<CompiledName("OrElse")>]
//    /// <summary>An associative operation representing a decision between two structures.</summary>
//    static member ( <|> ): first: Maybe<'T> * second: Maybe<'T> -> Maybe<'T>

//    /// <summary>The identity element of 'Alternative' operation(s).</summary>
//    static member Empty : Maybe<'T>

// Semigroup

    //[<CompiledName("Append")>]
    /// <summary>An associative binary operation on monoidal types.</summary>
    static member inline ( + ): first: Maybe< ^a> * second: Maybe< ^a> -> Maybe< ^a>
        when ^a: (static member ( + ): ^a -> ^a -> ^a)

// Monoid

    /// <summary>The identity element of the Monoidal 'append' operation.</summary>
    static member Zero: Maybe<'T>


/// <summary>Operations on Maybe</summary>
module Maybe =

// Haskell Primitives

    /// <summary>Acts as a pattern-match on a union-type, calling the appropriate function based on the case.</summary>
    val inline caseof: nothing: (unit -> ^b) -> just: (^a -> ^b) -> Maybe< ^a> -> ^b   

    /// <summary>Returns true if the value is a 'Just'; false otherwise.</summary>
    val isJust: maybe: Maybe<'a> -> bool

    /// <summary>Returns true if the value is a 'Nothing'; false otherwise.</summary>
    val isNothing: maybe: Maybe<'a> -> bool

    /// <summary>The fromMaybe function takes a default value and and Maybe value. If the Maybe is Nothing, it returns the default values; otherwise, it returns the value contained in the Maybe.</summary>
    val fromMaybe: defaultValue: 'a -> maybe: Maybe< ^a> -> ^a

    /// <summary>Returns 'Nothing' on an empty sequence or 'Just' the head of the sequence.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the input sequence is null.</exception>
    val ofSeq: source: #seq<'a> -> Maybe< ^a>

    /// <summary>Returns an empty list when given Nothing or a singleton list when given Just.</summary>
    val toSeq: maybe: Maybe<'a> -> ^a list

    /// <summary>Map a Maybe-producing function across a sequence and return only the Just-values.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline mapMaybes: f: (^a -> Maybe< ^b>) -> source: #seq< ^a> -> ^b seq


// F# Primitives

    /// <summary>Returns Nothing if the given object is null, otherwise returns the object wrapped in a 'Just'.</summary>
    val ofObj: obj: 'a -> Maybe<'a> when 'a : null

    /// <summary>Convert a Nullable to a Maybe.</summary>
    val ofNullable: nullable: System.Nullable<'a> -> Maybe< ^a>


// Functor

    /// <summary>Lift a function onto a context.</summary>
    val inline map: f: (^a -> ^b) -> fa: Maybe< ^a> -> Maybe< ^b>


// Applicative

    [<CompiledName("Unit")>]
    /// <summary>Lift a value into a context.</summary>
    val unit: value: 'a -> Maybe< ^a>

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    val inline ap: fv: Maybe< ^a> -> ff: Maybe<(^a -> ^b)> -> Maybe< ^b>

    /// <summary>Lift a binary function onto contexts.</summary>
    val inline map2: f: (^a -> ^b -> ^c) -> fa: Maybe< ^a> -> fb: Maybe< ^b> -> Maybe< ^c>

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val sequence: source: #seq<Maybe<'a>> -> Maybe< ^a seq>

    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline traverse: f: (^a -> Maybe< ^b>) -> source: #seq< ^a> -> Maybe< ^b seq>


// Alternative

    /// <summary>The identity element of 'Alternative' operation(s).</summary>
    val empty<'a> : Maybe< ^a>

    /// <summary>An associative operation representing a decision between two structures.</summary>
    val orElse: second: Maybe<'a> -> first: Maybe< ^a> -> Maybe< ^a>

    /// <summary>An associative operation representing a decision between two structures.</summary>
    val orElseWith: second: (unit -> Maybe<'a>) -> first: Maybe< ^a> -> Maybe< ^a>   

    /// <summary>If the condition is true, do 'nothing', otherwise 'choose/fail' in the context of the monad.</summary>
    val guard: condition: bool -> Maybe<unit>

    /// <summary>Generalizes the sequence-based 'concat' function.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the input sequence is null.</exception>
    val concat: source: #seq<Maybe<'a>> -> Maybe< ^a>    


// Monad

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline bind: f: (^a -> Maybe< ^b>) -> m: Maybe< ^a> -> Maybe< ^b>

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    val flatten: mm: Maybe<Maybe<'a>> -> Maybe< ^a>

    /// <summary>Recursively generate a monadic context using up to two continuation functions to produce different effects.</summary>
    val inline fixM:
        loop: ((^a -> Maybe< ^b>) -> (Maybe< ^a> -> Maybe< ^b>) -> ^a -> Maybe< ^b>) ->
        em: Choice< ^a, Maybe< ^a>> -> Maybe< ^b>

    ///// <summary>Monadic fold over the elements of a structure, associating to the left, i.e. from left to right.</summary>
    ///// <exception cref="ArgumentNullException">Thrown when the input sequence is null.</exception>
    /////val inline foldlM: f: (^s -> ^a -> Maybe< ^s>) -> seed: ^s ->  source: #seq< ^a> -> Maybe< ^s>
    /////
    ///// <summary>Monadic fold over the elements of a structure, associating to the right, i.e. from right to left.</summary>
    ///// <exception cref="ArgumentNullException">Thrown when the input sequence is null.</exception>
    /////val inline foldrM: f: (^a -> ^s -> Maybe< ^s>) -> seed: ^s -> source: #seq< ^a> -> Maybe< ^s>


    /// <summary>Computation expression / monadic-workflow type and operations for the given context.</summary>
    [<RequireQualifiedAccess>]
    module Workflow =

        /// <summary>Computation expression for the given monadic context.</summary>
        type MaybeBuilder =
            new: unit -> MaybeBuilder
            member Return: x: 'a -> Maybe< ^a>
            member ReturnFrom: m: Maybe<'a> -> Maybe< ^a>
            member Zero: unit -> Maybe<unit>
            member inline Bind: m: Maybe< ^a> * f: (^a -> Maybe< ^b>) -> Maybe< ^b>

    /// <summary>Computation expression instance for the given context.</summary>
    val maybe: Workflow.MaybeBuilder


// MonadPlus    

    /// <summary>Generalizes the sequence-based 'filter' function.</summary>
    val inline filter: pred: (^a -> bool) -> m: Maybe< ^a> -> Maybe< ^a>

    /// <summary>Acts similar to a SQL 'inner join', combining elements of each given monad when the elements satisfy a predicate.</summary>
    val inline join: pred: (^a -> ^b -> bool) -> func: (^a -> ^b -> ^c) -> ma: Maybe< ^a> -> mb: Maybe< ^b> -> Maybe< ^c>    


// Semigroup

    /// <summary>An associative binary operation on monoidal types.</summary>
    val inline append: second: Maybe< ^a> -> first: Maybe< ^a> -> Maybe< ^a>
        when ^a: (static member ( + ): ^a -> ^a -> ^a)


// Monoid

    /// <summary>The identity element of the Monoidal 'append' operation.</summary>
    val zero<'a> : Maybe<'a>

    /// <summary>Generalizes the 'concat' operation on sequences to monoids.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline sum: source: #seq<Maybe< ^a>> -> Maybe< ^a>
        when ^a: (static member ( + ): ^a -> ^a -> ^a)


// Foldable

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    val inline fold: folder: (^s -> ^a -> ^s) -> seed: ^s -> source: Maybe< ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    val inline foldBack: folder: (^a -> ^s -> ^s) -> seed: ^s -> source: Maybe< ^a> -> ^s

    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
    val inline mapFold: mapping: (^s -> ^a -> struct(^b * ^s)) -> seed: ^s -> source: Maybe< ^a> -> struct (Maybe< ^b> * ^s)

    /// <summary>Combines the functionality of map and foldBack, returning the pair of the final context-value and state.</summary>
    val inline mapFoldBack: mapping: (^a -> ^s -> struct(^b * ^s)) -> seed: ^s -> source: Maybe< ^a> -> struct (Maybe< ^b> * ^s)