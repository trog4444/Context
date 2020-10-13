namespace Rogz.Context.Base


/// <summary>Represents a type that is 'tagged' with extra type information,
/// which is not used in actual calculations.</summary>
[<Struct>]
type Tagged<'Attribute, 'T> = Tag of 'T
with

// Util

    /// <summary>Return the underlying value.</summary>
    member inline internal Value: ^T

// Union

    /// <summary>Acts as a pattern-match on a union-type, calling the appropriate function based on the case.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Match: f: System.Func< ^T, ^U> -> ^U

    /// <summary>Acts as a pattern-match on a union-type, calling the appropriate action based on the case.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the action attempting to be called is null.</exception>
    member inline Match: action: System.Action< ^T> -> unit


/// <summary>Operations on Tagged values.</summary>
module Tagged =

// F# Primitives

    /// <summary>Return the underlying value.</summary>
    val untag: Tagged<'t, 'a> -> ^a

    /// <summary>Apply a tag to a value.</summary>
    val withtag<'tag, 'a> : ^a -> Tagged< ^tag, ^a>

    /// <summary>Reset the tag to a new type.</summary>
    val retag<'tOld, 'tNew, 'a> : Tagged< ^tOld, ^a> -> Tagged< ^tNew, ^a>
    
    /// <summary>Provides type information, even though there is no value of that type.</summary>
    val proxy<'t> : Tagged<'t, unit>

    /// <summary>Takes a representation of a proxy to a Tagged value.</summary>
    val inline unproxy: (Tagged< ^t, unit> -> ^a) -> Tagged< ^t, ^a>

    /// <summary>Restrict the type of the supplied function such that its input matches the tag of the `proxy`.</summary>
    val inline taggedBy: proxy: Tagged< ^t, ^``_``> -> f: (^t -> ^r) -> x: ^t -> ^r

    /// <summary>Generate a System.Type representing the type of the given proxy's tag.</summary>
    val tagOf: proxy: Tagged<'t, '``_``> -> System.Type

    /// <summary>Apply a a value's own type as a tag.</summary>
    val tagSelf: x: 'a -> Tagged< ^a, ^a>


// Functor

    /// <summary>Lift a function onto a context.</summary>
    val inline map: f: (^a -> ^b) -> fa: Tagged< ^t, ^a> -> Tagged< ^t, ^b>


// Bifunctor

    /// <summary>Map over both arguments covariantly.</summary>
    val inline bimap: f: (^a -> ^c) -> g: (^b -> ^d) -> bf: Tagged< ^a, ^b> -> Tagged< ^c, ^d>

    /// <summary>Map over the first value, leaving the second value as-is.</summary>
    val inline mapFirst: f: (^a -> ^c) -> bf: Tagged< ^a, ^b> -> Tagged< ^c, ^b>


// Applicative

    [<CompiledName("Unit")>]
    /// <summary>Lift a value into a context.</summary>
    val unit: value: 'a -> Tagged<'t, ^a>

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    val inline ap: fv: Tagged< ^t, ^a> -> ff: Tagged< ^t, (^a -> ^b)> -> Tagged< ^t, ^b>

    /// <summary>Lift a binary function onto contexts.</summary>
    val inline map2: f: (^a -> ^b -> ^c) -> fa: Tagged< ^t, ^a> -> fb: Tagged< ^t, ^b> -> Tagged< ^t, ^c>

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val sequence: source: #seq<Tagged<'t, 'a>> -> Tagged< ^t, ^a seq>

    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline traverse: f: (^a -> Tagged< ^t, ^b>) -> source: #seq< ^a> -> Tagged< ^t, ^b seq>


// Monad

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline bind: f: (^a -> Tagged< ^t, ^b>) -> m: Tagged< ^t, ^a> -> Tagged< ^t, ^b>

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    val flatten: mm: Tagged<'t, Tagged< ^t, 'a>> -> Tagged< ^t, ^a>

    /// <summary>Recursively generate a monadic context using up to two continuation functions to produce different effects.</summary>
    val inline fixM:
        loop: ((^a -> Tagged< ^t, ^b>) -> (Tagged< ^t, ^a> -> Tagged< ^t, ^b>) -> ^a -> Tagged< ^t, ^b>) ->
        em: Choice< ^a, Tagged< ^t, ^a>> -> Tagged< ^t, ^b>


    /// <summary>Computation expression / monadic-workflow type and operations for the given context.</summary>
    [<RequireQualifiedAccess>]
    module Workflow =

        /// <summary>Computation expression for the given monadic context.</summary>
        type TaggedBuilder =
            new: unit -> TaggedBuilder
            member Return: x: 'a -> Tagged<'t, ^a>
            member ReturnFrom: m: Tagged<'t, 'a> -> Tagged< ^t, ^a>
            member Zero: unit -> Tagged<'t, unit>
            member inline Bind: m: Tagged< ^t, ^a> * f: (^a -> Tagged< ^t, ^b>) -> Tagged< ^t, ^b>            

    /// <summary>Computation expression instance for the given context.</summary>
    val tagged: Workflow.TaggedBuilder


// Comonad

    /// <summary>Retrieve a value from a co-context.</summary>
    val extract: w: Tagged<'t, 'a> -> ^a

    /// <summary>Sequentially compose two co-contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline extend: f: (Tagged< ^t, ^a> -> ^b) -> wa: Tagged< ^t, ^a> -> Tagged< ^t, ^b>

    /// <summary>Adds a layer of co-context onto an existing co-context.</summary>
    val duplicate: w: Tagged<'t, 'a> -> Tagged< ^t, Tagged< ^t, ^a>>


// Semigroup

    /// <summary>An associative binary operation on contexts.</summary>
    val inline append: second: Tagged< ^t, ^a> -> first: Tagged< ^t, ^a> -> Tagged< ^t, ^a>
        when ^a: (static member ( + ): ^a -> ^a -> ^a)


// Foldable

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    val inline fold: folder: (^s -> ^a -> ^s) -> seed: ^s -> source: Tagged< ^t, ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    val inline foldBack: folder: (^a -> ^s -> ^s) -> seed: ^s -> source: Tagged< ^t, ^a> -> ^s

    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
    val inline mapFold: mapping: (^s -> ^a -> struct (^b * ^s)) -> seed: ^s -> source: Tagged< ^t, ^a> -> struct (Tagged< ^t, ^b> * ^s)

    /// <summary>Combines the functionality of map and foldBack, returning the pair of the final context-value and state.</summary>
    val inline mapFoldBack: mapping: (^a -> ^s -> struct (^b * ^s)) -> seed: ^s -> source: Tagged< ^t, ^a> -> struct (Tagged< ^t, ^b> * ^s)