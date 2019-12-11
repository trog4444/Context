namespace Rogz.Context.Data.Attr


/// <summary>Operations on Attributed values.</summary>
module Attr =

// Primitives

    /// <summary>Return the underlying value.</summary>
    val unAttr: Attr<'t, 'a> -> ^a

    /// <summary>Apply an attribute to a value.</summary>
    val wAttr<'attr, 'a> : ^a -> Attr< ^attr, ^a>

    /// <summary>Set the attribute to a new type.</summary>
    val setAttr<'tOld, 'tNew, 'a> : Attr< ^tOld, ^a> -> Attr< ^tNew, ^a>


// Functor

    /// <summary>Lift a function onto a context.</summary>
    val inline map: f: (^a -> ^b) -> fa: Attr< ^t, ^a> -> Attr< ^t, ^b>


// Applicative

    /// <summary>Lift a value into a context.</summary>
    val unit: value: 'a -> Attr<'t, ^a>

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    val inline ap: fv: Attr< ^t, ^a> -> ff: Attr< ^t, (^a -> ^b)> -> Attr< ^t, ^b>

    /// <summary>Lift a binary function onto contexts.</summary>
    val inline map2: f: (^a -> ^b -> ^c) -> fa: Attr< ^t, ^a> -> fb: Attr< ^t, ^b> -> Attr< ^t, ^c>

    /// <summary>Sequence two contexts, discarding the results of the first.</summary>
    val inline andthen: fb: Attr< ^t, ^b> -> fa: Attr< ^t, ^a> -> Attr< ^t, ^b>

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline sequence: source: Attr< ^t, ^a> seq -> Attr< ^t, ^a seq>

    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline traverse: f: (^a -> Attr< ^t, ^b>) -> source: ^a seq -> Attr< ^t, ^b seq>


// Monad

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline bind: f: (^a -> Attr< ^t, ^b>) -> ma: Attr< ^t, ^a> -> Attr< ^t, ^b>

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    val flatten: mm: Attr<'t, Attr< ^t, 'a>> -> Attr< ^t, ^a>

    /// <summary>Recursively generate a monadic context using up to two continuation functions to produce different effects.</summary>
    val inline fixM:
        loop: ((^a -> Attr< ^t, ^b>) -> (Attr< ^t, ^a> -> Attr< ^t, ^b>) -> ^a -> Attr< ^t, ^b>) ->
        em: Rogz.Context.Data.Either.Either< ^a, Attr< ^t, ^a>> -> Attr< ^t, ^b>

    // foldlM
    // foldrM

    /// <summary>Computation expression / monadic-workflow type and operations for the given context.</summary>
    [<RequireQualifiedAccess>]
    module Workflow =

        /// <summary>Computation expression for the given monadic context.</summary>
        type AttrBuilder =
            new: unit -> AttrBuilder
            member inline Return: x: ^a -> Attr< ^t, ^a>
            member inline ReturnFrom: m: Attr< ^t, ^a> -> Attr< ^t, ^a>
            member inline Bind: m: Attr< ^t, ^a> * f: (^a -> Attr< ^t, ^b>) -> Attr< ^t, ^b>
            member inline Zero: unit -> Attr< ^t, unit>
            member inline Using: disp: ^d * f: (^d -> Attr< ^t, ^a>) -> Attr< ^t, ^a> when ^d :> System.IDisposable
            member inline TryWith: m: Attr< ^t, ^a> * h: (exn -> Attr< ^t, ^a>) -> Attr< ^t, ^a>
            member inline TryFinally: m: Attr< ^t, ^a> * f: (unit -> unit) -> Attr< ^t, ^a>


    /// <summary>Computation expression instance for the given context.</summary>
    val attr: Workflow.AttrBuilder


// Comonad

    /// <summary>Retrieve a value from a co-context.</summary>
    val extract: w: Attr<'t, 'a> -> ^a

    /// <summary>Sequentially compose two co-contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline extend: f: (Attr< ^t, ^a> -> ^b) -> wa: Attr< ^t, ^a> -> Attr< ^t, ^b>

    /// <summary>Adds a layer of co-context onto an existing co-context.</summary>
    val duplicate: w: Attr<'t, 'a> -> Attr< ^t, Attr< ^t, ^a>>


// Semigroup

    /// <summary>An associative binary operation on contexts.</summary>
    val inline append: first: Attr< ^t, ^a> -> second: Attr< ^t, ^a> -> Attr< ^t, ^a>
        when ^a: (static member Append: ^a -> ^a -> ^a)


// Foldable

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    val inline fold: folder: (^s -> ^a -> ^s) -> seed: ^s -> ta: Attr< ^t, ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    val inline foldBack: folder: (^a -> ^s -> ^s) -> seed: ^s -> ta: Attr< ^t, ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation. The accumulator is a thunk that is only called as needed by the folding function.</summary>
    val inline foldl: folder: ((unit -> ^s) -> ^a -> ^s) -> seed: (unit -> ^s) -> ta: Attr< ^t, ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation. The accumulator is a thunk that is only called as needed by the folding function.</summary>
    val inline foldr: folder: (^a -> (unit -> ^s) -> ^s) -> seed: (unit -> ^s) -> ta: Attr< ^t, ^a> -> ^s

    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
    val inline mapFold: mapping: (^s -> ^a -> struct (^b * ^s)) -> seed: ^s -> ta: Attr< ^t, ^a> -> struct (Attr< ^t, ^b> * ^s)

    /// <summary>Combines the functionality of map and foldBack, returning the pair of the final context-value and state.</summary>
    val inline mapFoldBack: mapping: (^a -> ^s -> struct (^b * ^s)) -> seed: ^s -> ta: Attr< ^t, ^a> -> struct (Attr< ^t, ^b> * ^s)