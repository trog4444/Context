namespace Rogz.Context.Base


/// <summary>Adds an 'accumulation' value to another value,
/// threading the accumulation through sequential computations.</summary>
[<Struct>]
type Writer<'Log, 'T> = { Log: 'Log; Value: 'T }
with

// Semigroup

    //[<CompiledName("Append")>]
    /// <summary>Acts as the semigroup, binary 'append' operator.</summary>
    static member inline ( + ): first: Writer< ^w, ^a> * second: Writer< ^w, ^a> -> Writer< ^w, ^a>
        when ^w: (static member ( + ): ^w -> ^w -> ^w)
        and  ^a: (static member ( + ): ^a -> ^a -> ^a)


/// <summary>Operations on Writers.</summary>
module Writer =

// Haskell Primitives

    /// <summary>Embeds a simple Writer action.</summary>
    val write: info: struct('w * 'a) -> Writer<'w, 'a>

    /// <summary>Insert a 'record' into the accumulation.</summary>
    val tell: record: 'w -> Writer< ^w, unit>

    /// <summary>Attaches the accumulated value to the output.</summary>
    val listen: writer: Writer<'w, 'a> -> Writer< ^w, struct(^w * ^a)>

    // pass :: m (a, w -> w) -> m a

    /// <summary>Applies a function to the accumulator then attaches the result to the output.</summary>
    val inline listens: f: (^w -> ^a -> ^b) -> writer: Writer< ^w, ^a> -> Writer< ^w, ^b>

    // censor :: (w -> w) -> m a -> m a

    /// <summary>Apply a function to both the accumulation and the output.</summary>
    val inline runWriter: f: (^w -> ^a -> ^b) -> writer: Writer< ^w, ^a> -> ^b

    // execWriter = gets only the log. note that I changed runWriter to not simply return the pair.

    /// <summary>Map both the return value and output of a computation using the given function.</summary>
    val inline mapWriter: f: (struct(^w0 * ^a) -> struct(^w * ^b)) -> writer: Writer< ^w0, ^a> -> Writer< ^w, ^b>


// Functor

    /// <summary>Lift a function onto a context.</summary>
    val inline map: f: (^a -> ^b) -> fa: Writer< ^w, ^a> -> Writer< ^w, ^b>


// Bifunctor

    /// <summary>Map over both arguments covariantly.</summary>
    val inline bimap: f: (^a -> ^c) -> g: (^b -> ^d) -> bf: Writer< ^a, ^b> -> Writer< ^c, ^d>

    /// <summary>Map over the first value, leaving the second value as-is.</summary>
    val inline mapFirst: f: (^a -> ^c) -> bf: Writer< ^a, ^b> -> Writer< ^c, ^b>


// Applicative

    [<CompiledName("Unit")>]
    /// <summary>Lift a value into a context.</summary>
    val inline unit: value: ^a -> Writer< ^w, ^a>
        when ^w: (static member Zero: unit -> ^w)

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    val inline ap: fv: Writer< ^w, ^a> -> ff: Writer< ^w, (^a -> ^b)> -> Writer< ^w, ^b>
        when ^w: (static member ( + ): ^w -> ^w -> ^w)

    /// <summary>Lift a binary function onto contexts.</summary>
    val inline map2: f: (^a -> ^b -> ^c) -> fa: Writer< ^w, ^a> -> fb: Writer< ^w, ^b> -> Writer< ^w, ^c>
        when ^w: (static member ( + ): ^w -> ^w -> ^w)

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline sequence: source: Writer< ^w, ^a> seq -> Writer< ^w, ^a seq>
        when ^w: (static member ( + ): ^w -> ^w -> ^w)
        and  ^w: (static member Zero: unit -> ^w)
    
    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline traverse: f: (^a -> Writer< ^w, ^b>) -> source: ^a seq -> Writer< ^w, ^b seq>
        when ^w: (static member ( + ): ^w -> ^w -> ^w)
        and  ^w: (static member Zero: unit -> ^w)


// Biapplicative

    /// <summary>Lift two values into a context.</summary>
    val biunit: a: 'a -> b: 'b -> Writer< ^a, ^b>

    /// <summary>Lift two binary functions onto contexts.</summary>
    val inline bimap2: f: (^a -> ^b -> ^c) -> g: (^d -> ^e -> ^f) -> fad: Writer< ^a, ^d> -> fbe: Writer< ^b, ^e> -> Writer< ^c, ^f>


// Monad

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline bind: f: (^a -> Writer< ^w, ^b>) -> m: Writer< ^w, ^a> -> Writer< ^w, ^b>
        when ^w: (static member ( + ): ^w -> ^w -> ^w)

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    val inline flatten: mm: Writer< ^w, Writer< ^w, ^a>> -> Writer< ^w, ^a>
        when ^w: (static member ( + ): ^w -> ^w -> ^w)

    /// <summary>Recursively generate a monadic context using up to two continuation functions to produce different effects.</summary>
    val inline fixM:
        loop: ((^a -> Writer< ^w, ^b>) -> (Writer< ^w, ^a> -> Writer< ^w, ^b>) -> ^a -> Writer< ^w, ^b>) ->
        em: Choice< ^a, Writer< ^w, ^a>> -> Writer< ^w, ^b>
        when ^w: (static member ( + ): ^w -> ^w -> ^w)
        and  ^w: (static member Zero: unit -> ^w)

    
    /// <summary>Computation expression / monadic-workflow type and operations for the given context.</summary>
    [<RequireQualifiedAccess>]
    module Workflow =

        /// <summary>Computation expression for the given monadic context.</summary>
        type WriterBuilder =
            new: unit -> WriterBuilder
            member inline Return: x: ^a -> Writer< ^w, ^a> when ^w: (static member Zero: unit -> ^w)
            member ReturnFrom: m: Writer<'w, 'a> -> Writer<'w, 'a>
            member inline Zero: unit -> Writer< ^w, unit> when ^w: (static member Zero: unit -> ^w)
            member inline Bind: m: Writer< ^w, ^a> * f: (^a -> Writer< ^w, ^b>) -> Writer< ^w, ^b>
                when ^w: (static member ( + ): ^w -> ^w -> ^w)

    /// <summary>Computation expression instance for the given context.</summary>
    val writer: Workflow.WriterBuilder


// Comonad

    /// <summary>Retrieve a value from a co-context.</summary>
    val extract: w: Writer<'w, 'a> -> ^a

    /// <summary>Sequentially compose two co-contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline extend: f: (Writer< ^w, ^a> -> ^b) -> w: Writer< ^w, ^a> -> Writer< ^w, ^b>

    /// <summary>Adds a layer of co-context onto an existing co-context.</summary>
    val duplicate: w: Writer<'w, 'a> -> Writer< ^w, Writer< ^w, ^a>>


// Semigroup

    /// <summary>An associative binary operation on contexts.</summary>
    val inline append: second: Writer< ^w, ^a> -> first: Writer< ^w, ^a> -> Writer< ^w, ^a>
        when ^w: (static member ( + ): ^w -> ^w -> ^w)
        and  ^a: (static member ( + ): ^a -> ^a -> ^a)


// Foldable

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    val inline fold: folder: (^s -> ^a -> ^s) -> seed: ^s -> source: Writer< ^w, ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    val inline foldBack: folder: (^a -> ^s -> ^s) -> seed: ^s -> source: Writer< ^w, ^a> -> ^s

    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
    val inline mapFold: mapping: (^s -> ^a -> struct(^b * ^s)) -> seed: ^s -> ta: Writer< ^w, ^a> -> struct (Writer< ^w, ^b> * ^s)

    /// <summary>Combines the functionality of map and foldBack, returning the pair of the final context-value and state.</summary>
    val inline mapFoldBack: mapping: (^a -> ^s -> struct(^b * ^s)) -> seed: ^s -> ta: Writer< ^w, ^a> -> struct (Writer< ^w, ^b> * ^s)


// Bifoldable

    /// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s).</summary>
    val inline bifold: fold1: (^s -> ^a -> ^s) -> fold2: (^s -> ^b -> ^s) -> seed: ^s -> source: Writer< ^a, ^b> -> ^s

    /// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s).</summary>
    val inline bifoldBack: fold1: (^a -> ^s -> ^s) -> fold2: (^b -> ^s -> ^s) -> seed: ^s -> source: Writer< ^a, ^b> -> ^s

    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
    val inline bimapFold: mapping1: (^s -> ^a -> struct (^b * ^s)) -> mapping2: (^s -> ^c -> struct (^d * ^s)) -> seed: ^s -> source: Writer< ^a, ^c> -> struct (Writer< ^b, ^d> * ^s)

    /// <summary>Combines the functionality of map and foldBack, returning the pair of the final context-value and state.</summary>
    val inline bimapFoldBack: mapping1: (^a -> ^s -> struct (^b * ^s)) -> mapping2: (^c -> ^s -> struct (^d * ^s)) -> seed: ^s -> source: Writer< ^a, ^c> -> struct (Writer< ^b, ^d> * ^s)