namespace Rogz.Context.Data.Writer


/// <summary>Operations on Writers.</summary>
module Writer =

// Minimal

    /// <summary>Insert a 'record' into the accumulation.</summary>
    val tell: record: 'w -> Writer< ^w, unit>

    /// <summary>Attaches the accumulated value to the output.</summary>
    val listen: writer: Writer<'w, 'a> -> Writer< ^w, struct(^w * ^a)>

    /// <summary>Applies a function to the accumulator then attaches the result to the output.</summary>
    val inline listens: f: (^w -> ^a -> ^b) -> writer: Writer< ^w, ^a> -> Writer< ^w, ^b>


// Primitives

    /// <summary>Apply a function to both the accumulation and the output.</summary>
    val inline runWriter: f: (^w -> ^a -> ^b) -> writer: Writer< ^w, ^a> -> ^b


// Functor

    /// <summary>Lift a function onto a context.</summary>
    val inline map: f: (^a -> ^b) -> fa: Writer< ^w, ^a> -> Writer< ^w, ^b>


// Bifunctor

    /// <summary>Map over both arguments covariantly.</summary>
    val inline bimap: f: (^a -> ^c) -> g: (^b -> ^d) -> bf: Writer< ^a, ^b> -> Writer< ^c, ^d>

    /// <summary>Map over the first value, leaving the second value as-is.</summary>
    val inline mapFirst: f: (^a -> ^c) -> bf: Writer< ^a, ^b> -> Writer< ^c, ^b>


// Applicative

    /// <summary>Lift a value into a context.</summary>
    val inline unit: value: ^a -> Writer< ^w, ^a>
        when ^w: (static member Empty: unit -> ^w)

    /// <summary>Sequential application of functions stored within contexts onto values stored within similar contexts.</summary>
    val inline ap: fv: Writer< ^w, ^a> -> ff: Writer< ^w, (^a -> ^b)> -> Writer< ^w, ^b>
        when ^w: (static member Append: ^w -> ^w -> ^w)

    /// <summary>Lift a binary function onto contexts.</summary>
    val inline map2: f: (^a -> ^b -> ^c) -> fa: Writer< ^w, ^a> -> fb: Writer< ^w, ^b> -> Writer< ^w, ^c>
        when ^w: (static member Append: ^w -> ^w -> ^w)

    ///// <summary>Sequence two contexts, discarding the results of the first.</summary>
    //val inline andthen: fb: Writer< ^w, ^b> -> fa: Writer< ^w, ^a> -> Writer< ^w, ^b>
    //    when ^w: (static member Append: ^w -> ^w -> ^w)

    /// <summary>Evaluate each context in a sequence from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline sequence: source: Writer< ^w, ^a> seq -> Writer< ^w, ^a seq>
        when ^w: (static member Append: ^w -> ^w -> ^w)
        and  ^w: (static member Empty: unit -> ^w)
    
    /// <summary>Map each element of a sequence to a context, evaluate these contexts from left to right, and collect the results.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val inline traverse: f: (^a -> Writer< ^w, ^b>) -> source: ^a seq -> Writer< ^w, ^b seq>
        when ^w: (static member Append: ^w -> ^w -> ^w)
        and  ^w: (static member Empty: unit -> ^w)


// Biapplicative

    /// <summary>Lift two values into a context.</summary>
    val biunit: a: 'a -> b: 'b -> Writer< ^a, ^b>

    /// <summary>Lift two binary functions onto contexts.</summary>
    val inline bimap2: f: (^a -> ^b -> ^c) -> g: (^d -> ^e -> ^f) -> fad: Writer< ^a, ^d> -> fbe: Writer< ^b, ^e> -> Writer< ^c, ^f>


// Monad

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline bind: f: (^a -> Writer< ^w, ^b>) -> m: Writer< ^w, ^a> -> Writer< ^w, ^b>
        when ^w: (static member Append: ^w -> ^w -> ^w)

    /// <summary>Removes one level of context structure, projecting its bound argument into the outer level.</summary>
    val inline flatten: mm: Writer< ^w, Writer< ^w, ^a>> -> Writer< ^w, ^a>
        when ^w: (static member Append: ^w -> ^w -> ^w)

    /// <summary>Recursively generate a monadic context using up to two continuation functions to produce different effects.</summary>
    val inline fixM:
        loop: ((^a -> Writer< ^w, ^b>) -> (Writer< ^w, ^a> -> Writer< ^w, ^b>) -> ^a -> Writer< ^w, ^b>) ->
        em: Choice< ^a, Writer< ^w, ^a>> -> Writer< ^w, ^b>
        when ^w: (static member Append: ^w -> ^w -> ^w)
        and  ^w: (static member Empty: unit -> ^w)

    // foldlM
    // foldrM
    
    /// <summary>Computation expression / monadic-workflow type and operations for the given context.</summary>
    [<RequireQualifiedAccess>]
    module Workflow =

        /// <summary>Computation expression for the given monadic context.</summary>
        type WriterBuilder =
            new: unit -> WriterBuilder
            member inline Return: x: ^a -> Writer< ^w, ^a>
                when ^w: (static member Empty: unit -> ^w)
            member ReturnFrom: m: Writer<'w, 'a> -> Writer< ^w, ^a>
            member inline Bind: m: Writer< ^w, ^a> * f: (^a -> Writer< ^w, ^b>) -> Writer< ^w, ^b>
                when ^w: (static member Append: ^w -> ^w -> ^w)
            member inline Zero: unit -> Writer< ^w, unit>
                when ^w: (static member Empty: unit -> ^w)
            //member inline Using: disp: ^d * f: (^d -> Writer< ^w, ^a>) -> Writer< ^w, ^a> when ^d :> System.IDisposable
            //member inline TryWith: m: Writer< ^w, ^a> * handler: (exn -> Writer< ^w, ^a>) -> Writer< ^w, ^a>
            //member inline TryFinally: m: Writer< ^w, ^a> * finalizer: (unit -> unit) -> Writer< ^w, ^a>
            member Using: disp: 'd * f: ('d -> Writer<'w, 'a>) -> Writer<'w, 'a> when 'd :> System.IDisposable
            //abstract TryWith: m: Writer<'w, 'a> * h: (exn -> Writer<'w, 'a>) -> Writer<'w, 'a>
            //abstract TryFinally: m: Writer<'w, 'a> * f: (unit -> unit) -> Writer<'w, 'a>


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
    val inline append: first: Writer< ^w, ^a> -> second: Writer< ^w, ^a> -> Writer< ^w, ^a>
        when ^w: (static member Append: ^w -> ^w -> ^w)
        and  ^a: (static member Append: ^a -> ^a -> ^a)


// Foldable

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    val inline fold: folder: (^s -> ^a -> ^s) -> seed: ^s -> ta: Writer< ^w, ^a> -> ^s

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    val inline foldBack: folder: (^a -> ^s -> ^s) -> seed: ^s -> ta: Writer< ^w, ^a> -> ^s

    ///// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation. The accumulator is a thunk that is only called as needed by the folding function.</summary>
    //val inline foldl: folder: ((unit -> ^s) -> ^a -> ^s) -> seed: (unit -> ^s) -> ta: Writer< ^w, ^a> -> ^s

    ///// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation. The accumulator is a thunk that is only called as needed by the folding function.</summary>
    //val inline foldr: folder: (^a -> (unit -> ^s) -> ^s) -> seed: (unit -> ^s) -> ta: Writer< ^w, ^a> -> ^s

    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
    val inline mapFold: mapping: (^s -> ^a -> struct(^b * ^s)) -> seed: ^s -> ta: Writer< ^w, ^a> -> struct (Writer< ^w, ^b> * ^s)

    /// <summary>Combines the functionality of map and foldBack, returning the pair of the final context-value and state.</summary>
    val inline mapFoldBack: mapping: (^a -> ^s -> struct(^b * ^s)) -> seed: ^s -> ta: Writer< ^w, ^a> -> struct (Writer< ^w, ^b> * ^s)


// Bifoldable

    /// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s).</summary>
    val inline bifold: fold1: (^s -> ^a -> ^s) -> fold2: (^s -> ^b -> ^s) -> seed: ^s -> t: Writer< ^a, ^b> -> ^s

    /// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s).</summary>
    val inline bifoldBack: fold1: (^a -> ^s -> ^s) -> fold2: (^b -> ^s -> ^s) -> seed: ^s -> t: Writer< ^a, ^b> -> ^s

    ///// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s). The accumulator is a thunk that is only called as needed by the folding function.</summary>
    //val inline bifoldl: fold1: ((unit -> ^s) -> ^a -> ^s) -> fold2: ((unit -> ^s) -> ^b -> ^s) -> seed: (unit -> ^s) -> t: Writer< ^a, ^b> -> ^s

    ///// <summary>Applies a function to all element(s) of up to two possible sources, threading an accumulator argument through the computation(s). The accumulator is a thunk that is only called as needed by the folding function.</summary>
    //val inline bifoldr: fold1: (^a -> (unit -> ^s) -> ^s) -> fold2: (^b -> (unit -> ^s) -> ^s) -> seed: (unit -> ^s) -> t: Writer< ^a, ^b> -> ^s

    /// <summary>Combines the functionality of map and fold, returning the pair of the final context-value and state.</summary>
    val inline bimapFold: mapping1: (^s -> ^a -> struct (^b * ^s)) -> mapping2: (^s -> ^c -> struct (^d * ^s)) -> seed: ^s -> t: Writer< ^a, ^c> -> struct (Writer< ^b, ^d> * ^s)

    /// <summary>Combines the functionality of map and foldBack, returning the pair of the final context-value and state.</summary>
    val inline bimapFoldBack: mapping1: (^a -> ^s -> struct (^b * ^s)) -> mapping2: (^c -> ^s -> struct (^d * ^s)) -> seed: ^s -> t: Writer< ^a, ^c> -> struct (Writer< ^b, ^d> * ^s)