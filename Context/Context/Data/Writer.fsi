namespace Rogz.Context.Data.Writer

/// <summary>Adds an 'accumulation' value to another value,
/// threading the accumulation through sequential computations.</summary>
[<Struct>]
type Writer<'Log, 'T> = { Log: 'Log; Value: 'T } with

// Functor
    /// <summary>Lift a function onto a context.
    ///
    /// Allows 'select'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Select: f: System.Func< ^T, ^U> -> Writer< ^Log, ^U>

// Bifunctor
    /// <summary>Map over both arguments covariantly.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline BiSelect: f: System.Func< ^Log, ^Log2> * g: System.Func< ^T, ^U> -> Writer< ^Log2, ^U>

// Foldable
    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Fold: seed: ^S * f: System.Func< ^S, ^T, ^S> -> ^S

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline FoldBack: seed: ^S * f: System.Func< ^T, ^S, ^S> -> ^S

// Bifoldable
    /// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s).</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline BiFold: seed: ^S * fold1: System.Func< ^S, ^Log, ^S> * fold2: System.Func< ^S, ^T, ^S> -> ^S

    /// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s).</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline BiFoldBack: seed: ^S * fold1: System.Func< ^Log, ^S, ^S> * fold2: System.Func< ^T, ^S, ^S> -> ^S