namespace Rogz.Context.Data.Either

/// <summary>A type that can represent a choice between two different types.</summary>
[<Struct>]
type Either<'L, 'R> = Left of left: 'L | Right of right: 'R with

// Union
    /// <summary>Acts as a pattern-match on a union-type, calling the appropriate function based on the case.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Match: fLeft: System.Func< ^L, ^T> * fRight: System.Func< ^R, ^T> -> ^T

    /// <summary>Acts as a pattern-match on a union-type, calling the appropriate action based on the case.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the action attempting to be called is null.</exception>
    member inline Match: leftAction: System.Action< ^L> * rightAction: System.Action< ^R> -> unit

// Functor
    /// <summary>Lift a function onto a context.
    ///
    /// Allows 'select'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Select: f: System.Func< ^R, ^S> -> Either< ^L, ^S>

// Bifunctor
    /// <summary>Map over both arguments covariantly.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline BiSelect: f: System.Func< ^L, ^L2> * g: System.Func< ^R, ^R2> -> Either< ^L2, ^R2>

// Applicative
    /// <summary>Lift a binary function onto contexts.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Zip: other: Either< ^L, ^S> * f: System.Func< ^R, ^S, ^T> -> Either< ^L, ^T>
    
    /// <summary>Lift a binary function onto contexts.
    ///
    /// Allows 'join ... on 1 equals 1'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Join: other: Either< ^L, ^S> * kt: System.Func< ^R, int> * ku: System.Func< ^S, int> * f: System.Func< ^R, ^S, ^T> -> Either< ^L, ^T>

// Alternative
    /// <summary>A monoidal, associative binary operation representing choice/failure.</summary>
    member inline OrElse: other: Either< ^L, ^R> -> Either< ^L, ^R>

// Monad
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.
    ///
    /// Allows 'select'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline SelectMany: f: System.Func< ^R, Either< ^L, ^S>> -> Either< ^L, ^S>
    
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.
    ///
    /// Allows nested 'from'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline SelectMany: f: System.Func< ^R, Either< ^L, ^S>> * g: System.Func< ^R, ^S, ^T> -> Either< ^L, ^T>

// Semigroup
    /// <summary>An associative binary operation on contexts.</summary>
    static member inline Append: first: Either< ^e, ^a> * second: Either< ^e, ^a> -> Either< ^e, ^a>
        when ^a: (static member Append: ^a -> ^a -> ^a)

// Foldable
    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Fold: seed: ^S * f: System.Func< ^S, ^R, ^S> -> ^S

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline FoldBack: seed: ^S * f: System.Func< ^R, ^S, ^S> -> ^S

// Bifoldable
    /// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s).</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline BiFold: seed: ^S * fold1: System.Func< ^S, ^L, ^S> * fold2: System.Func< ^S, ^R, ^S> -> ^S

    /// <summary>Applies a function to all element(s) of two possible sources, threading an accumulator argument through the computation(s).</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline BiFoldBack: seed: ^S * fold1: System.Func< ^L, ^S, ^S> * fold2: System.Func< ^R, ^S, ^S> -> ^S