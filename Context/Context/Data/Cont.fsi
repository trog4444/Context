namespace Rogz.Context.Data.Cont

/// <summary>The Continuation type represents computations in continuation-passing style (CPS).
/// In CPS, a function's result is not returned immediately, but instead is passed to another function, received as a parameter (continuation).
/// Computations are built up from sequences of nested continuations, terminated by a final continuation which produces the final result.</summary>
[<Struct; NoComparison; NoEquality>]
type Cont<'R, 'T> = Cont of (('T -> 'R) -> 'R) with

// Function
    /// <summary>The result of running a CPS computation with a given final continuation.</summary>
    member inline Invoke: k: System.Func< ^T, ^R> -> ^R

// Functor
    /// <summary>Lift a function onto a context.
    ///
    /// Allows 'select'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Select: f: System.Func< ^T, ^U> -> Cont< ^R, ^U>

// Applicative
    /// <summary>Lift a binary function onto contexts.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Zip: other: Cont< ^R, ^U> * f: System.Func< ^T, ^U, ^V> -> Cont< ^R, ^V>

    /// <summary>Lift a binary function onto contexts.
    ///
    /// Allows 'join ... on 1 equals 1'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Join: other: Cont< ^R, ^U> * kt: System.Func< ^T, int> * ku: System.Func< ^U, int> * f: System.Func< ^T, ^U, ^V> -> Cont< ^R, ^V>

// Monad
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.
    ///
    /// Allows 'select'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline SelectMany: f: System.Func< ^T, Cont< ^R, ^U>> -> Cont< ^R, ^U>

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.
    ///
    /// Allows nested 'from'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline SelectMany: f: System.Func< ^T, Cont< ^R, ^U>> * g: System.Func< ^T, ^U, ^V> -> Cont< ^R, ^V>