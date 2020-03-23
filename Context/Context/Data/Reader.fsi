namespace Rogz.Context.Data.Reader

/// <summary>Represents one or more computations that share an input 'environment'.</summary>
[<Struct; NoComparison; NoEquality>]
type Reader<'Env, 'T> = Reader of ('Env -> 'T) with

// Function
    /// <summary>The result of running a CPS computation with a given final continuation.</summary>
    member inline Invoke: env: ^Env -> ^T

// Functor
    /// <summary>Lift a function onto a context.
    ///
    /// Allows 'select'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Select: f: System.Func< ^T, ^U> -> Reader< ^Env, ^U>

// Profunctor
    /// <summary>Map over both arguments at the same time,
    /// the first (i.e. 'left') contravariantly
    /// and the second (i.e. 'right') covariantly.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline DiSelect: f: System.Func< ^PreEnv, ^Env> * g: System.Func< ^T, ^U> -> Reader< ^PreEnv, ^U>

// Applicative
    /// <summary>Lift a binary function onto contexts.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Zip: other: Reader< ^Env, ^U> * f: System.Func< ^T, ^U, ^V> -> Reader< ^Env, ^V>

    /// <summary>Lift a binary function onto contexts.
    ///
    /// Allows 'join ... on 1 equals 1'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Join: other: Reader< ^Env, ^U> * kt: System.Func< ^T, int> * ku: System.Func< ^U, int> * f: System.Func< ^T, ^U, ^V> -> Reader< ^Env, ^V>

// Monad
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.
    ///
    /// Allows 'select'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline SelectMany: f: System.Func< ^T, Reader< ^Env, ^U>> -> Reader< ^Env, ^U>

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.
    ///
    /// Allows nested 'from'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline SelectMany: f: System.Func< ^T, Reader< ^Env, ^U>> * g: System.Func< ^T, ^U, ^V> -> Reader< ^Env, ^V>

//// Comonad
//    /// <summary>Sequentially compose two co-contexts, passing any value produced by the first as an argument to the second.</summary>
//    member inline ContinueWith: f: System.Func<Reader< ^Env, ^T>, ^U> -> Reader< ^Env, ^U>

// Semigroup
    /// <summary>An associative binary operation on contexts.</summary>
    static member inline Append: first: Reader< ^e, ^a> * second: Reader< ^e, ^a> -> Reader< ^e, ^a>
        when ^a: (static member Append: ^a -> ^a -> ^a)