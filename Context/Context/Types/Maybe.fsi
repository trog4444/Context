namespace Rogz.Context.Data.Maybe

/// <summary>The type of optional value. 'Just a' represents a value 'a',
/// while 'Nothing' represents cases such as when a value does not exist or can't be reached.</summary>
[<Struct>]
type Maybe<'T> = Nothing | Just of ^T with

// Union

    /// <summary>Acts as a pattern-match on a union-type, calling the appropriate function based on the case.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Match: fNothing: System.Func< ^U> * fJust: System.Func< ^T, ^U> -> ^U

    /// <summary>Acts as a pattern-match on a union-type, calling the appropriate action based on the case.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the action attempting to be called is null.</exception>
    member inline Match: nothingAction: System.Action * justAction: System.Action< ^T> -> unit

// Functor
    /// <summary>Lift a function onto a context.
    ///
    /// Allows 'select'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Select: f: System.Func< ^T, ^U> -> Maybe< ^U>

// Applicative W some MonadPlus in Join
    /// <summary>Lift a binary function onto contexts.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Zip: other: Maybe< ^U> * f: System.Func< ^T, ^U, ^V> -> Maybe< ^V>
    
    /// <summary>Lift a binary function onto contexts, or return a 'failure' value if the key-functions don't equate.
    ///
    /// Allows 'join ... on x equals b'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Join: other: Maybe< ^U> * kt: System.Func< ^T, ^K> * ku: System.Func< ^U, ^K> * f: System.Func< ^T, ^U, ^V> -> Maybe< ^V> when ^K: equality

// Alternative
    /// <summary>The identity element of the 'orElse' operation.</summary>
    static member inline Nix: unit -> Maybe< ^a>

    /// <summary>A monoidal, associative binary operation representing choice/failure.</summary>
    member inline OrElse: other: Maybe< ^T> -> Maybe< ^T>

// Monad
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.
    ///
    /// Allows 'select'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline SelectMany: f: System.Func< ^T, Maybe< ^U>> -> Maybe< ^U>
    
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.
    ///
    /// Allows nested 'from'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline SelectMany: f: System.Func< ^T, Maybe< ^U>> * g: System.Func< ^T, ^U, ^V> -> Maybe< ^V>

// MonadPlus
    /// <summary>Acts similar to a SQL 'inner join', combining elements of each given monad when the elements satisfy a predicate.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline GroupJoin: other: Maybe< ^U> * p: System.Func< ^T, ^U, bool> * f: System.Func< ^T, ^U, ^V> -> Maybe< ^V>
    
    /// <summary>Acts similar to a SQL 'inner join', combining elements of each given monad when the elements satisfy a predicate.
    ///
    /// Allows 'join ... into ...'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline GroupJoin: other: Maybe< ^U> * kt: System.Func< ^T, ^K> * ku: System.Func< ^U, ^K> * f: System.Func< ^T, Maybe< ^U>, ^V> -> Maybe< ^V> when ^K: equality

// MonadPlus.General
    /// <summary>Generalizes the sequence-based 'filter' function.
    ///
    /// Allows 'where'-clauses in query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Where: p: System.Func< ^T, bool> -> Maybe< ^T>

// Semigroup
    /// <summary>An associative binary operation on contexts.</summary>
    static member inline Append: first: Maybe< ^a> * second: Maybe< ^a> -> Maybe< ^a>
        when ^a: (static member Append: ^a -> ^a -> ^a)

// Monoid
    /// <summary>The identity element of the 'append' operation.</summary>
    static member inline Empty: unit -> Maybe< ^a>

// Foldable
    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Fold: seed: ^S * f: System.Func< ^S, ^T, ^S> -> ^S

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline FoldBack: seed: ^S * f: System.Func< ^T, ^S, ^S> -> ^S