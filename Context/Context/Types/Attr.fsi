namespace Rogz.Context.Data.Attr

/// <summary>Represents a type that is 'tagged' with an
/// 'attribute' (i.e. extra type information),
/// which is not used in actual calculations.
/// The purpose is to add type-safety.</summary>
[<Struct>]
type Attr<'Attribute, 'T> = Attr of 'T with

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

// Functor
    /// <summary>Lift a function onto a context.
    ///
    /// Allows 'select'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Select: f: System.Func< ^T, ^U> -> Attr< ^Attribute, ^U>

// Applicative
    /// <summary>Lift a binary function onto contexts.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Zip: other: Attr< ^Attribute, ^U> * f: System.Func< ^T, ^U, ^V> -> Attr< ^Attribute, ^V>
    
    /// <summary>Lift a binary function onto contexts.
    ///
    /// Allows 'join ... on 1 equals 1'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Join: other: Attr< ^Attribute, ^U> * kt: System.Func< ^T, int> * ku: System.Func< ^U, int> * f: System.Func< ^T, ^U, ^V> -> Attr< ^Attribute, ^V>

// Monad
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.
    ///
    /// Allows 'select'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline SelectMany: f: System.Func< ^T, Attr< ^Attribute, ^U>> -> Attr< ^Attribute, ^U>
    
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.
    ///
    /// Allows nested 'from'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline SelectMany: f: System.Func< ^T, Attr< ^Attribute, ^U>> * g: System.Func< ^T, ^U, ^V> -> Attr< ^Attribute, ^V>

// Comonad
    /// <summary>Sequentially compose two co-contexts, passing any value produced by the first as an argument to the second.</summary>
    member inline ContinueWith: f: System.Func<Attr< ^Attribute, ^T>, ^U> -> Attr< ^Attribute, ^U>

// Semigroup
    /// <summary>An associative binary operation on contexts.</summary>
    static member inline Append: first: Attr< ^t, ^a> * second: Attr< ^t, ^a> -> Attr< ^t, ^a>
        when ^a: (static member Append: ^a -> ^a -> ^a)

// Foldable
    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Fold: seed: ^S * f: System.Func< ^S, ^T, ^S> -> ^S

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline FoldBack: seed: ^S * f: System.Func< ^T, ^S, ^S> -> ^S