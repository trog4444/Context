namespace Rogz.Context.Data.Tagged


/// <summary>Represents a type that is 'tagged' with extra type information,
/// which is not used in actual calculations.</summary>
[<Struct>]
type Tagged<'Attribute, 'T> = Tagged of 'T with

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
    member inline Select: f: System.Func< ^T, ^U> -> Tagged< ^Attribute, ^U>

// Applicative
    /// <summary>Lift a binary function onto contexts.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Zip: other: Tagged< ^Attribute, ^U> * f: System.Func< ^T, ^U, ^V> -> Tagged< ^Attribute, ^V>
    
    /// <summary>Lift a binary function onto contexts.
    ///
    /// Allows 'join ... on 1 equals 1'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Join: other: Tagged< ^Attribute, ^U> * kt: System.Func< ^T, int> * ku: System.Func< ^U, int> * f: System.Func< ^T, ^U, ^V> -> Tagged< ^Attribute, ^V>

// Monad
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.
    ///
    /// Allows 'select'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline SelectMany: f: System.Func< ^T, Tagged< ^Attribute, ^U>> -> Tagged< ^Attribute, ^U>
    
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.
    ///
    /// Allows nested 'from'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline SelectMany: f: System.Func< ^T, Tagged< ^Attribute, ^U>> * g: System.Func< ^T, ^U, ^V> -> Tagged< ^Attribute, ^V>

// Comonad
    /// <summary>Sequentially compose two co-contexts, passing any value produced by the first as an argument to the second.</summary>
    member inline ContinueWith: f: System.Func<Tagged< ^Attribute, ^T>, ^U> -> Tagged< ^Attribute, ^U>

// Semigroup
    /// <summary>An associative binary operation on contexts.</summary>
    static member inline Append: first: Tagged< ^t, ^a> * second: Tagged< ^t, ^a> -> Tagged< ^t, ^a>
        when ^a: (static member Append: ^a -> ^a -> ^a)

// Foldable
    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Fold: seed: ^S * f: System.Func< ^S, ^T, ^S> -> ^S

    /// <summary>Applies a function to all element(s) of the source, threading an accumulator argument through the computation.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline FoldBack: seed: ^S * f: System.Func< ^T, ^S, ^S> -> ^S