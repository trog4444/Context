namespace Rogz.Context.Data.State

/// <summary>Represents a state-value pair.</summary>
[<Struct>]
type SVPair<'S, 'V> = { State: 'S; Value: 'V } with

// Primitive
    /// <summary>Modify a state-value pair with a new state.</summary>
    member inline With: state: ^S -> SVPair< ^S, ^V>
    /// <summary>Modify a state-value pair with a new value.</summary>
    member inline With: value: ^V -> SVPair< ^S, ^V>


/// <summary>Represents a 'stateful'-computation, threading each new state as an argument in a resultant state-value pair.</summary>
[<Struct; NoComparison; NoEquality>]
type State<'S, 'T> = State of ('S -> SVPair<'S, 'T>) with

// Function
    /// <summary>Execute the given stateful computation.</summary>
    member inline Invoke: state: ^S -> SVPair< ^S, ^T>

// Functor
    /// <summary>Lift a function onto a context.
    ///
    /// Allows 'select'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Select: f: System.Func< ^T, ^U> -> State< ^S, ^U>

// Applicative
    /// <summary>Lift a binary function onto contexts.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Zip: other: State< ^S, ^U> * f: System.Func< ^T, ^U, ^V> -> State< ^S, ^V>

    /// <summary>Lift a binary function onto contexts.
    ///
    /// Allows 'join ... on 1 equals 1'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Join: other: State< ^S, ^U> * kt: System.Func< ^T, int> * ku: System.Func< ^U, int> * f: System.Func< ^T, ^U, ^V> -> State< ^S, ^V>

// Monad
    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.
    ///
    /// Allows 'select'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline SelectMany: f: System.Func< ^T, State< ^S, ^U>> -> State< ^S, ^U>

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.
    ///
    /// Allows nested 'from'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline SelectMany: f: System.Func< ^T, State< ^S, ^U>> * g: System.Func< ^T, ^U, ^V> -> State< ^S, ^V>