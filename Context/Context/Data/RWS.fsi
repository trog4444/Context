namespace Rogz.Context.Data.RWS


/// <summary>The result of an 'RWS' computation.</summary>
[<Struct>]
type RWSResult<'State, 'Log, 'T> =
    { State: 'State
    ; Log:   'Log
    ; Value: 'T }


/// <summary>A computation taking an environment and an initial state
/// which produceds a new state, a 'log', and a value.</summary>
[<Struct; NoComparison; NoEquality>]
type RWS<'Env, 'State, 'Log, 'T> = RWS of ('Env -> 'State -> RWSResult<'State, 'Log, 'T>) with

// Function
    /// <summary>Execute the given computation.</summary>
    member inline Invoke: env: ^Env * state: ^State -> RWSResult< ^State, ^Log, ^T>

// Functor
    /// <summary>Lift a function onto a context.
    ///
    /// Allows 'select'-clauses query expressions.</summary>
    /// <exception cref="ArgumentNullException">Thrown when the function attempting to be called is null.</exception>
    member inline Select: f: System.Func< ^T, ^U> -> RWS< ^Env, ^State, ^Log, ^U>