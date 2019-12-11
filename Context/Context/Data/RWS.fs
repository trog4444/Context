namespace Rogz.Context.Data.RWS


[<Struct>]
type RWSResult<'State, 'Log, 'T> =
    { State: 'State
    ; Log:   'Log
    ; Value: 'T }


[<Struct; NoComparison; NoEquality>]
type RWS<'Env, 'State, 'Log, 'T> = RWS of ('Env -> 'State -> RWSResult<'State, 'Log, 'T>) with

// Function
    member inline s.Invoke(env, state) = let (RWS r) = s in r env state

// Functor
    member inline s.Select(f: System.Func< ^T, ^U>) =
        let (RWS r) = s
        RWS (fun e s ->
            let ra = r e s
            { RWSResult.State = ra.State
            ; Log = ra.Log
            ; Value = f.Invoke(ra.Value) })