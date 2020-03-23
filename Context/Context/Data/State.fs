namespace Rogz.Context.Data.State

[<Struct>]
type SVPair<'S, 'V> = { State: 'S; Value: 'V } with

    member inline s.With state : SVPair< ^S, ^V> =
        { SVPair.State = state; Value = s.Value }
    member inline s.With value : SVPair< ^S, ^V> = 
        { SVPair.State = s.State; Value = value }


[<Struct; NoComparison; NoEquality>]
type State<'S, 'T> = State of ('S -> SVPair<'S, 'T>) with

// Function
    member inline s.Invoke(state) = let (State f) = s in f state

// Functor
    member inline s.Select(f: System.Func< ^T, ^U>) : State< ^S, ^U> =
        let (State st) = s
        State (fun s -> let sv = st s in { SVPair.State = sv.State; Value = f.Invoke(sv.Value) })

// Applicative
    member inline s.Zip((State s2), f: System.Func< ^T, ^U, ^V>) : State< ^S, ^V> =
        let (State s1) = s
        State (fun s ->
            let sv1 = s1 s
            let sv2 = s2 sv1.State
            { SVPair.State = sv2.State; Value = f.Invoke(sv1.Value, sv2.Value) })

    member inline s.Join((State s2), _: System.Func< ^T, int>, _: System.Func< ^U, int>, f: System.Func< ^T, ^U, ^V>) : State< ^S, ^V> =
        let (State s1) = s
        State (fun s ->
            let sv1 = s1 s
            let sv2 = s2 sv1.State
            { SVPair.State = sv2.State; Value = f.Invoke(sv1.Value, sv2.Value) })

// Monad
    member inline s.SelectMany(f: System.Func< ^T, State< ^S, ^U>>) : State< ^S, ^U> =
        let (State st) = s
        State (fun s ->
            let s1 = st s
            let (State st) = f.Invoke(s1.Value) in st s1.State)


    member inline s.SelectMany(f: System.Func< ^T, State< ^S, ^U>>, g: System.Func< ^T, ^U, ^V>) : State< ^S, ^V> =
        let (State st) = s
        State (fun s ->
            let sa = st s
            let (State st) = f.Invoke(sa.Value)
            let sb = st sa.State
            { SVPair.State = sb.State; Value = g.Invoke(sa.Value, sb.Value) })