namespace Rogz.Context.Data.State


module State =

// Interop

    let inline fromFunc (f: System.Func< ^s, SVPair< ^s, ^a>>) =
        State (fun s -> f.Invoke(s))


// Minimal

    let get<'s> : State< ^s, ^s> = State (fun s -> { SVPair.State = s; Value = s })

    let put (state: 's) = State (fun _ -> { State = state; Value = () })

    let inline modify modification : State< ^s, unit> =
        State (fun s -> { SVPair.State = modification s; Value = () })

    let inline gets (proj: ^s -> ^a) =
        State (fun s -> { SVPair.State = s; Value = proj s })


// Primitives

    let inline runState initial (State f) : SVPair< ^s, ^a> = f initial

    let inline evalState (initial: ^s) (State f) : ^a = (f initial).Value

    let inline execState initial (State (f: ^s -> SVPair< ^s, ^a>)) : ^s =
        (f initial).State

    let inline mapState mapping (State f) : State< ^s, ^b> =
        State (fun s -> mapping ((f s): SVPair< ^s, ^a>))

    let inline withState f (State st) : State< ^s, ^a> =
        State (fun s -> st (f s))

    let inline cache (State f) : State< ^s, ^a> when ^s: equality =
        let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        State (fun s -> match d.TryGetValue(s) with
                        | true, r  -> r
                        | false, _ -> let r = f s in d.[s] <- r; r)


// Functor

    let inline map (f: ^a -> ^b) (State st) =
        State (fun (s: ^s) ->
            let sa = st s in { SVPair.State = sa.State; Value = f sa.Value })


// Applicative

    let unit (value: 'a) : State<'s, ^a> =
        State (fun s -> { SVPair.State = s; Value = value })

    let inline ap (State fv) (State ff) : State< ^s, ^b> =
        State (fun s -> let sf = ff s in let sv = fv sf.State
                        { SVPair.State = sv.State
                        ; Value = sf.Value (sv.Value: ^a) })

    let inline map2 (f: ^a -> ^b -> ^c) (State fa) (State fb) : State< ^s, ^c> =
        State (fun s -> let sa = fa s in let sb = fb sa.State
                        { SVPair.State = sb.State
                        ; Value = f sa.Value sb.Value })

    let inline andthen (State fb) (State (fa: ^s -> SVPair< ^s, ^a>)) : State< ^s, ^b> =
        State (fun s -> fb (fa s).State)

    let inline sequence (source: seq<State< ^s, ^a>>) : State< ^s, seq< ^a>> =
        State (fun s ->
            let mutable s = s
            let xs = ResizeArray<_>()
            for (State f) in source do
                let sv = f s
                s <- sv.State
                xs.Add(sv.Value)
            { SVPair.State = s; Value = System.Linq.Enumerable.AsEnumerable(xs) })

    let inline traverse (f: ^a -> State< ^s, ^b>) (source: seq< ^a>) : State< ^s, seq< ^b>> =
        sequence (System.Linq.Enumerable.Select(source, fun a -> f a))


// Monad

    let inline bind (f: ^a -> State< ^s, ^b>) (State m) =
        State (fun s -> let sa = m s in runState sa.State (f sa.Value))

    let inline flatten (State mm) : State< ^s, ^a> =
        State (fun s -> let m = mm s in runState m.State m.Value)    

    let inline fixM loop (em: Rogz.Context.Data.Either.Either< ^a, State< ^s, ^a>>) : State< ^s, ^b> =
        State (fun s ->
            let mutable s = s
            let rec go (State st) =
                let sv = st s
                s <- sv.State
                k sv.Value
            and k a = loop k go a
            let (State st) = match em with
                             | Rogz.Context.Data.Either.Left a  -> k a
                             | Rogz.Context.Data.Either.Right m -> go m in st s)


    // foldlM
    // foldrM

    [<RequireQualifiedAccess>]
    module Workflow =

        type StateBuilder() =
            member inline _.Return(x: ^a) : State< ^s, ^a> = unit x
            member inline _.ReturnFrom(m) : State< ^s, ^a> = m
            member inline _.Bind(m, f: ^a -> State< ^s, ^b>) = bind f m
            member inline _.Zero() : State< ^s, unit> = unit ()
            member inline _.Using(disp: ^d, f: ^d -> State< ^s, ^a>) : State< ^s, ^a> when ^d :> System.IDisposable = using disp f
            member inline _.TryWith(m, handler: exn -> State< ^s, ^a>) : State< ^s, ^a> = try m with e -> handler e
            member inline _.TryFinally(m, finalizer: unit -> unit) : State< ^s, ^a> = try m finally finalizer ()


    let state = Workflow.StateBuilder()