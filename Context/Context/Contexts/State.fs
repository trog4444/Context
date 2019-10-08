namespace PTR.Context.Type.State


[<Struct>]
type StateValue<'State, 'Value> = { State: 'State ; Value: 'Value } with

    member inline s.With(state: ^State) = { StateValue.State = state ; Value = s.Value }
    member inline s.With(value: ^Value) = { StateValue.State = s.State ; Value = value }


[<Struct; NoComparison; NoEquality>]
type State<'State, 'Value> = State of ('State -> StateValue<'State, 'Value>) with
    
    member inline s.Invoke(state: ^State) = let (State f) = s in f state

    static member inline Unit(x: ^a) : State< ^s, ^a> =
        State (fun s -> { StateValue.State = s ; Value = x })
    
    member inline s.Select(f: System.Func< ^Value, ^NextValue>) : State< ^State, ^NextValue> =
        let (State sf) = s in State (fun s -> let sv = sf s in { StateValue.State = sv.State ; Value = f.Invoke(sv.Value) })

    member inline s.Select2((State s2), f: System.Func< ^Value, ^NextValue, ^FinalValue>) : State< ^State, ^FinalValue> =
        let (State s1) = s
        State (fun s ->
            let sa = s1 s
            let sb = s2 sa.State
            { StateValue.State = sb.State ; Value = f.Invoke(sa.Value, sb.Value) })

    member inline s.SelectMany(f: System.Func< ^Value, State< ^State, ^NextValue>>) : State< ^State, ^NextValue> =
        let (State sf) = s
        State (fun s -> let sv = sf s in f.Invoke(sv.Value).Invoke(sv.State))

    member inline s.SelectMany(f: System.Func< ^Value, State< ^State, ^NextValue>>, g: System.Func< ^Value, ^NextValue, ^FinalValue>) : State< ^State, ^FinalValue> =
        let (State s1) = s
        State (fun s ->
            let st1 = s1 s
            let st2 = f.Invoke(st1.Value).Invoke(st1.State)
            { StateValue.State = st2.State
            ; Value = g.Invoke(st1.Value, st2.Value) })
    
    member inline s.Join(t: State< ^State, ^NextValue>, kt: System.Func< ^Value, ^K>, ku: System.Func< ^NextValue, ^K>, rs: System.Func< ^Value, ^NextValue, ^FinalValue>) : State< ^State, ^FinalValue> =
        s.Select2(t, rs)

    member inline s.ContinueWith(f: System.Func<State< ^State, ^Value>, ^NewValue>) : State< ^State, ^NewValue> =
        let st = s in State (fun s -> { StateValue.State = s ; Value = f.Invoke(st) })

    static member inline Append((State s1), State s2) : State< ^s, ^a> =
        State (fun s ->
            let sa = s1 s
            let sb = s2 sa.State
            { sb with StateValue.Value = (^a : (static member Append: ^a -> ^a -> ^a) (sa.Value, sb.Value)) })


module State =

    module Pattern =

        let inline ( |SVPair| ) (sv: StateValue< ^s, ^a>) =
            SVPair (struct (sv.State, sv.Value))


// Primitives

    [<CompiledName("Make")>]
    let inline make (state: System.Func< ^s, System.ValueTuple< ^s, ^a>>) : State< ^s, ^a> =
        State (fun s -> let (struct (s, v)) = state.Invoke(s) in { StateValue.State = s ; Value = v })

    [<CompiledName("RunState")>]
    let runState (s: 's) (State (sa: ^s -> StateValue< ^s, 'a>)) : StateValue< ^s, ^a> = sa s

    [<CompiledName("EvalState")>]
    let evalState (s: 's) (State (sa: ^s -> StateValue< ^s, 'a>)) : ^a = (sa s).Value

    [<CompiledName("ExecState")>]
    let execState (s: 's) (State (sa: ^s -> StateValue< ^s, 'a>)) : ^s = (sa s).State

    [<CompiledName("MapState")>]
    let inline mapState (f: ^s -> ^a -> ^s * ^b) (State st) : State< ^s, ^b> =
        State (fun s0 ->
            let st = st s0
            let s, v = f st.State st.Value
            { StateValue.State = s ; Value = v })

    [<CompiledName("WithState")>]
    let inline withState f (State st) : State< ^s, ^a> = State (fun s -> st (f s))

    [<CompiledName("Get")>]
    let get<'s> : State< ^s, ^s> = State (fun s -> { StateValue.State = s ; Value = s })

    [<CompiledName("Put")>]
    let put (s: 's) : State< ^s, unit> = State (fun _ -> { StateValue.State = s ; Value = () })

    [<CompiledName("Modify")>]
    let inline modify f = State (fun (s: ^s) -> { StateValue.State = f s ; Value = () })

    [<CompiledName("CacheState")>]
    let inline cacheState (State st) : State< ^s, ^a> when ^s : equality =
        let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        let sf s =
            match d.TryGetValue(s) with
            | true, sv -> sv
            | false, _ -> let sv = st s in d.[s] <- sv ; sv
        State sf


// Isomorphisms

    [<CompiledName("NewSV")>]
    let inline newSV (state: ^s) (value: ^a) : StateValue< ^s, ^a> =
        { StateValue.State = state ; Value = value }


// Monad

    let inline unit (x: ^a) : State< ^s, ^a> =
        State (fun s -> { StateValue.State = s ; Value = x })

    [<CompiledName("Bind")>]
    let inline bind (f: ^a -> State< ^s, ^b>) (State st) =
        State (fun s ->
            let sv = st s in let (State st') = f sv.Value in st' sv.State)

    [<CompiledName("Flatten")>]
    let inline flatten (State mm) : State< ^s, ^a> =
        State (fun s ->
            let s_st = mm s in let (State st) = s_st.Value in st s_st.State)

    [<CompiledName("RecM")>]
    let inline recM f x : State< ^s, ^b> =
        State(fun s ->
            let mutable s = s
            let bind' k (State st) =
                let sv = st s
                s <- sv.State
                k sv.Value
            let rec go m = bind' j m
            and k a = go (unit a)
            and j a = f k a
            { runState s (j x) with StateValue.State = s })

    [<CompiledName("RecM1")>]
    let inline recM1 f (x: ^a) : State< ^s, ^b> =
        State (fun s ->
            let mutable s = s
            let bind' k (State st) =
                let sv = st s in s <- sv.State
                k sv.Value
            let rec go m = bind' k m
            and k a = f go a
            { runState s (k x) with StateValue.State = s })

    //[<CompiledName("FoldrM")>]
    //let inline foldrM (f: ^a -> ^z -> State< ^s, ^z>) (s0: ^z) (source: ^a seq) =
    //    let g k x s = bind k (f x s)
    //    match source with
    //    | :? array< ^a> as s -> Array.fold g wrap s s0
    //    | :? list<  ^a> as s -> List.fold  g wrap s s0
    //    | _ -> Seq.fold g wrap source s0
    //
    //[<CompiledName("FoldlM")>]
    //let inline foldlM (f: ^z -> ^a -> State< ^s, ^z>) (s0: ^z) (source: ^a seq) =
    //    let g x k s = bind k (f s x)
    //    match source with
    //    | :? array< ^a> as s -> Array.foldBack g s wrap s0
    //    | :? list<  ^a> as s -> List.foldBack  g s wrap s0
    //    | _ -> Seq.foldBack g source wrap s0


    module Workflow =

        type StateBuilder () =
            member inline _.Bind(m: State< ^s, ^a>, f: (^a -> State< ^s, ^b>)) = bind f m
            member inline _.Return x : State< ^s, ^a> = unit x
            member inline _.ReturnFrom m : State< ^s, ^a> = m
            member inline _.Zero() : State< ^s, unit> = unit ()
 
            member inline _.TryWith(body, handler) : State< ^s, ^a> = try body with e -> handler e
            member inline _.TryFinally(body, finalizer) : State< ^s, ^a> = try body finally finalizer ()
 
            member inline _.Using(disp: ^d, body) : State< ^s, ^a> when ^d :> System.IDisposable =
                using disp body
 
            member inline _.While(guard, body) : State< ^s, unit> =                    
                let rec go = function
                | false -> unit ()
                | true  -> bind k (body ())
                and k () = go (guard ()) in k ()
 
            member inline _.For(seq: #seq< ^b>, body) : State< ^s, unit> =
                use e = seq.GetEnumerator()
                let rec go = function
                | false -> unit ()
                | true  -> b e.Current
                and b x = bind k (body x)
                and k () = go (e.MoveNext()) in k ()


    let state = Workflow.StateBuilder ()


// Applicative
            
    [<CompiledName("Ap")>]
    let inline ap (State sv) (State sf) : State< ^s, ^b> =
        State (fun s ->
            let sf = sf s
            let sv : StateValue< ^s, ^a> = sv sf.State
            { StateValue.State = sv.State
            ; Value = sf.Value sv.Value })

    [<CompiledName("Map2")>]
    let inline map2 (f: ^a -> ^b -> ^c) (State sa) (State sb) : State< ^s, ^c> =
        State (fun s ->
            let sa = sa s
            let sb = sb sa.State
            { StateValue.State = sb.State
            ; Value = f sa.Value sb.Value })

    [<CompiledName("AndThen")>]
    let inline andThen (State sb) (State (sa: ^s -> StateValue< ^s, ^a>)) : State< ^s, ^b> =
        State (fun s -> sb ((sa s).State))

    [<CompiledName("When")>]
    let inline when_ condition f : State< ^s, unit> =
        if condition then f () else unit ()

    //[<CompiledName("FilterA")>]
    //let inline filterA p (source: ^a seq) : State< ^s, ^a seq> =
    //    State (fun s0 ->
    //        let mutable st = s0
    //        let xs =
    //            match source with
    //            | :? array< ^a> as s ->
    //                [| for i = 0 to s.Length - 1 do
    //                        let (State f) = p s.[i]
    //                        let sv = f st in st <- sv.State
    //                        if sv.Value then yield s.[i] |] :> _ seq
    //            | :? list< ^a> as s ->
    //                [| for x in s do
    //                        let (State f) = p x
    //                        let sv = f st in st <- sv.State
    //                        if sv.Value then yield x |] :> _ seq
    //            | :? ResizeArray< ^a> as s ->
    //                [| for i = 0 to s.Count - 1 do
    //                        let (State f) = p s.[i] in let sv = f st
    //                        st <- sv.State
    //                        if sv.Value then yield s.[i] |] :> _ seq
    //            | _ ->
    //                let ys = seq {
    //                    for x in source do
    //                        let (State s) = p x
    //                        let sv = s st in st <- sv.State
    //                        if sv.Value then yield x } |> Seq.cache
    //                do for _ in ys do ()
    //                ys
    //        { StateValue.State = st ; Value = xs })    
    //
    //[<CompiledName("ZipWithA")>]
    //let inline zipWithA f (source1: #seq< ^a>) (source2: #seq< ^b>) : State< ^s, ^c seq> =
    //    sequenceA (System.Linq.Enumerable.Zip(source1, source2, System.Func<_,_,_>f))
    //
    //[<CompiledName("ReplicateA")>]
    //let inline replicateA count (State f) : State< ^s, ^a seq> =
    //    State (fun s0 ->
    //        let mutable st = s0
    //        let xs = [| for i = 0 to count - 1 do
    //                        let sv = f st in st <- sv.State
    //                        yield sv.Value |]
    //        { StateValue.State = st ; Value = xs })


// Functor

    [<CompiledName("Map")>]
    let inline map (f: ^a -> ^b) (State st) : State< ^s, ^b> =
        State (fun s ->
            let sa = st s
            { StateValue.State = sa.State
            ; Value = f sa.Value })


// Semigroup

    let inline append (State sa) (State sb) : State< ^s, ^a>
        when ^a: (static member inline Append: ^a -> ^a -> ^a) =
            State (fun s ->
                let sa = sa s
                let sb = sb sa.State
                { sb with
                    StateValue.Value = (^a: (static member inline Append: ^a -> ^a -> ^a) (sa.Value, sb.Value)) })


// Traversable

    [<CompiledName("Sequence")>]
    let inline sequence (source: #seq<State< ^s, ^a>>) : State< ^s, ^a seq> =
        State (fun s0 ->
            let mutable s = s0
            let ra = ResizeArray<_>()
            for (State st) in source do
                let sv = st s
                s <- sv.State
                ra.Add(sv.Value)
            { StateValue.State = s ; Value = ra :> _ seq })

    [<CompiledName("Traverse")>]
    let inline traverse (f: ^a -> State< ^s, ^b>) (source: #seq< ^a>) : State< ^s, ^b seq> =
        State (fun s0 ->
            let mutable s = s0
            let ra = ResizeArray<_>()
            for x in source do
                let sv = runState s (f x)
                s <- sv.State
                ra.Add(sv.Value)
            { StateValue.State = s ; Value = ra :> _ seq })