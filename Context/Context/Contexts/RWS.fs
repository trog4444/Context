namespace PTR.Context.Type.RWS


[<Struct>]
type RWSResult<'State, 'Log, 'Value> =
    { State: 'State
    ; Log:   'Log
    ; Value: 'Value } with

       member inline s.With(state: ^State) = { RWSResult.State = state ; Log = s.Log ; Value = s.Value}
       member inline s.With(state: ^State, log: ^Log) = { RWSResult.State = state ; Log = log ; Value = s.Value }              
       member inline s.With(log: ^Log) = { RWSResult.State = s.State ; Log = log ; Value = s.Value }       
       member inline s.With(log: ^Log, value: ^Value) = { RWSResult.State = s.State ; Log = log ; Value = value }       
       member inline s.With(value: ^Value) = { RWSResult.State = s.State ; Log = s.Log ; Value = value }

       member inline s.Apply(f: System.Func< ^State, ^Log, ^Value, ^Result>) = f.Invoke(s.State, s.Log, s.Value)


[<Struct; NoComparison; NoEquality>]
type RWS<'Env, 'State, 'Log, 'Result> =
    RWS of ('Env -> 'State -> RWSResult<'State, 'Log, 'Result>) with

    member inline s.Invoke(env: ^Env, state: ^State) : RWSResult< ^State, ^Log, ^Result> =
        let (RWS r) = s in r env state
    
    static member inline Unit(x: ^a) : RWS< ^e, ^s, ^w, ^a> =
        RWS (fun _ s ->
            { RWSResult.State = s
            ; Log = (^w : (static member Empty: unit -> ^w) ())
            ; Value = x })

    member inline s.Select(f: System.Func< ^Result, ^NextResult>) : RWS< ^Env, ^State, ^Log, ^NextResult> =
        let (RWS r) = s
        RWS (fun e s ->
            let rws = r e s
            { RWSResult.State = rws.State
            ; Log = rws.Log
            ; Value = f.Invoke(rws.Value) })


module RWS =

    let inline private append' a b = (^w: (static member Append : ^w -> ^w -> ^w) (a, b))
    let inline private empty' ()   = (^w: (static member Empty  : unit -> ^w) ())


// Primitives

    [<CompiledName("Make")>]
    let inline make (rws: System.Func< ^e, ^s, System.ValueTuple< ^s, ^w, ^a>>) : RWS< ^e, ^s, ^w, ^a> =
        RWS (fun e s -> let (struct (s, w, a)) = rws.Invoke(e, s)
                        { RWSResult.State = s ; Log = w ; Value = a })

    [<CompiledName("RunRWS")>]
    let inline runRWS (e: ^e) (s: ^s) (RWS r) : RWSResult< ^s, ^w, ^r> = r e s

    [<CompiledName("EvalRWS")>]
    let inline evalRWS (e: ^e) (s: ^s) (RWS r) : ^w * ^r =
        let wv = r e s in wv.Log, wv.Value

    [<CompiledName("ExecRWS")>]
    let inline execRWS (e: ^e) (s: ^s) (RWS (r: ^e -> ^s -> RWSResult< ^s, ^w, ^r>)) : ^s * ^w =
        let sw = r e s in sw.State, sw.Log

    [<CompiledName("MapRWS")>]
    let inline mapRWS (f: ^s -> ^w1 -> ^a -> RWSResult< ^s, ^w2, ^b>) (RWS r) =
        RWS (fun (e: ^e) s -> let swv = r e s in f swv.State swv.Log swv.Value)

    [<CompiledName("WithRWS")>]
    let inline withRWS (f: ^e0 -> ^s -> ^e * ^s) (RWS r) : RWS< ^e0, ^s, ^w, ^a> =
        RWS (fun e s -> let e', s' = f e s in r e' s')

    [<CompiledName("Put")>]
    let inline put (s: ^s) : RWS< ^e, ^s, ^w, unit> =
        RWS (fun _ _ ->
            { RWSResult.State = s
            ; Log = empty' ()
            ; Value = () })

    [<CompiledName("Get")>]
    let inline get () : RWS< ^e, ^s, ^w, ^s> =
        RWS (fun _ s ->
            { RWSResult.State = s
            ; Log = empty' ()
            ; Value = s })

    [<CompiledName("Modify")>]
    let inline modify f : RWS< ^e, ^s, ^w, unit> =
        RWS (fun _ s ->
            { RWSResult.State = f s
            ; Log = empty' ()
            ; Value = () })

    [<CompiledName("Ask")>]
    let inline ask () : RWS< ^e, ^s, ^w, ^e> =
        RWS (fun e s ->
            { RWSResult.State = s
            ; Log = empty' ()
            ; Value = e })

    [<CompiledName("Local")>]
    let inline local (localize: ^e -> ^e) (RWS rf) : RWS< ^e, ^s, ^w, ^a> =
        RWS (fun e s -> rf (localize e) s)

    [<CompiledName("Tell")>]
    let tell (entry: 'w) : RWS<'e, 's, ^w, unit> =
        RWS (fun _ s ->
            { RWSResult.State = s
            ; Log = entry
            ; Value = () })    

    [<CompiledName("CacheRWS")>]
    let inline cacheRWS (RWS r) : RWS< ^e, ^s, ^w, ^r> when ^e : equality and ^s : equality =
        let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        RWS (fun e s -> let p = struct (e, s)
                        match d.TryGetValue(p) with
                        | true, res -> res
                        | false, _  -> let res = r e s in d.[p] <- res ; res)


// Monad

    let inline unit x : RWS< ^e, ^s, ^w, ^a> =
        RWS (fun _ s ->
            { RWSResult.State = s
            ; Log = empty' ()
            ; Value = x })

    [<CompiledName("Bind")>]
    let inline bind (f: ^a -> RWS< ^e, ^s, ^w, ^b>) (RWS r) =
        let g e s =
            let swa = r e s
            let swb = runRWS e swa.State (f swa.Value)
            { swb with RWSResult.Log = append' swa.Log swb.Log } in RWS g

    [<CompiledName("Flatten")>]
    let inline flatten (RWS (r: ^e -> ^s -> RWSResult< ^s, ^w, RWS< ^e, ^s, ^w, ^a>>)) =
        RWS (fun e s ->
            let swa = r e s
            let swb = runRWS e swa.State swa.Value
            { swb with RWSResult.Log = append' swa.Log swb.Log })

    [<CompiledName("RecM")>]
    let inline recM f (x: ^a) : RWS< ^e, ^s, ^w, ^b> =
        RWS (fun e s ->
            let mutable s = s
            let mutable w : ^w = empty' ()
            let bind' k (RWS r) =
                let rws = r e s
                s <- rws.State
                w <- append' w rws.Log
                k rws.Value
            let rec go m = bind' j m
            and k a = go (unit a)
            and j a = f k a
            runRWS e s (j x))

    [<CompiledName("RecM1")>]
    let inline recM1 f (x: ^a) : RWS< ^e, ^s, ^w, ^b> =
        RWS (fun e s ->
            let mutable s = s
            let mutable w : ^w = empty' ()
            let bind' k (RWS r) =
                let rws = r e s
                s <- rws.State
                w <- append' w rws.Log
                k rws.Value
            let rec go m = bind' k m
            and k a = f go a
            { runRWS e s (k x) with RWSResult.Log = w })

    //[<CompiledName("FoldrM")>]
    //let inline foldrM (f: ^a -> ^z -> RWS< ^e, ^s, ^w, ^z>) s0 (source: ^a seq) : RWS< ^e, ^s, ^w, ^z> =
    //    RWS (fun e s ->
    //        let mutable lg = empty ()
    //        let mutable st = s0
    //        let mutable zz = s
    //        let act a () =
    //            let rws = runRWS e zz (f a st)
    //            lg <- append lg rws.Log
    //            st <- rws.Value
    //            zz <- rws.State
    //        match source with
    //        | :? array< ^a> as s -> Array.foldBack act s ()
    //        | :? list<  ^a> as s -> List.foldBack act s ()
    //        | _ -> Seq.foldBack act source ()
    //        { RWSResult.Log = lg ; State = zz ; Value = st })
    //
    //[<CompiledName("FoldlM")>]
    //let inline foldlM (f: ^z -> ^a -> RWS< ^e, ^s, ^w, ^z>) s0 (source: ^a seq) : RWS< ^e, ^s, ^w, ^z> =
    //    RWS (fun e s ->
    //        let mutable lg = empty ()
    //        let mutable st = s0
    //        let mutable zz = s
    //        for x in source do
    //            let rws = runRWS e zz (f st x)
    //            lg <- append lg rws.Log
    //            st <- rws.Value
    //            zz <- rws.State
    //        { RWSResult.Log = lg ; State = zz ; Value = st })


    module Workflow =

        type RWSBuilder () =
            member inline _.Return(x: ^a) : RWS< ^e, ^s, ^w, ^a> = unit x
            member inline _.ReturnFrom m : RWS< ^e, ^s, ^w, ^a> = m
            member inline _.Bind(m, f) : RWS< ^e, ^s, ^w, ^b> = bind f m
            
            member inline _.Zero() = unit ()

            member inline _.Using(disp: ^d, body) : RWS< ^e, ^s, ^w, ^a> when ^d :> System.IDisposable =
                using disp body

            member inline _.TryWith(body, handler) : RWS< ^e, ^s, ^w, ^a> =
                try body with e -> handler e
            member inline _.TryFinally(body, finalizer) : RWS< ^e, ^s, ^w, ^a> =
                try body finally finalizer ()

            member inline _.While(guard, body) : RWS< ^e, ^s, ^w, unit> =
                let rec go = function
                | false -> unit ()
                | true  -> bind k (body ())
                and k () = go (guard ()) in k ()

            member inline _.For(seq: #seq< ^a>, body) : RWS< ^e, ^s, ^w, unit> =
                use e = seq.GetEnumerator()
                let rec go = function
                | false -> unit ()
                | true  -> b e.Current
                and b x = bind k (body x)
                and k () = go (e.MoveNext()) in k ()


    let rws = Workflow.RWSBuilder()


// Applicative

    [<CompiledName("Ap")>]
    let inline ap (RWS rv) (RWS rf) : RWS< ^e, ^s, ^w, ^b>
        when ^w : (static member Append: ^w -> ^w -> ^w) =
        RWS (fun e s ->
            let swf = rf e s
            let swv = rv e swf.State
            { RWSResult.State = swv.State
            ; Log = append' swf.Log swv.Log
            ; Value = swf.Value (swv.Value: ^a) })

    [<CompiledName("Map2")>]
    let inline map2 (f: ^a -> ^b -> ^c) (RWS ra) (RWS rb) : RWS< ^e, ^s, ^w, ^c>
        when ^w : (static member Append: ^w -> ^w -> ^w) =
        RWS (fun e s ->
            let swa = ra e s
            let swb = rb e swa.State
            { RWSResult.State = swb.State
            ; Log = append' swa.Log swb.Log
            ; Value = f swa.Value swb.Value })

    [<CompiledName("AndThen")>]
    let inline andThen fb fa : RWS< ^e, ^s, ^w, ^b> = map2 (fun (_: ^a) b -> b) fa fb

    [<CompiledName("When")>]
    let inline when_ condition f : RWS< ^e, ^s, ^w, unit> =
        if condition then f () else unit ()

    //[<CompiledName("FilterA")>]
    //let inline filterA (p: ^a -> RWS< ^e, ^s, ^w, bool>) (source: ^a seq) : RWS< ^e, ^s, ^w, ^a list>
    //    when ^w : (static member Append: ^w -> ^w -> ^w) and ^w : (static member Empty: unit -> ^w) =
    //    let cons x b xs = if b then x::xs else xs
    //    let inline g x xs = map2 (cons x) (p x) xs
    //    let z = unit []
    //    match source with
    //    | :? array< ^a> as s -> Array.foldBack g s z
    //    | :? list<  ^a> as s -> List.foldBack  g s z
    //    | _ -> Seq.foldBack g source z

    //[<CompiledName("ZipWithA")>]
    //let inline zipWithA f (source1: #seq< ^a>) (source2: #seq< ^b>) : RWS< ^e, ^s, ^w, ^c list>
    //    when ^w : (static member Append: ^w -> ^w -> ^w) and ^w : (static member Empty: unit -> ^w) =
    //    sequenceA (System.Linq.Enumerable.Zip(source1, source2, System.Func<_,_,_>f))

    //[<CompiledName("ReplicateA")>]
    //let inline replicateA count fa : RWS< ^e, ^s, ^w, ^a list>
    //    when ^w : (static member Append: ^w -> ^w -> ^w) and ^w : (static member Empty: unit -> ^w) =
    //    sequenceA (Seq.replicate (max 0 count) fa)


// Functor

    [<CompiledName("Map")>]
    let inline map (f: ^a -> ^b) (RWS r) : RWS< ^e, ^s, ^w, ^b> =
        RWS (fun e s ->
            let swa = r e s
            { RWSResult.State = swa.State
            ; Log = swa.Log
            ; Value = f swa.Value })


// Bifunctor

    [<CompiledName("Bimap")>]
    let inline bimap (f: ^a -> ^c) (g: ^b -> ^d) (RWS r) : RWS< ^e, ^s, ^c, ^d> =
        RWS (fun e s ->
            let swa = r e s
            { RWSResult.State = swa.State
            ; Log = f swa.Log
            ; Value = g swa.Value })

    [<CompiledName("MapFst")>]
    let inline mapFst (f: ^a -> ^c) (RWS r) : RWS< ^e, ^s, ^c, ^b> =
        RWS (fun e s ->
            let swa = r e s
            { RWSResult.State = swa.State
            ; Log = f swa.Log
            ; Value = swa.Value })

    [<CompiledName("MapSnd")>]
    let inline mapSnd (g: ^b -> ^c) (RWS r) : RWS< ^e, ^s, ^a, ^c> =
        RWS (fun e s ->
            let swa = r e s
            { RWSResult.State = swa.State
            ; Log = swa.Log
            ; Value = g swa.Value })


// Semigroup

    let inline append (RWS ra) (RWS rb) : RWS< ^e, ^s, ^w, ^a> =
        RWS (fun e s ->
             let swa = ra e s
             let swb = rb e swa.State
             { swb with RWSResult.Log = append' swa.Log swb.Log
                     ; Value = append' swa.Value swb.Value })


// Traversable

    [<CompiledName("Sequence")>]
    let inline sequence (source: #seq<RWS< ^e, ^s, ^w, ^a>>) : RWS< ^e, ^s, ^w, ^a seq> =
        let rr e s =
            let mutable s = s
            let mutable w = empty' ()
            let app w' = w <- append' w w'
            let ra = ResizeArray<_>()
            for (RWS r) in source do
                let rws = r e s
                s <- rws.State
                app rws.Log
                ra.Add(rws.Value)
            { RWSResult.State = s
            ; Log = w
            ; Value = System.Linq.Enumerable.AsEnumerable(ra) }
        RWS rr

    [<CompiledName("Traverse")>]
    let inline traverse f (source: #seq< ^a>) : RWS< ^e, ^s, ^w, ^b seq> =
        let rr e s =
            let mutable s = s
            let mutable w = empty' ()
            let app w' = w <- append' w w'
            let ra = ResizeArray<_>()
            for x in source do
                let rws = runRWS e s (f x)
                s <- rws.State
                app rws.Log
                ra.Add(rws.Value)
            { RWSResult.State = s
            ; Log = w
            ; Value = System.Linq.Enumerable.AsEnumerable(ra) }
        RWS rr