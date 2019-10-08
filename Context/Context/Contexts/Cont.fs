namespace PTR.Context.Type.Cont


[<Struct; NoComparison; NoEquality>]
type Cont<'R, 'T> = Cont of (('T -> 'R) -> 'R) with

    member inline s.Invoke(cont: System.Func< ^T, ^R>) : ^R =
        let (Cont c) = s in c cont.Invoke
            

    static member inline Unit x : Cont< ^r, ^a> = Cont (fun k -> k x)

    member inline s.Select(f: System.Func< ^T, ^U>) : Cont< ^R, ^U> =
        let (Cont c) = s in Cont (fun k -> c (fun a -> k (f.Invoke(a))))
    member inline s.Select2(second: Cont< ^R, ^U>, f: System.Func< ^T, ^U, ^V>) : Cont< ^R, ^V> =
        let (Cont c) = s in
        Cont (fun k -> c (fun a -> second.Invoke(fun b -> k (f.Invoke(a, b)))))
    member inline s.SelectMany(f: System.Func< ^T, Cont< ^R, ^U>>) : Cont< ^R, ^U> =
        let (Cont c) = s
        Cont (fun k -> c (fun a -> f.Invoke(a).Invoke(fun b -> k b)))
    member inline s.SelectMany(f: System.Func< ^T, Cont< ^R, ^U>>, g: System.Func< ^T, ^U, ^V>) : Cont< ^R, ^V> =
        let (Cont c) = s
        Cont (fun k -> c (fun a -> f.Invoke(a).Invoke(fun b -> k (g.Invoke(a, b)))))    

    member inline s.Join(t: Cont< ^R, ^U>, kt: System.Func< ^T, ^K>, ku: System.Func< ^U, ^K>, rs: System.Func< ^T, ^U, ^V>) : Cont< ^R, ^V> =
        s.Select2(t, rs)

    member inline s.ContinueWith(f: System.Func<Cont< ^R, ^T>, ^U>) : Cont< ^R, ^U> =
        let s' = s
        Cont (fun k -> k (f.Invoke(s')))

    static member inline Append ((Cont c1), Cont c2) : Cont< ^r, ^a> =
        Cont (fun k -> c1 (fun a1 -> c2 (fun a2 -> k (^a : (static member Append: ^a -> ^a -> ^a) (a1, a2)))))


module Cont =

// Primitives

    [<CompiledName("Make")>]
    let inline make (f: System.Func<System.Func< ^T, ^R>, ^R>) : Cont< ^R, ^T> =
        Cont (fun k -> f.Invoke(System.Func<_,_>k))

    [<CompiledName("RunCont")>]
    let inline runCont (k: ^a -> ^r) (Cont (c: (^a -> ^r) -> ^r)) : ^r = c k

    [<CompiledName("EvalCont")>]
    let evalCont (Cont (c: ('r -> ^r) -> ^r)) : ^r = c id

    [<CompiledName("MapCont")>]
    let inline mapCont f (Cont c) : Cont< ^r, ^a> = Cont (fun k -> f (c k))

    [<CompiledName("WithCont")>]
    let inline withCont (f: (^b -> ^r) -> ^a -> ^r) (Cont c) : Cont< ^r, ^b> = Cont (fun k -> c (f k))

    [<CompiledName("Exit")>]
    let exit (x: 'r) : Cont< ^r, '``_``> = Cont (fun _ -> x)
 
    [<CompiledName("Shift")>]
    let inline shift f : Cont< ^r, ^a> = Cont (fun k -> let (Cont c) = f k in c id)

    [<CompiledName("Reset")>]
    let inline reset (Cont c) : Cont< ^r0, ^r> = Cont (fun k -> k (c id))

    [<CompiledName("CallCC")>]
    let inline callCC (f: (^a -> Cont< ^r, ^``_``>) -> Cont< ^r, ^a>) =
        Cont (fun k -> let (Cont c) = f (fun x -> Cont (fun _ -> k x)) in c k)

    [<CompiledName("GetCC")>]
    let inline getCC x0 : Cont< ^r, struct (^a * (^a -> Cont< ^r, ^``_``>))> =
        callCC (fun c -> let rec f x = c (struct (x, f)) in Cont (fun k -> k (x0, f)))

    [<CompiledName("TryCC")>]
    let inline tryCC fOk fErr (input: ^a) : Cont< ^r, ^b> =
        callCC (fun ok -> callCC (fun err -> try ok (fOk input) with e -> err (fErr input e)))

    [<CompiledName("CacheCont")>]
    let inline cacheCont (Cont c) : Cont< ^r, ^a> when ^a : equality =
        let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        let memo k =
            c (fun a ->
                match d.TryGetValue(a) with
                | true, r  -> r
                | false, _ -> let r = k a in d.[a] <- r ; r)
        Cont memo


// Monad

    let inline unit x : Cont< ^r, ^a> = Cont (fun k -> k x)

    [<CompiledName("Bind")>]
    let inline bind (f: ^a -> Cont< ^r, ^b>) (Cont c) =
        Cont (fun k -> c (fun a -> runCont k (f a)))

    [<CompiledName("Flatten")>]
    let inline flatten (Cont cc) : Cont< ^r, ^a> =
        Cont (fun k -> cc (fun (Cont c) -> c k))

    [<CompiledName("RecM")>]
    let inline recM f (x: ^a) : Cont< ^r, ^b> =
        let rec go m = bind j m
        and k a = go (unit a)
        and j a = f k a
        j x

    [<CompiledName("RecM1")>]
    let inline recM1 f (x: ^a) : Cont< ^r, ^b> =
        let rec go m = bind k m
        and k a = f go a
        k x

    //[<CompiledName("FoldrM")>]
    //let inline foldrM (f: ^a -> ^s -> Cont< ^r, ^s>) (s0: ^s) (source: ^a seq) : Cont< ^r, ^s> =
    //    let g k x s = bind k (f x s)
    //    match source with
    //    | :? array< ^a> as s -> Array.fold g unit s s0
    //    | :? list<  ^a> as s -> List.fold  g unit s s0
    //    | _ -> Seq.fold g unit source s0
    //
    //[<CompiledName("FoldlM")>]
    //let inline foldlM (f: ^s -> ^a -> Cont< ^r, ^s>) (s0: ^s) (source: ^a seq) : Cont< ^r, ^s> =
    //    let g x k s = bind k (f s x)
    //    match source with
    //    | :? array< ^a> as s -> Array.foldBack g s unit s0
    //    | :? list<  ^a> as s -> List.foldBack  g s unit s0
    //    | _ -> Seq.foldBack g source unit s0


    module Workflow =

        type ContBuilder () =
            member inline _.Return(x: ^a) : Cont< ^r, ^a> = unit x
            member inline _.ReturnFrom (m: Cont< ^r, ^a>) : Cont< ^r, ^a> = m
            member inline _.Bind (m: Cont< ^r, ^a>, f: ^a -> Cont< ^r, ^b>) = bind f m
            
            member inline _.Zero() = unit ()

            member inline _.Using (disp: ^d, body: ^d -> Cont< ^r, ^a>) : Cont< ^r, ^a> when ^d :> System.IDisposable =
                using disp body

            member inline _.TryWith (body: Cont< ^r, ^a>, handler: exn -> Cont< ^r, ^a>) : Cont< ^r, ^a> =
                try body with e -> handler e
            member inline _.TryFinally (body: Cont< ^r, ^a>, finalizer: unit -> unit) : Cont< ^r, ^a> =
                try body finally finalizer ()

            member inline _.While(guard, body) : Cont< ^r, unit> =                    
                let rec go = function
                | false -> unit ()
                | true  -> bind k (body ())
                and k () = go (guard ()) in k ()

            member inline _.For(seq: #seq< ^a>, body) : Cont< ^r, unit> =
                use e = seq.GetEnumerator()
                let rec go = function
                | false -> unit ()
                | true  -> b e.Current
                and b x = bind k (body x)
                and k () = go (e.MoveNext()) in k ()


    let cont = Workflow.ContBuilder ()

  
// Applicative
          
    [<CompiledName("Ap")>]
    let inline ap (Cont fv) (Cont ff) : Cont< ^r, ^b> =
        Cont (fun k -> ff (fun f -> fv (fun (a: ^a) -> k (f a))))

    [<CompiledName("Map2")>]
    let inline map2 (f: ^a -> ^b -> ^c) (Cont ca) (Cont cb) : Cont< ^r, ^c> =
        Cont (fun k -> ca (fun a -> cb (fun b -> k (f a b))))

    //[<CompiledName("Map3")>]
    //let inline map3 (f: ^a -> ^b -> ^c -> ^d) (Cont ca) (Cont cb) (Cont cc) : Cont< ^r, ^d> =
    //    Cont (fun k -> ca (fun a -> cb (fun b -> cc (fun c -> k (f a b c)))))

    [<CompiledName("AndThen")>]
    let inline andThen (Cont cb) (Cont ca) : Cont< ^r, ^b> =
        Cont (fun k -> ca (fun (_: ^a) -> cb k))

    [<CompiledName("When")>]
    let inline when_ condition f : Cont< ^r, unit> =
        if condition then f () else unit ()

    //[<CompiledName("FilterA")>]
    //let inline filterA (p: ^a -> Cont< ^r, bool>) (source: ^a seq) : Cont< ^r, ^a list> =
    //    let cons x b xs = if b then x::xs else xs
    //    let inline f x xs = map2 (cons x) (p x) xs
    //    let z = wrap []
    //    match source with
    //    | :? array< ^a> as s -> Array.foldBack f s z
    //    | :? list<  ^a> as s -> List.foldBack  f s z
    //    | _ -> Seq.foldBack f source z

    //[<CompiledName("ZipWithA")>]
    //let inline zipWithA f (source1: #seq< ^a>) (source2: #seq< ^b>) : Cont< ^r, ^c list> =
    //    sequenceA (System.Linq.Enumerable.Zip(source1, source2, System.Func<_,_,_>f))

    //[<CompiledName("ReplicateA")>]
    //let inline replicateA count (Cont c) : Cont< ^r, ^a seq> =
    //    Cont (fun k -> c (fun x -> k (Seq.replicate (max 0 count) x)))


// Functor

    [<CompiledName("Map")>]
    let inline map (f: ^a -> ^b) (Cont c) : Cont< ^r, ^b> =
        Cont (fun k -> c (fun x -> k (f x)))


// Comonad

    [<CompiledName("Extract")>]
    let extract (Cont (c: ('a -> ^a) -> ^a)) : ^a = c id

    [<CompiledName("Extend")>]
    let inline extend j (w: Cont< ^r, ^a>) : Cont< ^r, ^b> =
        Cont (fun k -> k (j w))

    [<CompiledName("Duplicate")>]
    let duplicate (w: Cont<'r, 'a>) : Cont< ^r, Cont< ^r, ^a>> =
        Cont (fun k -> k w)

    [<CompiledName("RecW")>]
    let inline recW f w : ^a =
        let rec go w = f j w
        and k w = f extract w
        and j w = go (extend k w)
        j w


// Semigroup

    let inline append (Cont ca) (Cont cb) : Cont< ^r, ^a> =
        Cont (fun k ->
            ca (fun a ->
                cb (fun b -> k (^a: (static member Append: ^a -> ^a -> ^a) (a, b)))))


// Foldable

    [<CompiledName("Fold")>]
    let inline fold (folder: ^s -> ^a -> ^s) (seed: ^s) (Cont cc) : ^s =
        let mutable s = seed in cc (fun a -> s <- folder s a)
        s

    [<CompiledName("FoldBack")>]
    let inline foldBack (folder: ^a -> ^s -> ^s) (seed: ^s) (Cont cc) : ^s =        
        let ra = ResizeArray<_>() in cc ra.Add
        let mutable s = seed in for i = ra.Count - 1 downto 0 do s <- folder ra.[i] s
        s

    [<CompiledName("Foldr")>]
    let inline foldr (folder: ^a -> (unit -> ^s) -> ^s) seed (Cont cc) =
        let ra = ResizeArray<_>() in cc ra.Add
        let rec go n = if n < ra.Count then folder ra.[n] (fun () -> go (n + 1)) else seed ()
        go 0

    [<CompiledName("Foldl")>]
    let inline foldl (folder: (unit -> ^s) -> ^a -> ^s) seed (Cont cc) =
        let ra = ResizeArray<_>() in cc ra.Add
        let mutable ``cont?`` = true
        let f' z i =
            let z = ``cont?`` <- false
                    folder (fun () -> ``cont?`` <- true; z ()) ra.[i]
            in fun () -> z
        let rec go z i =
            if ``cont?`` && i < ra.Count
            then go (f' z i) (i + 1)
            else z ()
        if ra.Count = 0 then seed () else go seed 0

    [<CompiledName("Foldm")>]
    let inline foldm f source : ^m when ^m : (static member Append: ^m -> ^m -> ^m) =
        let mappend a b = (^m : (static member Append: ^m -> ^m -> ^m) (a, b))
        let z = (^m : (static member Empty: unit -> ^m) ())
        let folder a s = mappend (f a) s
        foldBack folder z source

    [<CompiledName("MapFold")>]
    let inline mapFold (mapping: ^s -> ^a -> ^b * ^s) seed (Cont cc) : Cont<unit, ^b> * ^s =
        let bs = ResizeArray<_>()
        let mutable s = seed
        cc (fun a -> let b, z = mapping s a
                     bs.Add(b)
                     s <- z)
        Cont (fun k -> for x in bs do k x), s

    [<CompiledName("MapFoldBack")>]
    let inline mapFoldBack (mapping: ^a -> ^s -> ^b * ^s) seed (Cont cc) : Cont<unit, ^b> * ^s =
        let az = ResizeArray<_>() in cc az.Add
        let bz = ResizeArray<_>(az.Count)
        let mutable s = seed
        for i = az.Count - 1 downto 0 do
            let b, z = mapping az.[i] s
            bz.Add(b)
            s <- z
        Cont (fun k -> for x in bz do k x), s


// Traversable

    [<CompiledName("Sequence")>]
    let inline sequence (source: seq<Cont< ^r, ^a>>) : Cont< ^r, seq< ^a>> =
        let cons x xs = x::xs
        let f x xs = map2 cons x xs
        let z = unit []
        match source with
        | :? array<Cont< ^r, ^a>> as s -> Array.foldBack f s z
        | :? list< Cont< ^r, ^a>> as s -> List.foldBack f s z
        | _ -> Seq.foldBack f source z
        |> map System.Linq.Enumerable.AsEnumerable

    [<CompiledName("Traverse")>]
    let inline traverse (f: ^a -> Cont< ^r, ^b>) (source: #seq< ^a>) : Cont< ^r, seq< ^b>> =
        sequence (System.Linq.Enumerable.Select(source, System.Func<_,_>f))