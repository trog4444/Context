namespace PTR.Context.Type


[<Struct; NoComparison; NoEquality>]
type Cont<'R, 'T> = Cont of ((^T -> ^R) -> ^R)


module Cont =

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
    let inline shift f : Cont< ^r, ^a> = Cont (fun k -> match f k with Cont c -> c id)

    [<CompiledName("Reset")>]
    let reset (Cont (c: ('r -> ^r) -> ^r)) : Cont<'r0, ^r> = Cont (fun k -> k (c id))

    [<CompiledName("CallCC")>]
    let inline callCC (f: (^a -> Cont< ^r, ^``_``>) -> Cont< ^r, ^a>) =
        Cont (fun k -> match f (fun x -> Cont (fun _ -> k x)) with Cont c -> c k)

    [<CompiledName("GetCC")>]
    let inline getCC x0 : Cont< ^r, struct (^a * (^a -> Cont< ^r, ^``_``>))> =
        callCC (fun c -> let rec f x = c (struct (x, f)) in Cont (fun k -> k (x0, f)))

    [<CompiledName("TryCC")>]
    let inline tryCC fOk fErr (input: ^a) : Cont< ^r, ^b> =
        callCC (fun ok -> callCC (fun err -> try ok (fOk input) with e -> err (fErr input e)))

    [<CompiledName("CacheCont")>]
    let cacheCont (Cont (c: ('a -> 'r) -> ^r)) : Cont< ^r, ^a> when 'a : equality =
        let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        Cont (fun k -> c (fun a ->
            match d.TryGetValue(a) with
            | true, r  -> r
            | false, _ -> let r = k a in d.[a] <- r ; r))


    module Compose =

        module Monad =

            [<CompiledName("Wrap")>]
            let inline wrap x : Cont< ^r, ^a> = Cont (fun k -> k x)

            [<CompiledName("Bind")>]
            let inline bind (k: ^a -> Cont< ^r, ^b>) (Cont c) =
                Cont (fun k' -> c (fun a -> match k a with Cont c' -> c' k'))

            [<CompiledName("Flatten")>]
            let inline flatten (Cont cc) : Cont< ^r, ^a> = Cont (fun k -> cc (fun (Cont c) -> c k))


            [<Sealed>]
            type ContBuilder () =
                member inline s.Bind(m, k) = bind k m
                member inline s.Return (x: ^a) : Cont< ^r, ^a> = wrap x
                member inline s.ReturnFrom m : Cont< ^r, ^a> = m
                member inline s.Zero () = s.Return ()
 
                member inline s.TryWith (body, handler) : Cont< ^r, ^a> =
                    try s.ReturnFrom(body ()) with e -> handler e
                member inline s.TryFinally (body, finalizer) : Cont< ^r, ^a> =
                    try s.ReturnFrom(body ()) finally finalizer ()
 
                member inline s.Using(disp: ^d when ^d :> System.IDisposable, body) : Cont< ^r, ^a> =
                    s.TryFinally((fun () -> body disp), disp.Dispose)
 
                member inline s.While(guard, body) =
                    let rec loop = function
                    | false -> s.Zero ()
                    | true  -> s.Bind(body (), fun () -> loop (guard ()))
                    loop (guard ())
 
                member inline s.For(seq: ^a seq, body) =
                    s.Using(seq.GetEnumerator(),
                            fun enum -> s.While(enum.MoveNext,
                                                fun () -> body enum.Current))


            [<CompiledName("RecM")>]
            let inline recM f x : Cont< ^r, ^a> =
                let rec go m = bind (f wrapgo) m
                and wrapgo x = go (wrap x)
                go (f wrap x)        

            [<CompiledName("FoldrM")>]
            let inline foldrM (f: ^a -> ^s -> Cont< ^r, ^s>) (s0: ^s) (source: ^a seq) : Cont< ^r, ^s> =
                let inline g k x s = bind k (f x s)
                match source with
                | :? array< ^a> as s -> Array.fold g wrap s s0
                | :? list<  ^a> as s -> List.fold  g wrap s s0
                | _ -> Seq.fold g wrap source s0

            [<CompiledName("FoldlM")>]
            let inline foldlM (f: ^s -> ^a -> Cont< ^r, ^s>) (s0: ^s) (source: ^a seq) : Cont< ^r, ^s> =
                let inline g x k s = bind k (f s x)
                match source with
                | :? array< ^a> as s -> Array.foldBack g s wrap s0
                | :? list<  ^a> as s -> List.foldBack  g s wrap s0
                | _ -> Seq.foldBack g source wrap s0

  
        module Applicative =

            [<CompiledName("Wrap")>]
            let inline wrap (x: ^a) : Cont< ^r, ^a> = Cont (fun k -> k x)

            [<CompiledName("Ap")>]
            let inline ap (Cont fv) (Cont ff) : Cont< ^r, ^b> =
                Cont (fun k -> ff (fun f -> fv (fun (a: ^a) -> k (f a))))

            [<CompiledName("Map2")>]
            let inline map2 (f: ^a -> ^b -> ^c) (Cont ca) (Cont cb) : Cont< ^r, ^c> =
                Cont (fun k -> ca (fun a -> cb (fun b -> k (f a b))))

            [<CompiledName("Map3")>]
            let inline map3 (f: ^a -> ^b -> ^c -> ^d) (Cont ca) (Cont cb) (Cont cc) : Cont< ^r, ^d> =
                Cont (fun k -> ca (fun a -> cb (fun b -> cc (fun c -> k (f a b c)))))

            [<CompiledName("AndThen")>]
            let inline andThen (Cont cb) (Cont ca) : Cont< ^r, ^b> =
                Cont (fun k -> ca (fun (_: ^a) -> cb k))

            [<CompiledName("When")>]
            let inline when_ condition f : Cont< ^r, unit> =
                if condition then f () else wrap ()

            [<CompiledName("FilterA")>]
            let inline filterA (p: ^a -> Cont< ^r, bool>) (source: ^a seq) : Cont< ^r, ^a list> =
                let cons x b xs = if b then x::xs else xs
                let inline f x xs = map2 (cons x) (p x) xs
                let xs = wrap []
                match source with
                | :? array< ^a> as s -> Array.foldBack f s xs
                | :? list<  ^a> as s -> List.foldBack  f s xs
                | _ -> Seq.foldBack f source xs

            [<CompiledName("SequenceA")>]
            let inline sequenceA (source: Cont< ^r, ^a> seq) : Cont< ^r, ^a list> =
                let cons x xs = x::xs
                let inline g x xs = map2 cons x xs
                let xs = wrap []
                match source with
                | :? array<Cont< ^r, ^a>> as s -> Array.foldBack g s xs
                | :? list< Cont< ^r, ^a>> as s -> List.foldBack  g s xs
                | _ -> Seq.foldBack g source xs

            [<CompiledName("ForA")>]
            let inline forA f (source: ^a seq) : Cont< ^r, ^b list> =
                let cons x xs = x::xs
                let inline g x xs = map2 cons (f x) xs
                let xs = wrap []
                match source with
                | :? array< ^a> as s -> Array.foldBack g s xs
                | :? list<  ^a> as s -> List.foldBack  g s xs
                | _ -> Seq.foldBack g source xs

            [<CompiledName("ZipWithA")>]
            let inline zipWithA f (source1: ^a seq) (source2: ^b seq) : Cont< ^r, ^c list> =
                sequenceA (Seq.map2 f source1 source2)

            [<CompiledName("ReplicateA")>]
            let inline replicateA count (Cont c) : Cont< ^r, ^a seq> =
                Cont (fun k -> c (fun x -> k (Seq.replicate (max 0 count) x)))


        module Functor =

            [<CompiledName("Map")>]
            let inline map (f: ^a -> ^b) (Cont c) : Cont< ^r, ^b> =
                Cont (fun k -> c (fun x -> k (f x)))

            [<CompiledName("Replace")>]
            let replace (b: 'b) (Cont (c: ('a -> 'r) -> ^r)) : Cont< ^r, ^b> =
                Cont (fun k -> c (fun _ -> k b))

            [<CompiledName("Tee")>]
            let inline tee (f: ^a -> ^b) (g: ^a -> ^b -> unit) (Cont c) : Cont< ^r, ^b> =
                Cont (fun k -> c (fun a -> let b = f a in g a b; k b))


        module Comonad =

            [<CompiledName("Extract")>]
            let extract (Cont (c: ('a -> ^a) -> ^a)) : ^a = c id

            [<CompiledName("Extend")>]
            let inline extend j (w: Cont< ^r, ^a>) : Cont< ^r, ^b> =
                Cont (fun k -> k (j w))

            [<CompiledName("Duplicate")>]
            let duplicate (w: Cont<'r, 'a>) : Cont< ^r, Cont< ^r, ^a>> =
                Cont (fun k -> k w)

            [<CompiledName("RecW")>]
            let inline recW f w : ^r =
                let rec go w = f go w in go (extend (f extract) w)


        module Semigroup =

            [<CompiledName("SAppend")>]
            let inline sappend (Cont ca) (Cont cb) : Cont< ^r, ^a> =
                Cont (fun k ->
                    ca (fun a ->
                        cb (fun b -> k (^a: (static member Append: ^a -> ^a -> ^a) (a, b)))))


    let cont = Compose.Monad.ContBuilder ()


    module Testing =
    
        printfn "starting..."

        let t = System.Diagnostics.Stopwatch() 

        let fib_cps n =
            let rec fib' n k =
                match n with
                | 0UL | 1UL -> k 1UL
                | _ -> fib' (n - 1UL) (fun x -> fib' (n - 2UL) (fun y -> k (x + y)))
            fib' n id

        let fib_cps_cache n =
            let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
            let rec fib' n k =
                match n with
                | 0UL | 1UL -> k 1UL
                | _ -> fib' (n - 1UL) (fun x -> fib' (n - 2UL) (fun y -> let xy = x + y
                                                                         match d.TryGetValue(xy) with
                                                                         | true, r -> r
                                                                         | false, _ -> let r = k (x + y) in d.[xy] <- r
                                                                                       r))
            fib' n id

        open Compose
        open Monad
    
        let fib_cont_cache n =
            let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
            let rec go n =
                match n with
                | 0UL | 1UL -> wrap 1UL
                | _ ->
                    let n_1 = n - 1UL
                    let n_2 = n - 2UL
                    let ca =
                        match d.TryGetValue(n_1) with
                        | true, r -> r
                        | false, _ -> let r = go n_1 in d.[n_1] <- r ; r
                    let cb =
                        match d.TryGetValue(n_2) with
                        | true, r -> r
                        | false, _ -> let r = go n_2 in d.[n_2] <- r ; r
                    Applicative.map2 (+) ca cb
            evalCont (go n)

        //let fib_scan (n: uint64) =
        //    //Seq.last <| Seq.take (int n) (seq { yield 0UL
        //    //                                    yield! Seq.unfold (fun struct (a, b) -> Some <| (b, struct (b, a + b))) (0UL, 1UL) })
        //    let mutable a = 0UL
        //    let mutable b = 1UL
        //    let rec go () =            
        //        seq { yield a
        //              yield b
        //              let a' = a
        //              a <- b
        //              b <- a' + b 
        //              yield! go () }
        //    go () |> Seq.take (int n) |> Seq.last


        let fib_cache n =
            let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
            let rec fib' = function
            | 0UL | 1UL -> 1UL
            | n ->
                let n_1 = n - 1UL
                let n_2 = n - 2UL
                let a =
                    match d.TryGetValue(n_1) with
                    | true, r -> r
                    | false, _ -> let r = fib' n_1 in d.[n_1] <- r ; r
                let b =
                    match d.TryGetValue(n_2) with
                    | true, r -> r
                    | false, _ -> let r = fib' n_2 in d.[n_2] <- r ; r
                a + b
            let x = fib' n
            d.Clear()
            x

        let rec fib n =
            match n with
            | 0UL | 1UL -> 1UL
            | x   -> fib (x - 1UL) + fib (x - 2UL)
    
        let cull () =
            for _ in 1..2 do
                let mutable xs = [|1..1_000_000|] in xs <- null
                System.Threading.Thread.Sleep 250
            System.Threading.Thread.Sleep 250
            System.GC.Collect()
            System.GC.GetTotalMemory true |> ignore

        async {
            cull ()

            let fibNum = 5UL in printfn "fib# = %i" fibNum
            do printfn "fib_cps: %i" <| fib_cps fibNum
            do printfn "fib_cps_cache: %i" <| fib_cps_cache fibNum
            do printfn "fib_cont_cache: %i" <| fib_cont_cache fibNum
            do printfn "fib_cache: %i" <| fib_cache fibNum
            //do printfn "fib_scan: %i" <| fib_scan fibNum
            do printfn "fib: %i" <| fib fibNum


            let fibns = 40UL in printfn "fib# = %i" fibns
        
            //cull ()
            //t.Start()
        
            //printfn "s-fib_cps: %i" <| fib_cps fibns
            //printfn "s-t: %i" <| t.ElapsedMilliseconds
            //printfn "s-m: %i" <| System.GC.GetTotalMemory false
            //t.Reset()
        
            //cull ()
            //t.Restart()
        
            //printfn "s-fib_cps_cache: %i" <| fib_cps_cache fibns
            //printfn "s-t: %i" <| t.ElapsedMilliseconds
            //printfn "s-m: %i" <| System.GC.GetTotalMemory false
            //t.Reset()
        
            cull ()
            t.Restart()

            printfn "s-fib_cont_cache: %i" <| fib_cont_cache fibns
            printfn "s-t: %i" <| t.ElapsedMilliseconds
            printfn "s-m: %i" <| System.GC.GetTotalMemory false
            t.Reset()
        
            cull ()
            t.Restart()
        
            printfn "s-fib_cache: %i" <| fib_cache fibns
            printfn "s-t: %i" <| t.ElapsedMilliseconds
            printfn "s-m: %i" <| System.GC.GetTotalMemory false
            t.Reset()
        
            //cull ()
            //t.Restart()
        
            //printfn "s-fib_scan: %i" <| fib_scan fibns
            //printfn "s-t: %i" <| t.ElapsedMilliseconds
            //printfn "s-m: %i" <| System.GC.GetTotalMemory false
            //t.Reset()
        
            cull ()
            t.Restart()

            printfn "s-fib: %i" <| fib fibns
            printfn "s-t: %i" <| t.ElapsedMilliseconds
            printfn "s-m: %i" <| System.GC.GetTotalMemory false
            t.Reset()

            let fibn = 100UL in printfn "fib# = %i" fibn

            //cull ()
            //t.Start()

            //printfn "fib_cps: %i" <| fib_cps fibn
            //printfn "t: %i" <| t.ElapsedMilliseconds
            //printfn "m: %i" <| System.GC.GetTotalMemory false
            //t.Reset()

            //cull ()
            //t.Restart()

            //printfn "fib_cps_cache: %i" <| fib_cps_cache fibns
            //printfn "t: %i" <| t.ElapsedMilliseconds
            //printfn "m: %i" <| System.GC.GetTotalMemory false
            //t.Reset()
        
            cull ()
            t.Restart()

            printfn "fib_cont_cache: %i" <| fib_cont_cache fibn
            printfn "t: %i" <| t.ElapsedMilliseconds
            printfn "m: %i" <| System.GC.GetTotalMemory false
            t.Reset()

            cull ()
            t.Restart()

            printfn "fib_cache: %i" <| fib_cache fibn
            printfn "t: %i" <| t.ElapsedMilliseconds
            printfn "m: %i" <| System.GC.GetTotalMemory false
            t.Reset()

            //cull ()
            //t.Restart()

            //printfn "fib_scan: %i" <| fib_scan fibn
            //printfn "t: %i" <| t.ElapsedMilliseconds
            //printfn "m: %i" <| System.GC.GetTotalMemory false
            //t.Reset()

            cull ()
            t.Restart()

            printfn "fib: %i" <| fib fibn
            printfn "t: %i" <| t.ElapsedMilliseconds
            printfn "m: %i" <| System.GC.GetTotalMemory false
            t.Reset()

            printfn "...done"
        } |> Async.Start


//open Cont
//open Compose
 
//// @ Operators @
//type Cont<'R, 'T> with

//// @ Primitive @

//    /// The result of running a CPS computation with a given final continuation.
//    static member inline ( >- ) (m, k) = runCont m k
//    /// The result of running a CPS computation with a given final continuation.
//    static member inline ( -< ) (k, m) = runCont m k

//// @ Monad @

//    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
//    static member inline ( >>= ) (m, k) = Monad.bind k m
//    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
//    static member inline ( =<< ) (k, m) = Monad.bind k m

//// @ Applicative @

//    /// Sequential application on effects.
//    static member inline ( <*> ) (ff, fx) = Applicative.ap fx ff
//    /// Sequential application on effects.
//    static member inline ( <**> ) (fx, ff) = Applicative.ap fx ff

//    /// Sequentially compose two effects, discarding any value produced by the first.
//    static member inline ( *> ) (fa, fb) = Applicative.andThen fb fa
//    /// Sequentially compose two effects, discarding any value produced by the first.
//    static member inline ( <* ) (fb, fa) = Applicative.andThen fb fa

//// @ Functor @

//    /// Lift a function onto effects.
//    static member inline ( |%> ) (fa, f) = Functor.map f fa
//    /// Lift a function onto effects.
//    static member inline ( <%| ) (f, fa) = Functor.map f fa

//    /// Replace all locations in the input with the same value.
//    static member inline ( %> ) (fa, b) = Functor.replace b fa
//    /// Replace all locations in the input with the same value.
//    static member inline ( <% ) (b, fa) = Functor.replace b fa

//// @ Comonad @

//    /// Sequentially compose two co-effects.
//    static member inline ( =>> ) (w, j) = Comonad.extend j w
//    /// Sequentially compose two co-effects.
//    static member inline ( <<= ) (j, w) = Comonad.extend j w

//// @ Semigroup @

//    /// An associative composition operation.
//    static member inline Append (e1, e2) = Semigroup.sappend e1 e2