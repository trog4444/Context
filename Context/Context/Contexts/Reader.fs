namespace PTR.Context.Type.Reader


[<Struct; NoComparison; NoEquality>]
type Reader<'Env, 'Result> = Reader of ('Env -> 'Result) with

    member inline s.Invoke(env: ^Env) = let (Reader r) = s in r env

    static member inline Unit(x: ^a) : Reader< ^e, ^a> = Reader (fun _ -> x)

    member inline s.Select(f: System.Func< ^Result, ^NextResult>) : Reader< ^Env, ^NextResult> =
        let (Reader r) = s in Reader (fun e -> f.Invoke(r e))

    member inline s.Select2((Reader r2), f: System.Func< ^Result, ^NextResult, ^FinalResult>) : Reader< ^Env, ^FinalResult> =
        let (Reader r) = s in Reader (fun e -> f.Invoke(r e, r2 e))

    member inline s.SelectMany(f: System.Func< ^Result, Reader< ^Env, ^NextResult>>) : Reader< ^Env, ^NextResult> =
        let (Reader r) = s in Reader (fun e -> f.Invoke(r e).Invoke(e))

    member inline s.SelectMany(f: System.Func< ^Result, Reader< ^Env, ^NextResult>>, g: System.Func< ^Result, ^NextResult, ^FinalResult>) : Reader< ^Env, ^FinalResult> =
        let (Reader r) = s in Reader (fun e -> let a = r e in g.Invoke(a, f.Invoke(r e).Invoke(e)))

    member inline s.Join(t: Reader< ^Env, ^NextResult>, kt: System.Func< ^Result, ^K>, ku: System.Func< ^NextResult, ^K>, rs: System.Func< ^Result, ^NextResult, ^FinalResult>) : Reader< ^Env, ^FinalResult> =
        s.Select2(t, rs)

    static member inline Append ((Reader r1), Reader r2) : Reader< ^e, ^a> =
        Reader (fun e -> (^a : (static member Append: ^a -> ^a -> ^a) (r1 e, r2 e)))


module Reader =

// Primitives

    [<CompiledName("Make")>]
    let inline make (reader: System.Func< ^e, ^a>) : Reader< ^e, ^a> = Reader reader.Invoke

    [<CompiledName("RunReader")>]
    let inline runReader (env: ^e) (Reader (r: ^e -> ^r)) : ^r = r env
    
    [<CompiledName("WithReader")>]
    let inline withReader (f: ^e0 -> ^e) (Reader r) : Reader< ^e0, ^r> = Reader (fun e -> r (f e))

    [<CompiledName("Ask")>]
    let ask<'e> : Reader< ^e, ^e> = Reader id

    [<CompiledName("Local")>]
    let inline local (localize: ^e -> ^e) (Reader r) : Reader< ^e, ^r> =
        Reader (fun e -> r (localize e))    

    [<CompiledName("Flip")>]
    let inline flip (f: ^a -> ^e -> ^r) : Reader< ^e, (^a -> ^r)> =
        Reader (fun e a -> f a e)

    [<CompiledName("Curry")>]
    let inline curry (f: ^a * ^b -> ^c) = Reader (fun a b -> f (a, b))

    [<CompiledName("Curry1")>]
    let inline curry1 (f: struct (^a * ^b) -> ^c) = Reader (fun a b -> f (a, b))

    [<CompiledName("Uncurry")>]
    let inline uncurry (f: ^a -> ^b -> ^c) = Reader (fun (a, b) -> f a b)

    [<CompiledName("Uncurry1")>]
    let inline uncurry1 (f: ^a -> ^b -> ^c) = Reader (fun struct (a, b) -> f a b)

    [<CompiledName("CacheReader")>]
    let inline cacheReader (Reader f) : Reader< ^e, ^r> when ^e : equality =
        let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        Reader (fun e -> match d.TryGetValue(e) with
                         | true, r  -> r
                         | false, _ -> let r = f e in d.[e] <- r ; r)


// Monad

    let unit (x: 'a) : Reader<'e, ^a> = Reader (fun _ -> x)

    [<CompiledName("Bind")>]
    let inline bind f (Reader (r: ^e -> ^a)) : Reader< ^e, ^b> =
        Reader (fun e -> let (Reader rr) = f (r e) in rr e)

    [<CompiledName("Flatten")>]
    let flatten (Reader (rr: 'e -> Reader< ^e, 'a>)) : Reader< ^e, ^a> =
        Reader (fun e -> let (Reader r) = rr e in r e)

    [<CompiledName("RecM")>]
    let inline recM f (x: ^a) : Reader< ^e, ^b> =
        let rec go m = bind j m
        and k a = go (unit a)
        and j a = f k a
        j x

    [<CompiledName("RecM1")>]
    let inline recM1 f (x: ^a) : Reader< ^e, ^b> =
        let rec go m = bind k m
        and k a = f go a
        k x

    //[<CompiledName("FoldrM")>]
    //let inline foldrM f (s0: ^s) (source: ^a seq) : Reader< ^e, ^s> =             
    //    Reader (fun e ->
    //        let mutable s = s0
    //        let act a () = s <- runReader e (f a s)
    //        match source with
    //        | :? array< ^a> as s -> Array.foldBack act s ()
    //        | :? list<  ^a> as s -> List.foldBack  act s ()
    //        | _ -> Seq.foldBack act source ()
    //        s)
    //
    //[<CompiledName("FoldlM")>]
    //let inline foldlM f (s0: ^s) (source: ^a seq) : Reader< ^e, ^s> =
    //    Reader (fun e ->
    //        let mutable z = s0
    //        let inline r x = let (Reader rdr) = f z x in z <- rdr e
    //        match source with
    //        | :? array< ^a> as s -> for i = 0 to s.Length - 1 do r s.[i]
    //        | :? ResizeArray< ^a> as s -> for i = 0 to s.Count - 1 do r s.[i]
    //        | :? list<  ^a> as s -> for x in s do r x
    //        | _ -> for x in source do r x
    //        z)

    
    module Workflow =

        type ReaderBuilder () =
            member inline _.Return(x: ^a) : Reader< ^e, ^a> = unit x
            member inline _.ReturnFrom (m: Reader< ^e, ^a>) : Reader< ^e, ^a> = m
            member inline _.Bind(m: Reader< ^e, ^a>, f) = bind f m
            
            member inline _.Zero() : Reader< ^e, unit> = unit ()

            member inline _.Using(disp: ^d, body: ^d -> Reader< ^e, ^a>) : Reader< ^e, ^a> when ^d :> System.IDisposable =
                using disp body

            member inline _.TryWith(body, handler) : Reader< ^e, ^a> =
                try body with e -> handler e
            member inline _.TryFinally(body, finalizer) : Reader< ^e, ^a> =
                try body finally finalizer ()

            member inline _.While(guard, body) : Reader< ^e, unit> =
                let rec go = function
                | false -> unit ()
                | true  -> bind k (body ())
                and k () = go (guard ()) in k ()

            member inline _.For(seq: #seq< ^a>, body) : Reader< ^e, unit> =
                use e = seq.GetEnumerator()
                let rec go = function
                | false -> unit ()
                | true  -> b e.Current
                and b x = bind k (body x)
                and k () = go (e.MoveNext()) in k ()


    let reader = Workflow.ReaderBuilder()


// Applicative
      
    [<CompiledName("Ap")>]
    let inline ap (Reader (rv: ^e -> ^a)) (Reader rf) : Reader< ^e, ^b> = Reader (fun e -> rf e (rv e))

    [<CompiledName("Map2")>]
    let inline map2 (f: ^a -> ^b -> ^c) (Reader ra) (Reader rb) : Reader< ^e, ^c> =
        Reader (fun e -> f (ra e) (rb e))

    [<CompiledName("AndThen")>]
    let inline andThen (Reader rb) (Reader (ra: ^e -> ^a)) : Reader< ^e, ^b> =
        Reader (fun e -> ignore (ra e); rb e)

    [<CompiledName("When")>]
    let inline when_ condition f : Reader< ^e, unit> =
        if condition then f () else unit ()


// Functor

    [<CompiledName("Map")>]
    let inline map (f: ^a -> ^b) (Reader r) : Reader< ^e, ^b> =
        Reader (fun e -> f (r e))

// Profunctor

    [<CompiledName("Dimap")>]
    let inline dimap (f: ^a0 -> ^a) (g: ^b -> ^c) (Reader r) : Reader< ^a0, ^c> =
        Reader (fun a0 -> g (r (f a0)))

    [<CompiledName("LMap")>]
    let inline lmap (f: ^a0 -> ^a) (Reader r) : Reader< ^a0, ^b> = Reader (fun a0 -> r (f a0))

    [<CompiledName("RMap")>]
    let inline rmap (g: ^b -> ^c) (Reader r) : Reader< ^a, ^c> = Reader (fun a -> g (r a))

        
// Semigroup

    let inline append (Reader (f: (^e -> ^a))) (Reader g) : Reader< ^e, ^a> when ^a: (static member Append: ^a -> ^a -> ^a) =
        Reader (fun e -> (^a: (static member Append: ^a -> ^a -> ^a) (f e, g e)))


// Traversable

    [<CompiledName("Sequence")>]
    let inline sequence (source: #seq<Reader< ^e, ^a>>) : Reader< ^e, ^a seq> =
        Reader (fun e -> System.Linq.Enumerable.Select(source, fun (Reader r) -> r e))

    [<CompiledName("Traverse")>]
    let inline traverse (f: ^a -> Reader< ^e, ^b>) (source: #seq< ^a>) : Reader< ^e, ^b seq> =
        Reader (fun e ->
            let g a = let (Reader r) = f a in r e
            System.Linq.Enumerable.Select(source, g))


// Cat

    [<CompiledName("Identity")>]
    let identity<'a> : Reader< ^a, ^a> = Reader id

    [<CompiledName("Compose")>]
    let inline compose (Reader rb) (Reader (ra: ^a -> ^b)) : Reader< ^a, ^c> =
        Reader (fun a -> rb (ra a))


// Arrow

    [<CompiledName("Arr")>]
    let inline arr f : Reader< ^a, ^b> = Reader f

    [<CompiledName("ArrFst")>]
    let inline arrFst (Reader r) : Reader< ^a * ^c, ^b * ^c> = Reader (fun (a, c) -> r a, c)

    [<CompiledName("ArrSnd")>]
    let inline arrSnd (Reader r) : Reader< ^c * ^a, ^c * ^b> = Reader (fun (c, a) -> c, r a)

    [<CompiledName("Split")>]
    let inline split (Reader rb) (Reader ra) : Reader< ^a * ^c, ^b * ^d> =
        Reader (fun (a, b) -> ra a, rb b)

    [<CompiledName("Fanout")>]
    let inline fanout (Reader rb) (Reader ra) : Reader< ^a, ^b * ^c> =
        Reader (fun e -> ra e, rb e)


// Choice

    [<CompiledName("Feed1")>]
    let inline feed1 (Reader (f: ^a -> ^b)) =
        Reader (function
        | Choice1Of2 a -> Choice1Of2 (f a)
        | Choice2Of2 c -> Choice2Of2 (c: ^c))
      
    [<CompiledName("Feed2")>]
    let inline feed2 (Reader (f: ^a -> ^b)) =
        Reader (function
        | Choice1Of2 c -> Choice1Of2 (c: ^c)
        | Choice2Of2 a -> Choice2Of2 (f a))

    [<CompiledName("Merge")>]
    let inline merge (Reader (g: ^c -> ^d)) (Reader (f: ^a -> ^b)) = 
        Reader (function
        | Choice1Of2 x -> Choice1Of2 (f x)
        | Choice2Of2 y -> Choice2Of2 (g y))

    [<CompiledName("Fanin")>]
    let inline fanin (Reader g) (Reader f) : Reader<Choice< ^a, ^c>, ^b> =
        Reader (function
        | Choice1Of2 x -> f x
        | Choice2Of2 y -> g y)


// Apply

    [<CompiledName("App")>]
    let app<'a, 'b> : Reader<Reader<'a, 'b> * ^a, ^b> =
        Reader (fun ((Reader f), b) -> f b)