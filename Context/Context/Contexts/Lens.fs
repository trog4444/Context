namespace PTR.Context.Type.Lens

// Based on the Haskell lens package by Edward Kmett.
//
// https://www.youtube.com/watch?v=XVmhK8WbRLY&list=PLFDB7DEC7F7F53DFD&index=2
//
// Yep. Like the monad laws, these are expectations you should have about lenses. Lenses that violate them are weird. Here they are
//
//  Get-Put: If you modify something by changing its subpart to exactly what it was before... then nothing happens
//  Put-Get: If you modify something by inserting a particular subpart and then viewing the result... you'll get back exactly that subpart
//  Put-Put: If you modify something by inserting a particular subpart a, and then modify it again inserting a different subpart b... it's exactly as if you only did the second step.


[<Struct; NoComparison; NoEquality>]
type Lens<'P, 'V> = { Get: ('P -> 'V) ; Set: ('P -> 'V -> 'P) } with

    member inline s.Invoke (property: ^P) = s.Set property (s.Get property)

    member inline s.With(get: System.Func< ^P, ^V>) =
        { Lens.Set = s.Set ; Get = get.Invoke }
    member inline s.With(set: System.Func< ^P, ^V, ^P>) =
        { Lens.Get = s.Get ; Set = fun p v -> set.Invoke(p, v) }
    
    static member inline Unit(x: ^a) : Lens< ^p, ^a> =
        { Lens.Get = fun _ -> x
        ; Set = fun y _ -> y }

    member inline s.Select (f: System.Func< ^V, ^W>) : Lens< ^P, ^W> =
        let s' = s
        { Lens.Get = fun p -> f.Invoke(s'.Get p)
        ; Set = fun p _ -> s'.Set p (s'.Get p) }

    member inline s.SelectMany (f: System.Func< ^V, Lens< ^P, ^W>>) : Lens< ^P, ^W> =
        let s' = s
        { Lens.Get = fun a -> (f.Invoke(s'.Get a)).Get a
        ; Set = fun a c -> s'.Set a (s'.Get ((f.Invoke(s'.Get a)).Set a c)) }

    member inline s.Select2 (second: Lens< ^P, ^W>, f: System.Func< ^V, ^W, ^X>) : Lens< ^P, ^X> =
        let s' = s
        s'.SelectMany(fun a -> second.Select(fun b -> f.Invoke(a, b)))

    member inline s.SelectMany (f: System.Func< ^V, Lens< ^P, ^W>>, g: System.Func< ^V, ^W, ^X>) : Lens< ^P, ^X> =
        s.SelectMany(fun a -> f.Invoke(a).Select(fun b -> g.Invoke(a, b)))

    member inline s.Join(t: Lens< ^P, ^W>, _: System.Func< ^V, ^K>, _: System.Func< ^W, ^K>, rs: System.Func< ^V, ^W, ^X>) : Lens< ^P, ^X> =
        s.Select2(t, rs)

    static member inline Append (first: Lens< ^p, ^a>, second: Lens< ^p, ^a>) : Lens< ^p, ^a> =
        let app a b = (^a : (static member Append: ^a -> ^a -> ^a) (a, b))
        first.Select2(second, System.Func<_,_,_>app)


module Lens =

    module Pattern =
  
        let inline ( |Lens| ) (lens: Lens< ^p, ^v>) = Lens lens

        let inline ( |LensGet| ) (lens: Lens< ^p, ^v>) = LensGet lens.Get
  
        let inline ( |LensSet| ) (lens: Lens< ^p, ^v>) = LensSet lens.Set  
  
    
    //[<RequireQualifiedAccess>]
    //module Lenses =
    //
    //[<RequireQualifiedAccess>]
    //module Map =
    //
    //    ////[<CompiledName("MemberL")>]
    //    ////let inline memberL key : Lens<Map< ^k, ^v>, ^v option> =
    //    ////    { Lens.Get = Map.tryFind key
    //    ////    ; Set = fun m v -> match v with
    //    ////                       | None   -> Map.remove key m
    //    ////                       | Some v -> Map.add key v m }
    //
    //[<RequireQualifiedAccess>]
    //module State =
    //
    //    let inline modifyL (lens: Lens< ^p, ^s>) property state = lens.Get (lens.Set property state)
    //
    //    let inline putL (lens: Lens< ^s, ^p>) state = lens.Set state (lens.Get state)
    //
    //    let inline focus f (lens: Lens< ^p, ^s>) property =
    //        match f (lens.Get property) with (s, a) -> (lens.Set property s, a)


    [<CompiledName("Make")>]
    let inline make (getter: System.Func< ^p, ^v>) (setter: System.Func< ^p, ^v, ^p>) : Lens< ^p, ^v> =
        { Lens.Get = getter.Invoke
        ; Set = fun p v -> setter.Invoke(p, v) }

    let inline newLens getter setter : Lens< ^p, ^v> = { Lens.Get = getter; Set = setter }
  
    let inline runLens property (lens: Lens< ^p, ^v>) : ^p = lens.Set property (lens.Get property)

    let inline getL (lens: Lens< ^p, ^v>) = lens.Get

    let inline setL (lens: Lens< ^p, ^v>) = lens.Set

    [<CompiledName("ModL")>]
    let inline modL f property (lens: Lens< ^p, ^v>) = lens.Set property (f (lens.Get property))

    [<CompiledName("FstL")>]
    let fstL<'a, 'b> : Lens< ^a * ^b, ^a> = { Lens.Get = fst; Set = fun (_, b) a -> a, b }

    [<CompiledName("SndL")>]
    let sndL<'a, 'b> : Lens< ^a * ^b, ^b> = { Lens.Get = snd; Set = fun (a, _) b -> a, b }  

    [<CompiledName("CacheLens")>]
    let inline cacheLens (lens: Lens< ^p, ^v> when ^p : equality and ^v : equality) =
        let dget = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        let dset = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        { Lens.Get = fun a ->
            match dget.TryGetValue(a) with
            | true, r  -> r
            | false, _ -> let r = lens.Get a in dget.[a] <- r ; r
        ; Set = fun a b ->
            let p = struct (a, b)
            match dset.TryGetValue(p) with
            | true, r  -> r
            | false, _ -> let r = lens.Set a b in dset.[p] <- r ; r }


// Monad

    let inline unit (x: ^a) : Lens< ^p, ^a> =
        { Lens.Get = fun _ -> x
        ; Set = fun y _ -> y }

    [<CompiledName("Bind")>]
    let inline bind (f: ^a -> Lens< ^p, ^b>) (m: Lens< ^p, ^a>) : Lens< ^p, ^b> =
        { Lens.Get = fun a -> (f (m.Get a)).Get a
        ; Set = fun a c -> m.Set a (m.Get ((f (m.Get a)).Set a c)) }

    [<CompiledName("Flatten")>]
    let inline flatten (mm: Lens< ^p, Lens< ^p, ^a>>) : Lens< ^p, ^a> =
        { Lens.Get = fun a -> (mm.Get a).Get a
        ; Set = fun a c -> mm.Set a (mm.Get ((mm.Get a).Set a c)) }

    [<CompiledName("RecM")>]
    let inline recM f x =
        let rec go m = bind j m
        and k a = go (unit a)
        and j a = f k a
        j x

    [<CompiledName("RecM1")>]
    let inline recM1 f (x: ^a) : Lens< ^p, ^b> =
        let rec go m = bind k m
        and k a = f go a
        k x

    //[<CompiledName("FoldrM")>]
    //let inline foldrM f (s0: ^s) (source: ^a seq) : Lens< ^p, ^s> =
    //    let g k x s = bind k (f x s)
    //    match source with
    //    | :? array< ^a> as s -> Array.fold g wrap s s0
    //    | :? list<  ^a> as s -> List.fold  g wrap s s0
    //    | _ -> Seq.fold g wrap source s0

    //[<CompiledName("FoldlM")>]
    //let inline foldlM f (s0: ^s) (source: ^a seq) : Lens< ^p, ^s> =
    //    let g x k s = bind k (f s x)
    //    match source with
    //    | :? array< ^a> as s -> Array.foldBack g s wrap s0
    //    | :? list<  ^a> as s -> List.foldBack  g s wrap s0
    //    | _ -> Seq.foldBack g source wrap s0


    module Workflow =

        type LensBuilder () =
            member inline _.Bind(m, f) : Lens< ^p, ^b> = bind f m
            member inline _.Return x : Lens< ^p, ^a> = unit x
            member inline _.ReturnFrom m : Lens< ^p, ^a> = m
            member inline _.Zero() : Lens< ^p, unit> = unit ()

            member inline _.TryWith(body, handler) : Lens< ^p, ^a> =
                try body with e -> handler e
            member inline _.TryFinally(body, finalizer) : Lens< ^p, ^a> =
                try body finally finalizer ()

            member inline _.Using(disp: ^d when ^d :> System.IDisposable, body) : Lens< ^p, ^a> =
                using disp body

            member inline _.While(guard, body) : Lens< ^p, unit> =                    
                let rec go = function
                | false -> unit ()
                | true  -> bind k (body ())
                and k () = go (guard ()) in k ()
 
            member inline _.For(seq: #seq< ^a>, body) : Lens< ^p, unit> =
                use e = seq.GetEnumerator()
                let rec go = function
                | false -> unit ()
                | true  -> b e.Current
                and b x = bind k (body x)
                and k () = go (e.MoveNext()) in k ()


    let lens = Workflow.LensBuilder ()


// Functor

    [<CompiledName("Map")>]
    let inline map (f: ^a -> ^b) (fa: Lens< ^p, ^a>) : Lens< ^p, ^b> =
        { Lens.Get = fun p -> f (fa.Get p)
        ; Set = fun p _ -> fa.Set p (fa.Get p) }


// Applicative

    [<CompiledName("Ap")>]
    let inline ap (fv: Lens< ^p, ^a>) (ff: Lens< ^p, ^a -> ^b>) : Lens< ^p, ^b> =
        bind (fun f -> map f fv) ff

    [<CompiledName("Map2")>]
    let inline map2 (f: ^a -> ^b -> ^c) fa fb =        
        bind (fun a -> map (f a) fb) fa

    [<CompiledName("AndThen")>]
    let inline andThen (fb: Lens< ^p, ^b>) (fa: Lens< ^p, ^a>) =
        map2 (fun _ b -> b) fa fb

    [<CompiledName("When")>]
    let inline when_ condition f = if condition then f () else unit ()


// Profunctor

    [<CompiledName("LMap")>]
    let inline lmap f (pf: Lens< ^a, ^b>) : Lens< ^c, ^b> =
        { Lens.Get = fun x -> pf.Get (f x)
        ; Set = fun a _ -> a }
    
    [<CompiledName("Dimap")>]
    let inline dimap (f: ^c -> ^a) (g: ^b -> ^d) (pf: Lens< ^a, ^b>) : Lens< ^c, ^d> =
        map g (lmap f pf)

    [<CompiledName("RMap")>]
    let inline rmap g (pf: Lens< ^a, ^b>) : Lens< ^a, ^c> = map g pf


// Semigroup

    let inline append first second : Lens< ^p, ^a> when ^a : (static member Append: ^a -> ^a -> ^a) =
        map2 (fun a b -> (^a: (static member Append: ^a -> ^a -> ^a) (a, b))) first second


// Traversable

    [<CompiledName("Sequence")>]
    let inline sequence (source: Lens< ^p, ^a> seq) : Lens< ^p, ^a seq> =
        let cons x xs = x::xs
        let g x xs = map2 cons x xs
        let z = unit []
        match source with
        | :? array<Lens< ^p, ^a>> as s -> Array.foldBack g s z
        | :? list< Lens< ^p, ^a>> as s -> List.foldBack  g s z
        | _ -> Seq.foldBack g source z
        |> map System.Linq.Enumerable.AsEnumerable

    [<CompiledName("Traverse")>]
    let inline traverse f (source: seq< ^a>) : Lens< ^c, ^b seq> =
        match source with
        | :? array< ^a> as s -> sequence (Array.map f s)
        | :? list<  ^a> as s -> sequence [| for x in s -> f x |]
        | :? ResizeArray< ^a> as s -> sequence [| for i = 0 to s.Count - 1 do f s.[i] |]
        | _ -> sequence (System.Linq.Enumerable.Select(source, f))


// Cat

    [<CompiledName("Identity")>]
    let identity<'a> : Lens< ^a, ^a> = { Lens.Get = id ; Set = fun x _ -> x }

    [<CompiledName("Compose")>]
    let inline compose (o2: Lens< ^b, ^c>) (o1: Lens< ^a, ^b>) : Lens< ^a, ^c> =
        { Lens.Get = fun a -> o2.Get(o1.Get a)
        ; Set = fun a c -> o1.Set a (o2.Set (o1.Get a) c) }


// Arrow

    [<CompiledName("Arr")>]
    let inline arr f : Lens< ^a, ^b> =
        { Lens.Get = f
        ; Set = fun a _ -> a }

    [<CompiledName("ArrFst")>]
    let inline arrFst (arrow: Lens< ^a, ^b>) : Lens< ^a * ^c, ^b * ^c> =
        { Lens.Get = fun (a, c) -> arrow.Get a, c
        ; Set = fun (a, _) (b, c) -> arrow.Set a b, c }

    [<CompiledName("ArrSnd")>]
    let inline arrSnd (arrow: Lens< ^a, ^b>) : Lens< ^c * ^a, ^c * ^b> =
        { Lens.Get = fun (c, a) -> c, arrow.Get a
        ; Set = fun (_, a) (c, b) -> c, arrow.Set a b }

    [<CompiledName("Split")>]
    let inline split (a2: Lens< ^c, ^d>) (a1: Lens< ^a, ^b>) : Lens< ^a * ^c, ^b * ^d> =
        { Lens.Get = fun (a, c) -> a1.Get a, a2.Get c
        ; Set = fun (a, c) (b, d) -> a1.Set a b, a2.Set c d }

    [<CompiledName("Fanout")>]
    let inline fanout (a2: Lens< ^a, ^c>) (a1: Lens< ^a, ^b>) : Lens< ^a, ^b * ^c> =
        { Lens.Get = fun a -> a1.Get a, a2.Get a
        ; Set = fun a (b, c) -> a2.Set (a1.Set a b) c }


// Choice

    [<CompiledName("Feed1")>]
    let inline feed1 (arrow: Lens< ^a, ^b>) : Lens<Choice< ^a, ^c>, Choice< ^b, ^c>> =
        { Lens.Get = function
            | Choice1Of2 a -> Choice1Of2 (arrow.Get a)
            | Choice2Of2 c -> Choice2Of2 c
        ; Set = fun ac bc ->
            match bc with
            | Choice1Of2 b -> match ac with
                              | Choice1Of2 a -> Choice1Of2 (arrow.Set a b)
                              | Choice2Of2 c -> Choice2Of2 c
            | Choice2Of2 c -> Choice2Of2 c }

    [<CompiledName("Feed2")>]
    let inline feed2 (arrow: Lens< ^a, ^b>) : Lens<Choice< ^c, ^a>, Choice< ^c, ^b>> =
        { Lens.Get = function
            | Choice1Of2 c -> Choice1Of2 c
            | Choice2Of2 a -> Choice2Of2 (arrow.Get a)
        ; Set = fun ca cb ->
            match cb with
            | Choice1Of2 c -> Choice1Of2 c
            | Choice2Of2 b -> match ca with
                              | Choice1Of2 c -> Choice1Of2 c
                              | Choice2Of2 a -> Choice2Of2 (arrow.Set a b) }

    [<CompiledName("Merge")>]
    let inline merge (a2: Lens< ^c, ^d>) (a1: Lens< ^a, ^b>)
        : Lens<Choice< ^a, ^c>, Choice< ^b, ^d>> =
        { Lens.Get = function
            | Choice1Of2 a -> Choice1Of2 (a1.Get a)
            | Choice2Of2 c -> Choice2Of2 (a2.Get c)
        ; Set = fun ac bd ->
            match bd with
            | Choice1Of2 b -> match ac with
                              | Choice1Of2 a -> Choice1Of2 (a1.Set a b)
                              | Choice2Of2 c -> Choice2Of2 c
            | Choice2Of2 d -> match ac with
                              | Choice1Of2 a -> Choice1Of2 a
                              | Choice2Of2 c -> Choice2Of2 (a2.Set c d) }

    [<CompiledName("Fanin")>]
    let inline fanin (a2: Lens< ^c, ^b>) (a1: Lens< ^a, ^b>) : Lens<Choice< ^a, ^c>, ^b> =
        { Lens.Get = function
            | Choice1Of2 a -> a1.Get a
            | Choice2Of2 c -> a2.Get c
        ; Set = fun c b ->
            match c with
            | Choice1Of2 a -> Choice1Of2 (a1.Set a b)
            | Choice2Of2 c -> Choice2Of2 (a2.Set c b) }


// Apply

    [<CompiledName("App")>]
    let app<'a, 'b> : Lens<Lens< ^a, ^b> * ^a, ^b> =
        { Lens.Get = fun (l, a) -> l.Get a
        ; Set = fun (l, a) b ->
            { Lens.Get = fun _ -> l.Get a
            ; Set = fun _ _ -> l.Set a b }, a }