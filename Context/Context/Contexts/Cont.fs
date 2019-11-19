namespace Rogz.Context.Data.Cont


module Cont =

// Interop

    [<CompiledName("Make")>]
    let inline make (cont: System.Func<System.Func< ^a, ^r>, ^r>) =
        Cont (fun k -> cont.Invoke(System.Func<_,_>k))


// Minimal

    let inline callCC (f: ((^a -> Cont< ^r, ^``_``>) -> Cont< ^r, ^a>)) =
        Cont (fun k -> let (Cont cc) = f (fun x -> Cont (fun _ -> k x)) in cc k)

    let inline shift f : Cont< ^r, ^a> =
        Cont (fun k -> let (Cont cc) = f k in cc id)

    let inline reset (Cont cc) : Cont< ^r0, ^r> =
        Cont (fun k -> k (cc id))


// Primitives

    let inline runCont (k: ^a -> ^r) (Cont cc) = cc k
    
    let inline evalCont (Cont cc) : ^r = cc id

    let inline mapCont (f: ^r -> ^r) (Cont cc) : Cont< ^r, ^a> =
        Cont (fun k -> cc (fun a -> f (k a)))

    let inline withCont (f: (^b -> ^r) -> ^a -> ^r) (Cont cc) =
        Cont (fun k -> cc (fun a -> f k a))

    let inline getCC (x0: ^a) : Cont< ^r, struct (^a * (^a -> Cont< ^r, ^``_``>))> =
        callCC (fun c -> let rec f x = c (struct (x, f))
                         Cont (fun k -> k (x0, f)))

    let quitCC (x: 'r) : Cont< ^r, '``_``> = Cont (fun _ -> x)    

    let inline cache (Cont cc) : Cont< ^r, ^a> when ^a: equality =
        let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        Cont (fun k ->
            cc (fun a -> match d.TryGetValue(a) with
                         | true, r -> r
                         | false, _ -> let r = k a in d.[a] <- r; r))


// Functor

    let inline map (f: ^a -> ^b) (Cont cc) : Cont< ^r, ^b> =
        Cont (fun k -> cc (fun a -> k (f a)))


// Applicative

    let inline unit value : Cont< ^r, ^a> = Cont (fun k -> k value)

    let inline ap (Cont cv) (Cont cf) : Cont< ^r, ^b> =
        Cont (fun k -> cf (fun f -> cv (fun (v: ^a) -> k (f v))))

    let inline map2 (f: ^a -> ^b -> ^c) (Cont ca) (Cont cb) : Cont< ^r, ^c> =
        Cont (fun k -> ca (fun a -> cb (fun b -> k (f a b))))

    let inline andthen (Cont cb) (Cont ca) : Cont< ^r, ^b> =
        Cont (fun k -> ca (fun (_: ^a) -> cb k))


// Monad

    let inline bind (f: ^a -> Cont< ^r, ^b>) (Cont cc) =
        Cont (fun k -> cc (fun a -> let (Cont cb) = f a in cb k))

    let inline flatten (Cont ccc) : Cont< ^r, ^a> =
        Cont (fun k -> ccc (fun (Cont cc) -> cc k))

    let inline fixM loop (em: Rogz.Context.Data.Either.Either< ^a, Cont< ^r, ^a>>) : Cont< ^r, ^b> =
        let rec go m = bind k m
        and k a = loop k go a
        match em with
        | Rogz.Context.Data.Either.Left a  -> k a
        | Rogz.Context.Data.Either.Right m -> go m


    //type Writer<'w, 'a> = { Log: 'w ; Value: 'a }
    
    //Writer still needs to be optimized to prevent stack overflows
    //and still requires Empty method
    //therefore do ALL types still need a dependency on unit?
    //type T = T of string with
    //    member s.Append (T b) = let (T a) = s in T (sprintf "%s-%s" a b)
    //    static member Empty() = T ""
    //let inline recM f x =
    //    let mutable log = (^w: (static member Empty: unit -> ^w) ())
    //    let inline ( >>= ) (w: Writer< ^w, ^a>) f : Writer< ^w, ^b> =
    //        let w' = f w.Value
    //        log <- (^w: (member Append: ^w -> ^w) (log, w'.Log))
    //        w'
    //    let rec go m = m >>= k
    //    and k a = f k go a
    //    { k x with Writer.Log = log }
    //let f k i a =
    //    if a >= 100 then { Writer.Log = T (string a); Value = string a }
    //    elif a % 10 = 0 then k (a + 1)
    //    else i { Writer.Log = T (string a) ; Value = a + 1 }
    //let w = recM f 1



    // foldlM
    // foldrM


    [<RequireQualifiedAccess>]
    module Workflow =

        type ContBuilder() =
            member inline _.Return(x) : Cont< ^r, ^a> = unit x
            member inline _.ReturnFrom(m) : Cont< ^r, ^a> = m
            member inline _.Bind(m, f: (^a -> Cont< ^r, ^b>)) = bind f m
            member inline _.Zero() : Cont< ^r, unit> = unit ()
            member inline _.Using(disp: ^d, f: ^d -> Cont< ^r, ^a>) : Cont< ^r, ^a> when ^d :> System.IDisposable = using disp f
            member inline _.TryWith(m, handler: exn -> Cont< ^r, ^a>) = try m with e -> handler e
            member inline _.TryFinally(m: Cont< ^r, ^a>, finalizer: unit -> unit) = try m finally finalizer ()


    let cont = Workflow.ContBuilder()


// Comonad

    let inline extract (Cont cc) : ^a = cc id

    let inline extend (f: Cont< ^r, ^a> -> ^b) (w: Cont< ^r, ^a>) : Cont< ^r, ^b> =
        Cont (fun k -> k (f w))

    let inline duplicate w : Cont< ^r, Cont< ^r, ^a>> = Cont (fun k -> k w)


//// Foldable

//    let inline fold (folder: ^s -> ^a -> ^s) (seed: ^s) (Cont cc) : ^s =
//        let mutable s = seed in cc (fun a -> s <- folder s a)
//        s
//    let inline foldBack (folder: ^a -> ^s -> ^s) (seed: ^s) (Cont cc) : ^s =        
//        let ra = ResizeArray<_>() in cc ra.Add
//        let mutable s = seed in for i = ra.Count - 1 downto 0 do s <- folder ra.[i] s
//        s

//    let inline foldl (folder: (unit -> ^s) -> ^a -> ^s) seed (Cont cc) =
//        let ra = ResizeArray<_>() in cc ra.Add
//        let mutable ``cont?`` = true
//        let f' z i =
//            let z = ``cont?`` <- false
//                    folder (fun () -> ``cont?`` <- true; z ()) ra.[i]
//            in fun () -> z
//        let rec go z i =
//            if ``cont?`` && i < ra.Count
//            then go (f' z i) (i + 1)
//            else z ()
//        if ra.Count = 0 then seed () else go seed 0

//    let inline foldr (folder: ^a -> (unit -> ^s) -> ^s) seed (Cont cc) =
//        let ra = ResizeArray<_>() in cc ra.Add
//        let rec go n = if n < ra.Count then folder ra.[n] (fun () -> go (n + 1)) else seed ()
//        go 0    

//    let inline mapFold (mapping: ^s -> ^a -> struct (^b * ^s)) seed (Cont cc) : struct (Cont<unit, ^b> * ^s) =
//        let bs = ResizeArray<_>()
//        let mutable s = seed
//        cc (fun a -> let struct (b, z) = mapping s a
//                     bs.Add(b)
//                     s <- z)
//        Cont (fun k -> for x in bs do k x), s

//    let inline mapFoldBack (mapping: ^a -> ^s -> struct (^b * ^s)) seed (Cont cc) : struct (Cont<unit, ^b> * ^s) =
//        let az = ResizeArray<_>() in cc az.Add
//        let bz = ResizeArray<_>(az.Count)
//        let mutable s = seed
//        for i = az.Count - 1 downto 0 do
//            let struct (b, z) = mapping az.[i] s
//            bz.Add(b)
//            s <- z
//        Cont (fun k -> for x in bz do k x), s


// Traversable

    let inline sequence (source: seq<Cont< ^r, ^a>>) : Cont< ^r, seq< ^a>> =
        let cons x xs = x::xs
        let f x xs = map2 cons x xs
        let z = unit []
        match source with
        | :? array<Cont< ^r, ^a>> as s -> Array.foldBack f s z
        | :? list< Cont< ^r, ^a>> as s -> List.foldBack f s z
        | _ -> Seq.foldBack f source z
        |> map System.Linq.Enumerable.AsEnumerable

    let inline traverse (f: ^a -> Cont< ^r, ^b>) source : Cont< ^r, seq< ^b>> =
        sequence (System.Linq.Enumerable.Select(source, System.Func<_,_>f))