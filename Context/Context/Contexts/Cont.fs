namespace Rogz.Context.Data.Cont


module Cont =

// Interop

    let inline fromFunc (cont: System.Func<System.Func< ^a, ^r>, ^r>) =
        Cont (fun k -> cont.Invoke(System.Func<_,_>k))


// Minimal

    let inline callCC (f: ((^a -> Cont< ^r, ^``_``>) -> Cont< ^r, ^a>)) =
        Cont (fun k -> let (Cont cc) = f (fun x -> Cont (fun _ -> k x)) in cc k)

    let inline shift f : Cont< ^r, ^a> =
        Cont (fun k -> let (Cont cc) = f k in cc id)

    let reset (Cont cc) : Cont<'r0, 'r> = Cont (fun k -> k (cc id))


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

    //let quitCC (x: 'r) : Cont< ^r, '``_``> = Cont (fun _ -> x)    

    //let inline cache (Cont cc) : Cont< ^r, ^a> when ^a: equality =
    //    let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
    //    Cont (fun k ->
    //        cc (fun a -> match d.TryGetValue(a) with
    //                     | true, r -> r
    //                     | false, _ -> let r = k a in d.[a] <- r; r))


// Functor

    let inline map (f: ^a -> ^b) (Cont cc) : Cont< ^r, ^b> =
        Cont (fun k -> cc (fun a -> k (f a)))


// Applicative

    let unit (value: 'a) : Cont<'r, ^a> = Cont (fun k -> k value)

    let inline ap (Cont cv) (Cont cf) : Cont< ^r, ^b> =
        Cont (fun k -> cf (fun f -> cv (fun (v: ^a) -> k (f v))))

    let inline map2 (f: ^a -> ^b -> ^c) (Cont ca) (Cont cb) : Cont< ^r, ^c> =
        Cont (fun k -> ca (fun a -> cb (fun b -> k (f a b))))

    //let inline andthen (Cont cb) (Cont ca) : Cont< ^r, ^b> =
    //    Cont (fun k -> ca (fun (_: ^a) -> cb k))

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


// Monad

    let inline bind (f: ^a -> Cont< ^r, ^b>) (Cont cc) =
        Cont (fun k -> cc (fun a -> let (Cont cb) = f a in cb k))

    let flatten (Cont ccc) : Cont<'r, 'a> =
        Cont (fun k -> ccc (fun (Cont cc) -> cc k))

    let inline fixM loop em : Cont< ^r, ^b> =
        let rec go m = bind k m
        and k a = loop k go a
        match em with
        | Choice1Of2 a -> k (a: ^a)
        | Choice2Of2 m -> go m

    // foldlM
    // foldrM


    [<RequireQualifiedAccess>]
    module Workflow =

        type ContBuilder() =
            member _.Return(x) : Cont< 'r, 'a> = unit x
            member _.ReturnFrom(m) : Cont<'r, 'a> = m
            member inline _.Bind(m, f: (^a -> Cont< ^r, ^b>)) = bind f m
            member _.Zero() : Cont<'r, unit> = unit ()
            //member inline _.Using(disp: ^d, f: ^d -> Cont< ^r, ^a>) : Cont< ^r, ^a> when ^d :> System.IDisposable = using disp f
            //member inline _.TryWith(m, handler: exn -> Cont< ^r, ^a>) = try m with e -> handler e
            //member inline _.TryFinally(m: Cont< ^r, ^a>, finalizer: unit -> unit) = try m finally finalizer ()
            //abstract member Using: disp: 'd * f: ('d -> Cont<'r, 'a>) -> Cont<'r, 'a> when 'd :> System.IDisposable
            //abstract member TryWith: m: Cont<'r, 'a> * h: (exn -> Cont<'r, 'a>) -> Cont<'r, 'a>
            //abstract member TryFinally: m: Cont<'r, 'a> * f: (unit -> unit) -> Cont<'r, 'a>
            member _.Using(disp: 'd, f) : Cont<'r, 'a> when 'd :> System.IDisposable = using disp f
            //default _.TryWith(m, h) : Cont<'r, 'a> = try m with e -> h e
            //default _.TryFinally(m, f) : Cont<'r, 'a> = try m finally f ()


    let cont = Workflow.ContBuilder()