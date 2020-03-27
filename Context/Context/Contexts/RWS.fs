namespace Rogz.Context.Data.RWS


module RWS =


// Util
    
    let inline private append' a b = (^a: (static member Append: ^a -> ^a -> ^a) (a, b))
    let inline private mt() = (^m: (static member Empty: unit -> ^m) ())


// Interop

    let inline fromFunc (f: System.Func< ^e, ^s, RWSResult< ^s, ^w, ^a>>) =
        RWS (fun e s -> f.Invoke(e, s))


// Minimal

    // Reader
    
    let inline ask () : RWS< ^e, ^s, ^w, ^e> =
        RWS (fun e s -> { RWSResult.State = s; Log = mt (); Value = e })

    let inline asks (query: ^e -> ^a) : RWS< ^e, ^s, ^w, ^a> =
        RWS (fun e s ->
            { RWSResult.State = s
            ; Log = mt ()
            ; Value = query e })

    let inline local (localize: ^e -> ^e) (RWS r) : RWS< ^e, ^s, ^w, ^a> =
        RWS (fun e s -> r (localize e) s)


    // Writer

    let tell (record: 'w) : RWS<'e, 's, ^w, unit> =
        RWS (fun _ s -> { RWSResult.State = s; Log = record; Value = () })

    let listen (RWS r) : RWS<'e, 's, 'w, struct (^w * 'a)> =
        RWS (fun e s ->
            let res = r e s
            { RWSResult.State = res.State
            ; Log = res.Log
            ; Value = (res.Log, res.Value) })

    let inline listens (f: ^w -> ^a -> ^b) (RWS r) : RWS< ^e, ^s, ^w, ^b> =
        RWS (fun e s ->
            let res = r e s
            { RWSResult.State = res.State
            ; Log = res.Log
            ; Value = f res.Log res.Value })


    // State

    let inline get () : RWS< ^e, ^s, ^w, ^s> =
        RWS (fun _ s -> { RWSResult.State = s; Log = mt (); Value = s })

    let inline put state : RWS< ^e, ^s, ^w, unit> =
        RWS (fun _ _ -> { RWSResult.State = state; Log = mt (); Value = () })

    let inline modify (modification: ^s -> ^s) : RWS< ^e, ^s, ^w, unit> =
        RWS (fun _ s ->
            { RWSResult.State = modification s
            ; Log = mt ()
            ; Value = () })

    let inline gets (proj: ^s -> ^a) : RWS< ^e, ^s, ^w, ^a> =
        RWS (fun _ s ->
            { RWSResult.State = s
            ; Log = mt ()
            ; Value = proj s })


// Primitives

    let inline runRWS (env: ^e) state (RWS r) : RWSResult< ^s, ^w, ^a> = r env state
  
    let inline mapRWS (mapping: RWSResult< ^s, ^w0, ^a> -> RWSResult< ^s, ^w1, ^b>) (RWS r) : RWS< ^e, ^s, ^w1, ^b> =
        RWS (fun e s -> mapping (r e s))
    
    let inline withRWS (f: ^e1 -> ^s -> struct (^e0 * ^s)) (RWS r) : RWS< ^e1, ^s, ^w, ^a> =
        RWS (fun e s -> let struct (e', s') = f e s in r e' s')
  
    //let inline cache (RWS r) : RWS< ^e, ^s, ^w, ^a> =
    //    let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
    //    RWS (fun e s ->
    //        let p = struct (e, s)
    //        match d.TryGetValue(p) with
    //        | true, rs -> rs
    //        | false, _ -> let rs = r e s in d.[p] <- rs; rs)


// Functor

    let inline map (f: ^a -> ^b) (RWS r) : RWS< ^e, ^s, ^w, ^b> =
        RWS (fun e s ->
            let x = r e s
            { RWSResult.State = x.State
            ; Log = x.Log
            ; Value = f x.Value })


// Bifunctor (over Log and Value)

    let inline bimap (f: ^a -> ^c) (g: ^b -> ^d) (RWS r) : RWS< ^e, ^s, ^c, ^d> =
        RWS (fun e s ->
            let rs = r e s
            { RWSResult.State = rs.State
            ; Log = f rs.Log
            ; Value = g rs.Value })

    let inline mapFirst (f: ^a -> ^c) (RWS r) : RWS< ^e, ^s, ^c, ^b> =
        RWS (fun e s ->
            let rs = r e s
            { RWSResult.State = rs.State
            ; Log = f rs.Log
            ; Value = rs.Value })


// Applicative

    let inline unit value : RWS< ^e, ^s, ^w, ^a> =
        RWS (fun _ s -> { RWSResult.State = s; Log = mt (); Value = value })

    let inline ap (RWS rv) (RWS rf) : RWS< ^e, ^s, ^w, ^b> =
        RWS (fun e s ->
            let f = rf e s
            let v = rv e f.State
            { RWSResult.State = v.State
            ; Log = append' f.Log v.Log
            ; Value = f.Value (v.Value: ^a) })

    let inline map2 (f: ^a -> ^b -> ^c) (RWS ra) (RWS rb) : RWS< ^e, ^s, ^w, ^c> =
        RWS (fun e s ->
            let a = ra e s
            let b = rb e a.State
            { RWSResult.State = b.State
            ; Log = append' a.Log b.Log
            ; Value = f a.Value b.Value })

    //let inline andthen (RWS rb) (RWS ra) : RWS< ^e, ^s, ^w, ^b> =
    //    RWS (fun e s ->
    //        let a : RWSResult< ^s, ^w, ^a> = ra e s
    //        let b = rb e a.State
    //        { b with RWSResult.Log = append' a.Log b.Log })

    let inline sequence (source: seq<RWS< ^e, ^s, ^w, ^a>>) : RWS< ^e, ^s, ^w, ^a seq> =
        RWS (fun e s ->
            let mutable s = s
            let mutable l = mt ()
            let xs = ResizeArray<_>()
            for (RWS r) in source do
                let rs = r e s
                s <- rs.State
                l <- append' l rs.Log
                xs.Add(rs.Value)
            { RWSResult.State = s
            ; Log = l
            ; Value = System.Linq.Enumerable.AsEnumerable(xs) })

    let inline traverse (f: ^a -> RWS< ^e, ^s, ^w, ^b>) (source: ^a seq) : RWS< ^e, ^s, ^w, ^b seq> =
        sequence (System.Linq.Enumerable.Select(source, fun x -> f x))


// Biapplicative (over Log and Value)

    let biunit a b : RWS<'e, 's, 'a, 'b> =
        RWS (fun _ s -> { RWSResult.State = s; Log = a; Value = b })

    let inline bimap2 (f: ^a -> ^b -> ^c) (g: ^d -> ^e -> ^f) (RWS rad) (RWS rbe) : RWS< ^env, ^s, ^c, ^f> =
        RWS (fun e s ->
            let ad = rad e s
            let be = rbe e ad.State
            { RWSResult.State = be.State
            ; Log = f ad.Log be.Log
            ; Value = g ad.Value be.Value })


// Monad

    let inline bind (f: ^a -> RWS< ^e, ^s, ^w, ^b>) (RWS r) : RWS< ^e, ^s, ^w, ^b> =
        RWS (fun e s ->
            let ra = r e s
            let rb = runRWS e ra.State (f ra.Value)
            { rb with RWSResult.Log = append' ra.Log rb.Log })

    let inline flatten mm : RWS< ^e, ^s, ^w, ^a> = bind id mm

    // foldlM
    // foldrM

    let inline fixM
        (loop: (^a -> RWS< ^e, ^s, ^w, ^b>) -> (RWS< ^e, ^s, ^w, ^a> -> RWS< ^e, ^s, ^w, ^b>) -> ^a -> RWS< ^e, ^s, ^w, ^b>)
        (em: Choice< ^a, RWS< ^e, ^s, ^w, ^a>>) : RWS< ^e, ^s, ^w, ^b> =
        RWS (fun e s ->
            let mutable w = mt ()
            let mutable s = s
            let rec go (RWS r) =
                let res = r e s
                w <- append' w res.Log
                s <- res.State
                k res.Value
            and k a = loop k go a
            let res = (match em with Choice1Of2 a -> k a | Choice2Of2 m -> go m) |> runRWS e s
            { res with
                RWSResult.State = s
                Log = w })

    // foldlM
    // foldrM

    [<RequireQualifiedAccess>]
    module Workflow =

        type RWSBuilder() =
            member inline _.Return(x) : RWS< ^e, ^s, ^w, ^a> = unit x
            member _.ReturnFrom(m) : RWS<'e, 's, 'w, 'a> = m
            member inline _.Bind(m, f: ^a -> RWS< ^e, ^s, ^w, ^b>) = bind f m
            member inline _.Zero() : RWS< ^e, ^s, ^w, unit> = unit ()
            //member inline _.Using(disp: ^d, f: ^d -> RWS< ^e, ^s, ^w, ^a>) : RWS< ^e, ^s, ^w, ^a> when ^d :> System.IDisposable = using disp f
            //member inline _.TryWith(m: RWS< ^e, ^s, ^w, ^a>, handler: exn -> RWS< ^e, ^s, ^w, ^a>) = try m with e -> handler e
            //member inline _.TryFinally(m: RWS< ^e, ^s, ^w, ^a>, finalizer: unit -> unit) = try m finally ()
            //abstract member Using: disp: 'd * f: ('d -> RWS<'e, 's, 'w, 'a>) -> RWS<'e, 's, 'w, 'a> when 'd :> System.IDisposable
            //abstract member TryWith: m: RWS<'e, 's, 'w, 'a> * h: (exn -> RWS<'e, 's, 'w, 'a>) -> RWS<'e, 's, 'w, 'a>
            //abstract member TryFinally: m: RWS<'e, 's, 'w, 'a> * f: (unit -> unit) -> RWS<'e, 's, 'w, 'a>
            //member _.Using(disp: 'd, f) : RWS<'e, 's, 'w, 'a> when 'd :> System.IDisposable = using disp f
            //default _.TryWith(m, h) : RWS<'e, 's, 'w, 'a> = try m with e -> h e
            //default _.TryFinally(m, f) : RWS<'e, 's, 'w, 'a> = try m finally f ()


    let rws = Workflow.RWSBuilder()


// Semigroup

    let inline append first second : RWS< ^e, ^s, ^w, ^a> = map2 append' first second


//// Cat

//    //let inline identity () : RWS< ^a, ^s, ^w, ^a> =
//    //    RWS (fun e s -> { RWSResult.State = s
//    //                    ; Log = mt ()
//    //                    ; Value = e })

//    let inline compose (RWS rbc) (RWS rab) : RWS< ^a, ^s, ^w, ^c> =
//        RWS (fun a s ->
//            let ab = rab a s
//            let bc = rbc (ab.Value: ^b) ab.State
//            { bc with RWSResult.Log = append' ab.Log bc.Log })


//// Arrow

//    let inline arr f : RWS< ^a, ^s, ^w, ^b> =
//        RWS (fun a s ->
//            { RWSResult.State = s
//            ; Log = mt ()
//            ; Value = f a })

//    let inline first (RWS r) : RWS< ^a * ^c, ^s, ^w, ^b * ^c> =
//        RWS (fun (a, c) s ->
//            let x = r a s
//            { RWSResult.State = s
//            ; Log = x.Log
//            ; Value = x.Value, c })

//    let inline second (RWS r) : RWS< ^c * ^a, ^s, ^w, ^c * ^b> =
//        RWS (fun (c, a) s ->
//            let x = r a s
//            { RWSResult.State = s
//            ; Log = x.Log
//            ; Value = c, x.Value })

//    let inline split (RWS cd) (RWS ab) : RWS< ^a * ^c, ^s, ^w, ^b * ^d> =
//        RWS (fun (a, c) s ->
//            let rac = ab a s
//            let rcd = cd c rac.State
//            { RWSResult.State = s
//            ; Log = append' rac.Log rcd.Log
//            ; Value = rac.Value, rcd.Value })

//    let inline fanout (RWS ac) (RWS ab) : RWS< ^a, ^s, ^w, ^b * ^c> =
//        RWS (fun a s ->
//            let rb = ab a s
//            let rc = ac a rb.State
//            { RWSResult.State = rc.State
//            ; Log = append' rb.Log rc.Log
//            ; Value = rb.Value, rc.Value })


//// Arrow.Choice


//// Arrow.Apply