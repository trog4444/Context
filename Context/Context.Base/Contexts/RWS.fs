namespace Rogz.Context.Base


[<Struct>]
type RWSResult<'State, 'Log, 'T> =
    { State: 'State
    ; Log:   'Log
    ; Value: 'T }


[<Struct; NoComparison; NoEquality>]
type RWS<'Env, 'State, 'Log, 'T> = RWS of ('Env -> 'State -> RWSResult<'State, 'Log, 'T>) with

// Function
    member inline s.Invoke(env, state) = let (RWS r) = s in r env state


module RWS =

// Util
    
    //let inline private append' a b = (^a: (static member Append: ^a -> ^a -> ^a) (a, b))
    let inline private zero_ () = (^m: (static member Zero: unit -> ^m) ())


// Interop

    let inline fromFunc (f: System.Func< ^e, ^s, RWSResult< ^s, ^w, ^a>>) =
        RWS (fun e s -> f.Invoke(e, s))


// Minimal

    // Reader
    
    let inline ask () : RWS< ^e, ^s, ^w, ^e> =
        RWS (fun e s -> { RWSResult.State = s; Log = zero_  (); Value = e })

    let inline asks (func: ^e -> ^a) : RWS< ^e, ^s, ^w, ^a> =
        RWS (fun e s ->
            { RWSResult.State = s
            ; Log = zero_  ()
            ; Value = func e })

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
        RWS (fun _ s -> { RWSResult.State = s; Log = zero_  (); Value = s })

    let inline put state : RWS< ^e, ^s, ^w, unit> =
        RWS (fun _ _ -> { RWSResult.State = state; Log = zero_  (); Value = () })

    let inline modify (modification: ^s -> ^s) : RWS< ^e, ^s, ^w, unit> =
        RWS (fun _ s ->
            { RWSResult.State = modification s
            ; Log = zero_  ()
            ; Value = () })

    let inline gets (proj: ^s -> ^a) : RWS< ^e, ^s, ^w, ^a> =
        RWS (fun _ s ->
            { RWSResult.State = s
            ; Log = zero_  ()
            ; Value = proj s })


// Primitives

    let inline runRWS (env: ^e) state (RWS r) : RWSResult< ^s, ^w, ^a> = r env state
  
    let inline mapRWS (mapping: RWSResult< ^s, ^w0, ^a> -> RWSResult< ^s, ^w1, ^b>) (RWS r) : RWS< ^e, ^s, ^w1, ^b> =
        RWS (fun e s -> mapping (r e s))
    
    let inline withRWS (f: ^e1 -> ^s -> struct (^e0 * ^s)) (RWS r) : RWS< ^e1, ^s, ^w, ^a> =
        RWS (fun e s -> let struct (e', s') = f e s in r e' s')


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
        RWS (fun _ s -> { RWSResult.State = s; Log = zero_  (); Value = value })

    let inline ap (RWS rv) (RWS rf) : RWS< ^e, ^s, ^w, ^b> =
        RWS (fun e s ->
            let f = rf e s
            let v = rv e f.State
            { RWSResult.State = v.State
            ; Log = (f.Log: ^w) + (v.Log: ^w)
            ; Value = f.Value (v.Value: ^a) })

    let inline map2 (f: ^a -> ^b -> ^c) (RWS ra) (RWS rb) : RWS< ^e, ^s, ^w, ^c> =
        RWS (fun e s ->
            let a = ra e s
            let b = rb e a.State
            { RWSResult.State = b.State
            ; Log = (a.Log: ^w) + (b.Log: ^w)
            ; Value = f a.Value b.Value })

    //let inline andthen (RWS rb) (RWS ra) : RWS< ^e, ^s, ^w, ^b> =
    //    RWS (fun e s ->
    //        let a : RWSResult< ^s, ^w, ^a> = ra e s
    //        let b = rb e a.State
    //        { b with RWSResult.Log = append' a.Log b.Log })

    let inline sequence (source: seq<RWS< ^e, ^s, ^w, ^a>>) : RWS< ^e, ^s, ^w, ^a seq> =
        RWS (fun e s ->
            let mutable s = s
            let mutable l = zero_  ()
            let xs = ResizeArray<_>()
            for (RWS r) in source do
                let rs = r e s
                s <- rs.State
                l <- l + rs.Log
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
            { rb with RWSResult.Log = (ra.Log: ^w) + (rb.Log: ^w) })

    let inline flatten (mm: RWS< ^e, ^s, ^w, RWS< ^e, ^s, ^w, ^a>>) : RWS< ^e, ^s, ^w, ^a> = bind id mm

    // foldlM
    // foldrM

    let inline fixM
        (loop: (^a -> RWS< ^e, ^s, ^w, ^b>) -> (RWS< ^e, ^s, ^w, ^a> -> RWS< ^e, ^s, ^w, ^b>) -> ^a -> RWS< ^e, ^s, ^w, ^b>)
        (em: Choice< ^a, RWS< ^e, ^s, ^w, ^a>>) : RWS< ^e, ^s, ^w, ^b> =
        RWS (fun e s ->
            let mutable w = zero_ ()
            let mutable s = s
            let rec go (RWS r) =
                let res = r e s
                w <- w + res.Log
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
            member inline _.Zero() : RWS< ^e, ^s, ^w, unit> = unit ()
            member inline _.Bind((m: RWS< ^e, ^s, ^w, ^a>), f: ^a -> RWS< ^e, ^s, ^w, ^b>) : RWS< ^e, ^s, ^w, ^b> = bind f m            

    let rws = Workflow.RWSBuilder()


// Semigroup

    let inline append (second: RWS< ^e, ^s, ^w, ^a>) (first: RWS< ^e, ^s, ^w, ^a>) : RWS< ^e, ^s, ^w, ^a> = map2 ( + ) first second