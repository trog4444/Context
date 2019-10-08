namespace PTR.Context.Type.Incomplete//.Coroutine


//[<Struct; NoComparison; NoEquality>]
//type Coroutine<'T> = internal Co of (('T -> unit) -> unit)


//open FSharp.Core.Printf
//open System.Threading

//type Task<'T> = Tasks.Task<'T>
//type Task = Tasks.Task

//type Action<'T> = System.Action<Task<'T>>
//type Action = System.Action<Task>


//module Coroutine =    

//    let inline Co coroutine : Coroutine< ^a> = Co coroutine

//    let inline invoke r (Co c) =
//        let wc _ = c r
//        ThreadPool.QueueUserWorkItem (WaitCallback wc)

//    let inline invoke_ r (Co c) =
//        let wc _ = c r
//        ThreadPool.QueueUserWorkItem (WaitCallback wc) |> ignore

//    let inline ofTask (t: Task< ^a>) =
//        let cw r (t: Task< ^a>) = r t.Result
//        let co r = t.ContinueWith(Action< ^a>(cw r)) |> ignore
//        Co co

//    let inline ofTask0 (t: Task) =
//        let cw r (_: Task) = r ()
//        let co r = t.ContinueWith(Action(cw r)) |> ignore
//        Co co


//    module private Par =

//        let inline refEq (a: ^T) (b: ^T) : bool when ^T : not struct = obj.ReferenceEquals(a, b)

//        [<NoComparison; NoEquality>]
//        type ChildState<'T> =
//            | Initial
//            | HasReceiver of r: ('T -> unit)
//            | HasValue of v: 'T
//            | Done    

//        let rec cas' (rs: byref<ChildState<'T>>) (cs: ChildState<'T>) u =
//            let v, ns = u cs
//            let acs = Interlocked.CompareExchange(&rs, ns, cs)
//            if refEq acs cs then v
//            else cas' &rs acs u

//        let inline cas (rs: byref<ChildState<_>>) u =
//            let cs = rs
//            Interlocked.MemoryBarrier()
//            cas' &rs cs u

//    open Par


//    let inline inParallel (Co c) =
//        let co r =
//            let mutable state = Initial
//            let cr v =
//                let update = function
//                | Initial -> ValueNone, HasValue v
//                | HasReceiver r -> ValueSome r, Done
//                | HasValue _ -> ValueNone, HasValue v
//                | Done -> ValueNone, Done
//                match cas &state update with
//                | ValueSome r -> r v
//                | ValueNone -> ()
//            c cr
//            let cco cr =
//                let update = function
//                | Initial -> ValueNone, HasReceiver cr
//                | HasReceiver _ -> ValueNone, HasReceiver cr
//                | HasValue v -> ValueSome v, Done
//                | Done -> ValueNone, Done
//                match cas &state update with
//                | ValueSome v -> cr v
//                | ValueNone -> ()
//            r (Co cco)
//        Co co


//    let inline bind f (Co c) =
//        let cr r v = let (Co d) = f v in d r
//        let co r = c (cr r)
//        Co co

//    let inline unit x = let co r = r x in Co co

//    let inline combine (Co c) (Co d) =
//        let cr r _ = d r
//        let co r = c (cr r) in Co co


//    type CoroutineBuilder() =
//        member inline _.Return x = unit x
//        member inline _.ReturnFrom m : Coroutine< ^a> = m
//        member inline _.ReturnFrom m : Coroutine< ^a> = ofTask m
//        member inline _.ReturnFrom m : Coroutine<unit> = ofTask0 m
//        member inline _.Bind(m, f) = bind f m
//        member inline _.Bind(m, f) = bind f (ofTask m)
//        member inline _.Bind(m, f) = bind f (ofTask0 m)

//        //member inline _.Combine(c, d) = combine c d
//        //member inline _.Combine(t, d) = combine (ofTask0 t) d

//        member inline s.Zero() = s.Return()

//        member inline _.Delay f : ^any = f ()
//        member inline _.Run f : ^f = f


//    let t = System.Diagnostics.Stopwatch()
//    System.Threading.Thread.Sleep 1000
//    let cull () = System.GC.Collect()
//    cull ()

//    let inline task f = new Task<_>(fun _ -> f ())

//    let coroutine = CoroutineBuilder()
//    let r = ref 0L
//    t.Start()
//    let result =
//        ofTask (task (fun () -> 1))
//        |> invoke (fun t -> r := int64 t)
//        |> fun b -> b, !r