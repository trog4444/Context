namespace PTR.Context.Type.Incomplete.Stream2

#if COMPILED
type Stream<'T> = ((^T -> bool) -> unit)

[<AutoOpen>]
module StreamPrimitives =        
    let inline ( |Stream| ) (stream: ^T Stream) = Stream stream
    let inline Stream (stream: ((^T -> bool) -> unit)) : ^T Stream = stream
open StreamPrimitives

#else
[<Struct; NoComparison; NoEquality>]
type Stream<'T> = Stream of ((^T -> bool) -> unit)
#endif

module Stream =

    [<CompiledName("OfArray")>]
    let inline ofArray (source: ^a []) : ^a Stream =              
        let stream k =
            let mutable i = 0
            let len = source.Length
            while i < len && k source.[i] do i <- i + 1
        Stream stream

    [<CompiledName("OfList")>]
    let inline ofList (source: ^a list) : ^a Stream =              
        let stream k =
            let rec go = function
            | [] -> ()
            | x::xs -> if k x then go xs else ()
            go source
        Stream stream

    [<CompiledName("OfSeq")>]
    let inline ofSeq (source: ^a seq) : ^a Stream =                
        // do this OPTIMIZATION?
        match source with
        | :? array< ^a> as s -> ofArray s
        | :? list<  ^a> as s -> ofList s
        | :? ResizeArray< ^a> as s ->
            let stream k =
                let mutable i = 0
                let len = s.Count
                while i < len && k s.[i] do i <- i + 1
            Stream stream
        | _ ->
            let stream k =
                use e = source.GetEnumerator()
                while e.MoveNext() && k e.Current do ()
            Stream stream

    [<CompiledName("Map")>]
    let inline map (mapping: ^a -> ^b) (Stream str) : ^b Stream =
        let stream k = str (fun a -> k (mapping a))
        Stream stream

    [<CompiledName("Collect")>]
    let inline collect (projection: ^a -> ^b Stream) (Stream str) : ^b Stream =
        let stream k = str (fun a -> let (Stream str') = projection a in str' k; true)
        Stream stream

    [<CompiledName("CollectMany")>]
    let inline collectMany (projection: ^a -> ^b Stream) (mapping: ^a -> ^b -> ^c) (Stream str) : ^c Stream =
        let stream k =
            str (fun a -> let (Stream str') = projection a
                          str' (fun b -> k (mapping a b))
                          true)
        Stream stream

    [<CompiledName("Filter")>]
    let inline filter (predicate: ^a -> bool) (Stream str) : ^a Stream =
        let str2 k = str (fun a -> if predicate a then k a else true)
        Stream str2

    [<CompiledName("Choose")>]
    let inline choose (chooser: ^a -> ^b PTR.Context.Type.Maybe.Maybe) (Stream str) : ^b Stream =
        let stream k =
            str (fun a -> match chooser a with
                          | PTR.Context.Type.Maybe.Nothing -> true
                          | PTR.Context.Type.Maybe.Just b  -> k b)
        Stream stream

    [<CompiledName("Fold")>]
    let inline fold (folder: ^s -> ^a -> ^s) seed (Stream str) : ^s =
        let mutable s = seed in str (fun a -> s <- folder s a; true)
        s

    [<CompiledName("FoldWhile")>]
    let inline foldWhile (folder: ^s -> ^a -> ^s * bool) seed (Stream str) : ^s =
        let mutable s = seed
        str (fun a -> let z, f = folder s a in s <- z ; f)
        s

    [<CompiledName("Take")>]
    let inline take count (Stream str) : ^a Stream =        
        let stream k =
            let i = ref 0
            let mutable f = true
            let len = max 0 count
            str (fun a -> incr i ; !i <= count && k a)
        Stream stream

    [<CompiledName("TakeWhile")>]
    let inline takeWhile (predicate: ^a -> bool) (Stream str) : ^a Stream =
        let stream k = str (fun a -> predicate a && k a)
        Stream stream

    [<CompiledName("CacheStream")>]
    let inline cacheStream (Stream str) : ^a Stream when ^a : equality =
        let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        let s k =
            str (fun a -> match d.TryGetValue(a) with
                          | true, r  -> r
                          | false, _ -> let r = k a in d.[a] <- r ; r)
        Stream s


    //open System.Linq
    //async {        

    //    let inline add a = a + 1L
    //    let inline sub a = a - 1L
    //    let inline ftrue _ = true
    //    let inline tkW a = a < 90_000_000L
    //    let inline prj a = Seq.replicate 3 a
    //    let inline prj' a = ofSeq (prj a)
    //    let s = 0L
    //    let xs = [|1..100_000_000|]
    //    let xss = seq { 1..100_000_000 }
    //    let ys = [|1..1_000|]
    //    //let xs' = [1..50_000_000]
    //    let cull () = System.GC.Collect()
    //    let t = System.Diagnostics.Stopwatch()
    //    printfn "starting..."
    //    cull ()

    //    cull ()
    //    t.Restart()
    //    let ofArray_big =
    //        xs
    //        |> ofArray
    //        |> map string
    //        |> filter ftrue 
    //        |> map int64
    //        |> filter ftrue
    //        |> map add
    //        |> filter ftrue
    //        |> map sub
    //        |> collect prj'
    //        |> takeWhile tkW
    //        |> fold (+) s
    //    cull ()
    //    printfn "tofArray_big = %i" t.ElapsedMilliseconds
    //    t.Reset()

    //    cull ()
    //    t.Restart()
    //    let ofSeq_big =
    //        xs
    //        |> ofSeq
    //        |> map string
    //        |> filter ftrue 
    //        |> map int64
    //        |> filter ftrue
    //        |> map add
    //        |> filter ftrue
    //        |> map sub
    //        |> collect prj'
    //        |> takeWhile tkW
    //        |> fold (+) s
    //    cull ()
    //    printfn "tofSeq_big = %i" t.ElapsedMilliseconds
    //    t.Reset()

    //    cull ()
    //    t.Restart()
    //    let Seq_big =
    //        xs
    //        |> Seq.map string
    //        |> Seq.filter ftrue 
    //        |> Seq.map int64
    //        |> Seq.filter ftrue
    //        |> Seq.map add
    //        |> Seq.filter ftrue
    //        |> Seq.map sub
    //        |> Seq.collect prj
    //        |> Seq.takeWhile tkW
    //        |> Seq.fold (+) s
    //    cull ()
    //    printfn "tSeq_big = %i" t.ElapsedMilliseconds
    //    t.Reset()

    //    cull ()
    //    t.Restart()
    //    let linq_big =
    //        xs
    //         .Select(fun a -> string a)
    //         .Where(fun a -> ftrue a) 
    //         .Select(fun a -> int64 a)
    //         .Where(fun a -> ftrue a)
    //         .Select(fun a -> add a)
    //         .Where(fun a -> ftrue a)
    //         .Select(fun a -> sub a)
    //         .SelectMany(fun a -> prj a)
    //         .TakeWhile(fun a -> tkW a)
    //         .Aggregate(s, fun a b -> a + b)
    //    cull ()
    //    printfn "tlinq_big = %i" t.ElapsedMilliseconds
    //    t.Reset()

    //    cull ()
    //    t.Restart()
    //    let sq_ofArray_big =
    //        xss
    //        |> Seq.toArray
    //        |> ofArray
    //        |> map string
    //        |> filter ftrue 
    //        |> map int64
    //        |> filter ftrue
    //        |> map add
    //        |> filter ftrue
    //        |> map sub
    //        |> collect prj'
    //        |> takeWhile tkW
    //        |> fold (+) s
    //    cull ()
    //    printfn "sq_tofArray_big = %i" t.ElapsedMilliseconds
    //    t.Reset()

    //    cull ()
    //    t.Restart()
    //    let sq_ofSeq_big =
    //        xss
    //        |> ofSeq
    //        |> map string
    //        |> filter ftrue 
    //        |> map int64
    //        |> filter ftrue
    //        |> map add
    //        |> filter ftrue
    //        |> map sub
    //        |> collect prj'
    //        |> takeWhile tkW
    //        |> fold (+) s
    //    cull ()
    //    printfn "sq_tofSeq_big = %i" t.ElapsedMilliseconds
    //    t.Reset()

    //    cull ()
    //    t.Restart()
    //    let sq_Seq_big =
    //        xss
    //        |> Seq.map string
    //        |> Seq.filter ftrue 
    //        |> Seq.map int64
    //        |> Seq.filter ftrue
    //        |> Seq.map add
    //        |> Seq.filter ftrue
    //        |> Seq.map sub
    //        |> Seq.collect prj
    //        |> Seq.takeWhile tkW
    //        |> Seq.fold (+) s
    //    cull ()
    //    printfn "sq_tSeq_big = %i" t.ElapsedMilliseconds
    //    t.Reset()

    //    cull ()
    //    t.Restart()
    //    let sq_linq_big =
    //        xss
    //         .Select(fun a -> string a)
    //         .Where(fun a -> ftrue a) 
    //         .Select(fun a -> int64 a)
    //         .Where(fun a -> ftrue a)
    //         .Select(fun a -> add a)
    //         .Where(fun a -> ftrue a)
    //         .Select(fun a -> sub a)
    //         .SelectMany(fun a -> prj a)
    //         .TakeWhile(fun a -> tkW a)
    //         .Aggregate(s, fun a b -> a + b)
    //    cull ()
    //    printfn "sq_tlinq_big = %i" t.ElapsedMilliseconds
    //    t.Reset()

    //    let ONE = 1
    //    let END = 10000

    //    cull ()
    //    t.Restart()
    //    let ofArray_small =
    //        for i = ONE to END do
    //            ys
    //            |> ofArray
    //            |> map string
    //            |> filter ftrue 
    //            |> map int64
    //            |> filter ftrue
    //            |> map add
    //            |> filter ftrue
    //            |> map sub
    //            |> collect prj'
    //            |> takeWhile tkW
    //            |> fold (+) s
    //            |> ignore
    //    cull ()
    //    printfn "tofArray_small = %i" t.ElapsedMilliseconds
    //    t.Reset()

    //    cull ()
    //    t.Restart()
    //    let ofSeq_small =
    //        for i = ONE to END do
    //            ys
    //            |> ofSeq
    //            |> map string
    //            |> filter ftrue 
    //            |> map int64
    //            |> filter ftrue
    //            |> map add
    //            |> filter ftrue
    //            |> map sub
    //            |> collect prj'
    //            |> takeWhile tkW
    //            |> fold (+) s
    //            |> ignore
    //    cull ()
    //    printfn "tofSeq_small = %i" t.ElapsedMilliseconds
    //    t.Reset()

    //    cull ()
    //    t.Restart()
    //    let Seq_small =
    //        for i = ONE to END do
    //            ys
    //            |> Seq.map string
    //            |> Seq.filter ftrue 
    //            |> Seq.map int64
    //            |> Seq.filter ftrue
    //            |> Seq.map add
    //            |> Seq.filter ftrue
    //            |> Seq.map sub
    //            |> Seq.collect prj
    //            |> Seq.takeWhile tkW
    //            |> Seq.fold (+) s
    //            |> ignore
    //    cull ()
    //    printfn "tSeq_small = %i" t.ElapsedMilliseconds
    //    t.Reset()

    //    cull ()
    //    t.Restart()
    //    let linq_small =
    //        for i = ONE to END do
    //            ys
    //             .Select(fun a -> string a)
    //             .Where(fun a -> ftrue a) 
    //             .Select(fun a -> int64 a)
    //             .Where(fun a -> ftrue a)
    //             .Select(fun a -> add a)
    //             .Where(fun a -> ftrue a)
    //             .Select(fun a -> sub a)
    //             .SelectMany(fun a -> prj a)
    //             .TakeWhile(fun a -> tkW a)
    //             .Aggregate(s, fun a b -> a + b)
    //             |> ignore
    //    cull ()
    //    printfn "tlinq_small = %i" t.ElapsedMilliseconds
    //    t.Reset()

    //    printfn "done..."
    //    }
    //|> Async.StartImmediate