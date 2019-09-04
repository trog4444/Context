namespace PTR.Context.Type.Incomplete

open PTR.Context.Type


///
type Stream<'T> = Cont<unit, ^T>


///
//module Stream =

//    let inline internal run k (stream: ^a Stream) = Cont.runCont k stream

//    ///
//    let inline toStream (cont: Cont<unit, ^a>) : ^a Stream = cont

//    ///
//    let inline ofArray (source: ^a []) : Stream< ^a> =
//        Cont (fun k -> Array.iter k source)

//    ///
//    let inline ofList (source: ^a list) : Stream< ^a> =
//        Cont (fun k -> List.iter k source)

//    ///
//    let inline ofSeq (source: ^a seq) : Stream< ^a> =
//        Cont (fun k ->
//            match source with
//            | :? array<_> as xs -> Array.iter k xs
//            | :? list<_>  as xs -> List.iter k xs
//            | :? ResizeArray<_> as xs -> for i = 0 to xs.Count - 1 do k xs.[i]
//            | _ -> Seq.iter k source)

//    ///
//    let empty<'a> : Stream< ^a> = Cont (fun _ -> ())

//    let singleton (x: 'a) : Stream< ^a> = Cont (fun k -> k x)

//    ///
//    let inline cons x (xs: ^a Stream) : ^a Stream = Cont (fun k -> k x ; run k xs)

//    ///
//    let inline snoc x (xs: ^a Stream) : ^a Stream = Cont (fun k -> run k xs ; k x)

//    ///
//    let inline append (xs: ^a Stream) (ys: ^a Stream) : ^a Stream =
//        Cont (fun k -> run k xs ; run k ys)

//    ///
//    let inline fold folder (seed: ^s) (stream: ^a Stream) =
//        let mutable s = seed in run (fun a -> s <- folder s a) stream
//        s

//    ///
//    let inline iter action (stream: ^a Stream) = run action stream

//    ///
//    let inline filter predicate (stream: ^a Stream) : ^a Stream =
//        Cont (fun k -> run (fun a -> if predicate a then k a else ()) stream)

//    ///
//    let inline choose chooser (stream: ^a Stream) : ^b Stream =
//        Cont (fun k -> run (fun a -> match chooser a with Nothing -> () | Just b -> k b) stream)

//    ///
//    let inline take n (stream: ^a Stream) : ^a Stream =
//        Cont (fun k ->
//            let n = max 0 n
//            let mutable i = 1
//            run (fun a -> if i <= n then let _ = k a in i <- i + 1 else ()) stream)

//    ///
//    let inline takeWhile predicate (stream: ^a Stream) : ^a Stream =
//        Cont (fun k ->
//            let mutable g = true
//            run (fun a -> if g then
//                             if predicate a then k a
//                             else g <- false) stream)

//    ///
//    let inline skip n (stream: ^a Stream) : ^a Stream =
//        Cont (fun k ->
//            let n = max 0 n
//            let mutable i = 1
//            run (fun a -> if i <= n then i <- i + 1 else k a) stream)

//    ///
//    let inline skipWhile predicate (stream: ^a Stream) : ^a Stream =
//        Cont (fun k ->
//            let mutable g = true in run (fun a -> if g then g <- predicate a else k a) stream)

//    ///
//    let inline mapStream f (stream: ^a Stream) : ^b Stream =
//        Cont (fun k -> run (fun a -> k (f a)) stream)

//    ///
//    let inline withStream f (stream: ^a Stream) : ^b Stream = Cont (fun k -> run (f k) stream)

//    ///
//    let exit<'a> : Stream< ^a> = Cont (fun _ -> ())

//    ///
//    let cacheStream (stream: 'a Stream) : ^a Stream = Cont.cacheCont stream


//    ///
//    module Compose =

//        ///
//        module Monad =

//            /// Monadic computation builder specialised to the given monad.
//            type StreamBuilder () =
//                member inline s.Bind((m: ^a Stream), (k: ^a -> ^b Stream)) : ^b Stream = Cont.Compose.Monad.bind k m
//                member inline s.Return x = singleton x
//                member inline s.ReturnFrom m : Stream< ^a> = m
//                member inline s.Zero () = s.Return ()
 
//                member inline s.Delay f = f ()
//                member inline s.Run f = f
 
//                member inline s.TryWith (body, handler) = try s.ReturnFrom(body ()) with e -> handler e
//                member inline s.TryFinally (body, finalizer) = try s.ReturnFrom(body ()) finally finalizer ()
 
//                member inline s.Using(disp: #System.IDisposable, body) =
//                    s.TryFinally((fun () -> body disp), fun () -> match box disp with null -> () | _ -> disp.Dispose ())
 
//                member inline s.While(guard, body) =
//                    let rec loop = function
//                    | false -> s.Zero ()
//                    | true -> s.Bind(body (), fun x -> loop (guard x))
//                    loop (guard ())
 
//                member inline s.For(seq: _ seq, body) =
//                    s.Using(seq.GetEnumerator(), fun enum -> s.While(enum.MoveNext, s.Delay(fun () -> body enum.Current)))


//            ///
//            module Plus =

//                ///
//                let mzero<'a> : Stream< ^a> = Cont (fun _ -> ())

//                ///
//                let inline mplus (m1: ^a Stream) (m2: ^a Stream) : ^a Stream =
//                    Cont (fun k -> run k m1 ; run k m2)

//                ///
//                let inline guard condition : Stream<unit> =
//                    if condition then Cont (fun k -> k ()) else Cont (fun _ -> ())

//                ///
//                let inline recover (makeNew: unit -> ^a Stream) (m: ^a Stream) : ^a Stream =
//                    Cont (fun k ->
//                        let mutable i = 0 in run (fun a -> i <- i + 1 ; k a) m
//                        if i = 0 then match makeNew () with Cont st -> st k)

//                ///
//                let inline relate f (k1: ^a -> ^k) (k2: ^b -> ^k) (m1: ^a Stream) (m2: ^b Stream)
//                    : Stream< ^c> =
//                    Cont (fun k -> run (fun x -> run (fun y ->
//                        if k1 x = k2 y then k (f x y)) m2) m1)


//        ///
//        module Applicative =

//            /// A monoid on applicative functors.
//            module Alternative =

//                /// The identity of orElse.
//                let empty<'a> : ^a Stream = Monad.Plus.mzero

//                /// An associative binary operation on applicative functors.
//                let inline orElse choice2 choice1 = Monad.Plus.mplus choice1 choice2

//                /// <summary>The sum of a collection of effects.</summary>
//                /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//                let inline asum t_fa =
//                    Seq.foldBack Monad.Plus.mplus t_fa Monad.Plus.mzero

//                /// Return one or none results on effects.
//                let inline optional fa = orElse (singleton None) (mapStream Some fa)

//                /// Create a new item if the previous was empty, else keep the original.
//                let inline alt def (stream: ^a Stream) : ^a Stream =
//                    Monad.Plus.recover def stream


//        ///
//        module Semigroup =

//            ///
//            let inline sappend (e1: ^a Stream) (e2: ^a Stream) : Stream< ^a> =
//                Cont.Compose.Applicative.map2 (fun a b ->
//                    (^a: (static member Append: ^a -> ^a -> ^a) (a, b))) e1 e2


//        ///
//        module Monoid =

//            ///
//            let mempty<'a> : Stream< ^a> = Cont (fun _ -> ())
        
//            ///
//            let inline mappend e1 e2 = Semigroup.sappend e1 e2


//    ///
//    let stream = Compose.Monad.StreamBuilder ()


//    module T =

//        type Sum = S of string
//        with static member inline Append ((S a), S b) = S (a + b)
//             static member Empty() = S ""


//    open Compose
//    open System.Linq
//    open Monad
//    open Plus
//    async {
//        printfn "initializing..."

//        let lim = 1
//        let t = System.Diagnostics.Stopwatch()
//        let xs = [|1L..5L|]
//        let cull wait =
//            let delay = 250 in if wait then System.Threading.Thread.Sleep delay
//            System.GC.Collect()
//            System.GC.GetTotalMemory true |> ignore
//        printfn "starting..."
//        cull true

//        let x1 = ofArray xs
//        let x2 = ofArray xs


//        t.Restart()
//        let a = //Monad.Plus.mplus x1 Monad.Plus.mzero
//            stream {
//                let! a = ofList [1..100]
//                let! b = ofList [1..100]
//                let! c = ofList [1..100]
//                do! guard (float a ** 2. + float b ** 2. = float c ** 2.)
//                return a, b, c }            
//            //Applicative.Alternative.alt (fun () -> ofArray [|1L..10L|]) x1
//            //takeWhile (fun a -> a < 4) <| ofArray [|1..20|]
//            //empty
//            //|> cons 1
//            //|> cons 2
//            //|> cons 3
//            //ofArray xs
//            //|> skip 2
//            //|> filter (fun x -> x % 2L = 1L)
//            //|> bind (fun x -> ofArray [|1L..x|])
//            //|> bind (fun x -> ofArray [| for i = 1L to x do yield string i |])
//            //relate (fun a b -> a, b) (fun a -> a % 2L = 1L) (fun b -> b % 2L = 0L) x1 x2
//            //|> filter (fun x -> int64 x % 2L = 0L)
//        iter (printfn "%A") a
//        cull false
//        printfn "%i" t.ElapsedMilliseconds

//        cull true

//        let b =            
//            seq {
//                for a in [1..100] do
//                for b in [1..100] do
//                for c in [1..100] do
//                if (float a ** 2. + float b ** 2. = float c ** 2.)
//                then yield a, b, c }
//        Seq.iter (printfn "%A") b
//        cull false
//        printfn "%i" t.ElapsedMilliseconds


//        //let b =
//        //    empty
//        //    |> snoc 1
//        //    |> snoc 2
//        //    |> snoc 3
//        //    //Applicative.Alternative.alt (fun () -> ofArray [|1..10|]) (ofArray [||])
//        //iter (printfn "%i") b


//        (*        
//        t.Restart()
//        let r_Stream = for i = 1 to lim do ignore (ofList xs |> map (fun x -> id (id (x))) |> fold (+) 0L)
//        cull false
//        printfn "t_Stream = %i" t.ElapsedMilliseconds
//        t.Reset()

//        cull true

//        t.Restart()
//        let r_Native = for i = 1 to lim do ignore (Array.map (fun x -> id (id (x))) xs |> Array.sum)
//        cull false
//        printfn "t_Native = %i" t.ElapsedMilliseconds
//        t.Reset()

//        cull true

//        t.Restart()
//        let r_Seq = for i = 1 to lim do ignore (Seq.map (fun x -> id (id (x))) xs |> Seq.sum)
//        cull false
//        printfn "t_Seq = %i" t.ElapsedMilliseconds
//        t.Reset()

//        cull true        

//        t.Restart()
//        let r_Linq = for i = 1 to lim do ignore (xs.Select(fun x -> id (id x)).Sum())
//        cull false
//        printfn "t_Linq = %i" t.ElapsedMilliseconds
//        *)

//        printfn "...done"
//    } |> Async.Start