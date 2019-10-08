namespace PTR.Context.Type//.Incomplete.Stream_


/////
//[<Struct; NoComparison; NoEquality>]
//type Stream_<'T> = Stream of ((^T -> unit) -> unit)


/////
//module Stream_ =

//    ///
//    let inline ofArray (source: ^a []) : Stream_< ^a> =
//        Stream (fun k -> Array.iter k source)

//    ///
//    let inline ofList (source: ^a list) : Stream_< ^a> =
//        Stream (fun k -> List.iter k source)

//    ///
//    let inline ofSeq (source: ^a seq) : Stream_< ^a> =
//        Stream (fun k ->
//            match source with
//            | :? array<_> as xs -> Array.iter k xs
//            | :? list<_> as xs -> List.iter k xs
//            | :? ResizeArray<_> as xs -> for i = 0 to xs.Count - 1 do k xs.[i]
//            | _ -> Seq.iter k source)

//    ///
//    let empty<'a> : Stream_< ^a> = Stream (fun _ -> ())

//    ///
//    let inline cons x (Stream xs) = Stream (fun k -> k x ; xs k)

//    ///
//    let inline snoc x (Stream xs) = Stream (fun k -> xs k ; k x)

//    ///
//    let inline append (Stream s1) (Stream s2) = Stream (fun k -> s1 k ; s2 k)

//    ///
//    let inline fold folder (seed: ^s) (Stream st) =
//        let mutable s = seed in st (fun a -> s <- folder s a)
//        s

//    ///
//    let inline iter action (Stream st) = st action

//    ///
//    let inline filter predicate (Stream st) =
//        Stream (fun k -> st (fun a -> if predicate a then k a else ()))

//    ///
//    let inline choose chooser (Stream st) =
//        Stream (fun k -> st (fun a -> match chooser a with None -> () | Some b -> k b))

//    ///
//    let inline take n (Stream st) =
//        Stream (fun k ->
//            let mutable i = 1 in st (fun a -> if i <= n then let _ = k a in i <- i + 1 else ()))

//    ///
//    let inline takeWhile predicate (Stream st) =
//        Stream (fun k ->
//            let mutable g = true
//            st (fun a -> if g then
//                             if predicate a then k a
//                             else g <- false))    

//    ///
//    let inline skip n (Stream st) =
//        Stream (fun k ->
//            let mutable i = 1 in st (fun a -> if i <= n then i <- i + 1 else k a))

//    ///
//    let inline skipWhile predicate (Stream st) =
//        Stream (fun k ->
//            let mutable g = true in st (fun a -> if g then g <- predicate a else k a))

//    ///
//    let inline mapStream f (Stream st) = Stream (fun k -> st (fun a -> k (f a)))

//    ///
//    let inline withStream f (Stream st) = Stream (fun k -> st (f k))

//    ///
//    let exit<'a> : Stream_< ^a> = Stream (fun _ -> ())

//    ///
//    let cacheStream (Stream  st) =
//        let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
//        Stream (fun k ->
//            st (fun a -> match d.TryGetValue(a) with
//                         | true, u -> u
//                         | false, _ -> d.[a] <- k a))


//    ///
//    module Compose =

//        ///
//        module Monad =

//            ///
//            let wrap (x: 'a) : Stream_< ^a> = Stream (fun k -> k x)
        
//            ///
//            let inline bind k (Stream st) : Stream_< ^b> =
//                Stream (fun c -> st (fun a -> match k a with Stream st2 -> st2 c))

//            ///
//            let inline flatten (Stream mm) =
//                Stream (fun k -> mm (fun (Stream a) -> a k))


//            /// Monadic computation builder specialised to the given monad.
//            type StreamBuilder () =
//                member inline _.Bind(m, k) = bind k m
//                member inline _.Return x = wrap x
//                member inline _.ReturnFrom m : Stream_< ^a> = m
//                member inline s.Zero() = unit ()
 
//                member inline _.Delay f = f ()
//                member inline _.Run f = f
 
//                member inline _.TryWith(body, handler) = try body () with e -> handler e
//                member inline _.TryFinally(body, finalizer) = try body () finally finalizer ()
 
//                member inline _.Using(disp: #System.IDisposable, body) =
//                    s.TryFinally((fun () -> body disp), fun () -> match box disp with null -> () | _ -> disp.Dispose ())
 
//                member inline _.While(guard, body) =
//                    let rec loop = function
//                    | false -> wrap ()
//                    | true  -> bind (fun () -> loop (guard ())) (body ())
//                    loop (guard ())
 
//                member inline s.For(seq: _ seq, body) =
//                    s.Using(seq.GetEnumerator(), fun enum -> s.While(enum.MoveNext, fun () -> body enum.Current))


//            ///
//            module Plus =

//                ///
//                let mzero<'a> : Stream_< ^a> = Stream (fun _ -> ())

//                ///
//                let inline mplus (Stream s1) (Stream s2) =
//                    Stream (fun k -> s1 k ; s2 k)

//                ///
//                let inline guard condition : Stream_<unit> =
//                    if condition then Stream (fun k -> k ()) else Stream (fun _ -> ())

//                ///
//                let inline recover makeNew (Stream st) =
//                    Stream (fun k ->
//                        let mutable i = 0
//                        st (fun a -> i <- i + 1 ; k a)
//                        if i = 0 then match makeNew () with Stream st -> st k)

//                ///
//                let inline relate f (k1: ^a -> ^k) (k2: ^b -> ^k) (Stream s1) (Stream s2) : Stream_< ^c> =
//                    Stream (fun k ->
//                        s1 (fun x ->
//                        s2 (fun y ->
//                            if k1 x = k2 y then k (f x y))))


//        ///
//        module Applicative =

//            ///
//            let wrap (x: 'a) : Stream_< ^a> = Stream (fun k -> k x)

//            ///
//            let inline ap (Stream fv) (Stream ff) =
//                Stream (fun kb -> ff (fun f -> fv (fun v -> kb (f v))))

//            ///
//            let inline map2 f (Stream s1) (Stream s2) =
//                Stream (fun k -> s1 (fun a -> s2 (fun b -> k (f a b))))

//            ///
//            let inline map3 f (Stream s1) (Stream s2) (Stream s3) =
//                Stream (fun k -> s1 (fun a -> s2 (fun b -> s3 (fun c -> k (f a b c)))))

//            /// Sequentially compose two effects, discarding any value produced by the first.
//            let inline andThen (Stream fb) (Stream fa) : Stream_< ^b> =
//                Stream (fun k -> fa (fun _ -> fb (fun b -> k b)))

//            /// Conditional execution of effectful expressions.
//            let inline when_ condition f = if condition then f () else wrap ()

//            /// <summary>Generalizes the sequence-based filter function.</summary>
//            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//            let inline filterA p source =
//                let cons x b xs = if b then x::xs else xs
//                let f x xs = map2 (cons x) (p x) xs
//                Seq.foldBack f source (wrap [])

//            /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
//            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//            let inline sequenceA source =                
//                let cons x xs = x::xs
//                let f x xs = map2 cons x xs
//                Seq.foldBack f source (wrap [])

//            /// <summary>Produce an effect for the elements in the sequence from left to right then evaluate each effect, and collect the results.</summary>
//            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//            let inline forA f source = sequenceA (System.Linq.Enumerable.Select(source, System.Func<_,_>f))

//            /// <summary>Produce an effect for each pair of elements in the sequences from left to right, then evaluate each effect and collect the results.
//            /// If one sequence is longer, its extra elements are ignored.</summary>
//            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//            let inline zipWithA f source1 source2 =
//                sequenceA (System.Linq.Enumerable.Zip(source1, source2, System.Func<_,_,_>f))

//            /// Performs the effect 'n' times.
//            let replicateA count fa =
//                sequenceA (Seq.replicate (max 0 count) fa)


//            /// A monoid on applicative functors.
//            module Alternative =

//                /// The identity of orElse.
//                let empty<'a> : ^a Stream_ = Monad.Plus.mzero

//                /// An associative binary operation on applicative functors.
//                let inline orElse choice2 choice1 = Monad.Plus.mplus choice1 choice2

//                /// <summary>The sum of a collection of effects.</summary>
//                /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//                let inline asum t_fa =
//                    Seq.foldBack Monad.Plus.mplus t_fa Monad.Plus.mzero

//                /// Return one or none results on effects.
//                let inline optional fa = orElse (wrap None) (mapStream Some fa)

//                /// Create a new item if the previous was empty, else keep the original.
//                let inline alt def (Stream st) =
//                    Stream (fun k ->
//                        let mutable i = 0
//                        st (fun a -> i <- i + 1 ; k a)
//                        if i = 0 then match def () with Stream st -> st k)





//        ///
//        module Functor =

//            ///
//            let inline map f (Stream st) =
//                Stream (fun k -> st (fun a -> k (f a)))

//            ///
//            let inline replace (b: ^b) (Stream st) = Stream (fun k -> st (fun _ -> k b))

//            ///
//            let inline tee f g (Stream st) =
//                Stream (fun k -> st (fun a -> let b = f a in g a b ; k b))


//        ///
//        module Semigroup =

//            ///
//            let inline sappend e1 e2 =
//                Applicative.map2 (fun a b -> (^a: (static member Append: ^a -> ^a -> ^a) (a, b))) e1 e2


//        ///
//        module Monoid =

//            ///
//            let mempty<'a> : Stream_< ^a> = Stream (fun _ -> ())
        
//            ///
//            let inline mappend e1 e2 = Semigroup.sappend e1 e2





//    ///
//    let stream = Compose.Monad.StreamBuilder ()

//    module T =

//        type Sum = S of string
//        with static member inline Append ((S a), S b) = S (a + b)


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
//        let r = ref 0L
//        let y1 = Stream (fun k -> if !r <= 3L then
//                                    r := !r + 1L
//                                    k !r)


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



//open Stream_
//open Compose

//// @ Operators @
//type Stream_<'T> with

//// @ Primitive @

//    ///
//    static member inline ( >- ) (s, a) = iter a s
//    ///
//    static member inline ( -< ) (a, s) = iter a s

//    ///
//    static member inline ( >- ) (s, (f, z)) = fold f z s
//    ///
//    static member inline ( -< ) ((f, z), s) = fold f z s

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

//// @ Applicative.Alternative @

//    /// An associative binary operation on applicative functors.
//    static member inline ( <|> ) (c1, c2) = Applicative.Alternative.orElse c2 c1
//    /// An associative binary operation on applicative functors.
//    static member inline ( <||> ) (c2, c1) = Applicative.Alternative.orElse c2 c1

//// @ Functor @

//    /// Lift a function onto effects.
//    static member inline ( |%> ) (fa, f) = Functor.map f fa
//    /// Lift a function onto effects.
//    static member inline ( <%| ) (f, fa) = Functor.map f fa

//    /// Replace all locations in the input with the same value.
//    static member inline ( %> ) (fa, b) = Functor.replace b fa
//    /// Replace all locations in the input with the same value.
//    static member inline ( <% ) (b, fa) = Functor.replace b fa

//// @ Semigroup @

//    /// An associative composition operation.
//    static member inline Append (e1, e2) = Semigroup.sappend e1 e2

//// @ Monoid @

//    /// The identity element for the composition operator.
//    static member inline Empty () : ^a Stream_ = Monoid.mempty