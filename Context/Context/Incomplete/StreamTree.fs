namespace Ptr.Context.Incomplete.Type.StreamTree


///// Combines the notions of nullability, mutability, (streaming) non-determinism, and (binary) choice.
//[<Struct; NoComparison; NoEquality>]
//type StreamTree<'state, 'left, 'right, 'value> =
//    | TNil
//    | TSome  of Value : ^value
//    | TLeft  of Left  : (^state -> struct (^state * ^left  * StreamTree< ^state, ^left, ^right, ^value>))
//    | TRight of Right : (^state -> struct (^state * ^right * StreamTree< ^state, ^left, ^right, ^value>))


///// Standard operations on `StreamTree` values.
//module Std =

//    /// Generate a new stream that, when enumerated, will create new 'left'-value(s).
//    let inline initLeft (n: uint32) f : StreamTree< ^s, ^l, ^r, unit> =
//        let rec go = function
//        | m when m >= n -> TSome ()
//        | n -> TLeft (fun s -> struct (s, f n, go (n + 1u)))
//        go n

//    /// Generate a new stream that, when enumerated, will create ininite new 'left'-value(s).
//    let inline initInfLeft f (s0: ^z) : StreamTree< ^s, ^l, ^r, unit> =
//        let rec go z = TLeft (fun s -> match f z s with struct (s, l, z) -> struct (s, l, go z))
//        go s0    

//    /// Enumerate all 'left'-values into a sequence.
//    let inline streamLefts s0 (t: StreamTree< ^s, ^l, ^r, ^a>) =
//        let rec go s m = seq {
//            match m with
//            | TNil -> ()
//            | TSome _ -> ()
//            | TLeft f -> match f s with struct (s, l, t) -> yield l ; yield! go s t
//            | TRight f -> match f s with struct (s, _, t) -> yield! go s t }
//        go s0 t

//    /// Generate a new stream that, when enumerated, will create new 'right'-value(s).
//    let inline initRight (n: uint32) f : StreamTree< ^s, ^l, ^r, unit> =
//        let rec go = function
//        | m when m >= n -> TSome ()
//        | n -> TRight (fun s -> struct (s, f n, go (n + 1u)))
//        go 0u    

//    /// Generate a new stream that, when enumerated, will create ininite new 'right'-value(s).
//    let inline initInfRight f (s: ^z) : StreamTree< ^s, ^l, ^r, unit> =
//        let rec go z = TRight (fun s -> match f z s with struct (s, r, z) -> struct (s, r, go z))
//        go s

//    /// Enumerate all 'right'-values into a sequence.
//    let inline streamRights s0 (t: StreamTree< ^s, ^l, ^r, ^a>) =
//        let rec go s m = seq {
//            match m with
//            | TNil -> ()
//            | TSome _ -> ()
//            | TLeft f -> match f s with struct (s, _, t) -> yield! go s t
//            | TRight f -> match f s with struct (s, r, t) -> yield r ; yield! go s t }
//        go s0 t 

//    /// Generate a new stream that, when enumerated, will create either a 'left' or 'right' value at each point.
//    let inline initChoice (n: uint32) choice f g : StreamTree< ^``l*``, ^``r*``, ^s, unit> =
//        let rec go = function
//        | m when m >= n -> TSome ()
//        | n -> match choice n with
//               | Choice1Of2 (a: ^l) -> TLeft  (fun s -> struct (s, f a, go (n + 1u)))
//               | Choice2Of2 (b: ^r) -> TRight (fun s -> struct (s, g b, go (n + 1u)))
//        go 0u

//    /// Generate a new stream that, when enumerated, will create ininite new 'left' or 'right' value(s) at each point..
//    let inline initInfChoice f (s: ^s) : StreamTree< ^s, ^l, ^r, unit> =
//        let rec go z =
//            match f z with
//            | Choice1Of2 l -> TLeft  (fun s -> struct (s, l, go s))
//            | Choice2Of2 r -> TRight (fun s -> struct (s, r, go s))
//        go s

//    /// Enumerate all 'left' and 'right'-values into as a sequence of 'Choices'.
//    let inline streamChoice s0 (t: StreamTree< ^s, ^l, ^r, ^a>) =
//        let rec go s t = seq {
//            match t with
//            | TNil -> ()
//            | TSome _ -> ()
//            | TLeft f  -> match f s with struct (s, l, t) -> yield Choice1Of2 l ; yield! go s t
//            | TRight f -> match f s with struct (s, r, t) -> yield Choice2Of2 r ; yield! go s t }
//        go s0 t


///// Compositional operations on `StreamTree` values.
//module Compose =

//    /// Lift a value onto an effectful context.
//    let inline wrap x : StreamTree< ^s, ^l, ^r, ^a> = TSome x

//    /// Sequentially compose two effects, passing any value produced by the first
//    /// as an argument to the second.
//    let inline bind k (m: StreamTree< ^s, ^l, ^r, ^a>) : StreamTree< ^s, ^l, ^r, ^b> =
//        let rec go = function
//        | TNil    -> TNil
//        | TSome a -> k a
//        | TLeft f -> TLeft (fun s ->
//            match f s with
//            | struct (s, l, t) -> struct (s, l, go t))
//        | TRight f -> TRight (fun s ->
//            match f s with
//            | struct (s, r, t) -> struct (s, r, go t))
//        go m

//    /// Removes one layer of monadic context from a nested monad.
//    let inline flatten mm : StreamTree< ^s, ^l, ^r, ^a> = bind id mm

//    /// Sequential application on effects.
//    let inline ap mv mf : StreamTree< ^s, ^l, ^r, ^b> =
//        let rec go = function
//        | TNil     -> TNil
//        | TSome f  -> mk f mv
//        | TLeft l  -> TLeft (fun s -> match l s with struct (s, l, t) -> struct (s, l, go t))
//        | TRight r -> TRight (fun s -> match r s with struct (s, r, t) -> struct (s, r, go t))
//        and mk f = function
//        | TNil     -> TNil
//        | TSome v  -> TSome (f v)
//        | TLeft l  -> TLeft (fun s -> match l s with struct (s, l, t) -> struct (s, l, mk f t))
//        | TRight r -> TRight (fun s -> match r s with struct (s, r, t) -> struct (s, r, mk f t))
//        go mf

//    /// Lift a function onto effects.
//    let inline map f m : StreamTree< ^s, ^l, ^r, ^b> =
//        let rec go = function
//        | TNil -> TNil
//        | TSome a -> TSome (f a)
//        | TLeft f -> TLeft (fun s ->
//            match f s with
//            | struct (s, l, t) -> struct (s, l, go t))
//        | TRight f -> TRight (fun s ->
//            match f s with
//            | struct (s, r, t) -> struct (s, r, go t))
//        go m


//    /// Supplementary Monad operations on the given type.
//    module Monad =

//        /// Monadic computation builder specialised to the given monad.
//        type StreamTreeBuilder () =
//            member inline s.Bind(m, k) = bind k m
//            member inline s.Return x = wrap x
//            member inline s.ReturnFrom m : StreamTree< ^s, ^l, ^r, ^a> = m
//            member inline s.Zero () = s.Return ()
  
//            member inline s.Delay f = f ()
//            member inline s.Run f = f
  
//            member inline s.TryWith (body, handler) = try s.ReturnFrom(body ()) with e -> handler e
//            member inline s.TryFinally (body, finalizer) = try s.ReturnFrom(body ()) finally finalizer ()
  
//            member inline s.Using(disp: #System.IDisposable, body) =
//                s.TryFinally((fun () -> body disp),
//                    fun () -> match box disp with null -> () | _ -> disp.Dispose ())
  
//            member inline s.While(guard, body) =
//                let rec loop = function
//                | false -> s.Zero ()
//                | true -> s.Bind(body (), guard >> loop)
//                loop (guard ())
  
//            member inline s.For(seq: _ seq, body) =
//                s.Using(seq.GetEnumerator(),
//                    fun enum -> s.While(enum.MoveNext,
//                                    s.Delay(fun () -> body enum.Current)))


//    /// Creates a computation expression for the given type.
//    let streamtree = Monad.StreamTreeBuilder ()



//    ///
//    module Quadfunctor =

//        //
//        //let inline mapLRTree fLeft fRight fState fValue
//        //        (t: StreamTree< ^s, ^l, ^r, ^a>) : StreamTree< ^``l*``, ^``r*``, ^s, ^b> =
//            //let rec go = function
//            //| TNil -> TNil
//            //| TSome a -> TSome (fValue a)
//            //| TLeft l  -> TLeft  (fun s -> match l s with struct (s, l, t) -> struct (fState s, fLeft l, go t))
//            //| TRight r -> TRight (fun s -> match r s with struct (s, r, t) -> struct (fState s, fRight r, go t))
//            //go t
//        //
//        // Map a function over any 'right' values present in the stream.
//        //let inline mapRights f (t: StreamTree< ^l, ^r, ^s, ^a>) : StreamTree< ^l, ^``r*``, ^s, ^a> =
//            //let rec go = function
//            //| TNil -> TNil
//            //| TSome a -> TSome a
//            //| TLeft l  -> TLeft  (fun s -> match l s with struct (s, l, t) -> struct (s, l, go t))
//            //| TRight r -> TRight (fun s -> match r s with struct (s, r, t) -> struct (s, f r, go t))
//            //go t
//        //
//        // Map a function over any 'left' values present in the stream.
//        //let inline mapLefts f (t: StreamTree< ^s, ^l, ^r, ^a>) : StreamTree< ^``l*``, ^r, ^s, ^a> =
//        //    let rec go = function
//        //    | TNil -> TNil
//        //    | TSome a -> TSome a
//        //    | TLeft l  -> TLeft  (fun s -> match l s with struct (s, l, t) -> struct (s, f l, go t))
//        //    | TRight r -> TRight (fun s -> match r s with struct (s, r, t) -> struct (s, r, go t))
//        //    go t

//        ()