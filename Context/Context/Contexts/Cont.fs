namespace PTR.Context.Type


/// A CPS ("continuation-passing style") computation that produces an intermediate
/// result of type `a` within a CPS computation whose final result type is `r`.
[<Struct; NoComparison; NoEquality>]
type Cont<'R, 'T> = Cont of ((^T -> ^R) -> ^R)


/// Operations on `Cont` values.
module Cont =

    /// The result of running a CPS computation with a given final continuation.
    let runCont (Cont (c: ('a -> 'r) -> ^r)) (k: 'a -> 'r) : ^r = c k

    /// The result of running a CPS computation with the identity as the final continuation.
    let evalCont (Cont (c: ('r -> ^r) -> ^r)) : ^r = c id

    /// Apply a function to transform the result of a continuation-passing computation.
    let inline mapCont f (Cont c) : Cont< ^r, ^a> = Cont (fun k -> f (c k))

    /// Apply a function to transform the continuation passed to a CPS computation.
    let inline withCont f (Cont c) : Cont< ^r, ^b> = Cont (fun k -> c (f k))

    /// End the current continuation chain with a specific value.
    let exit (x: 'r) : Cont< ^r, '``_``> = Cont (fun _ -> x)
 
    /// shift 'f' captures the continuation up to the nearest enclosing 'reset' and passes it to 'f'.
    let inline shift f : Cont< ^r, ^a> = Cont (fun k -> match f k with Cont c -> c id)

    /// reset 'm' delimits the continuation of any shift inside 'm'.
    let reset (Cont (c: ('r -> ^r) -> ^r)) : Cont<'r0, ^r> = Cont (fun k -> k (c id))

    /// Call with current continuation.
    let inline callCC f : Cont< ^r, ^a> =
        Cont (fun k -> match f (fun x -> Cont (fun _ -> k x)) with Cont c -> c k)

    /// Allows looping with a given continuation function and input.
    let inline getCC x0 = callCC (fun c -> let rec f x = c (x, f) in Cont (fun k -> k (x0, f)))

    /// Attempt to apply `fOk` on an input and return a continuation. If the function fails,
    /// return a continuation with the result of applying `fErr` to the input and exception.
    let inline tryCC fOk fErr input =
        callCC (fun ok -> callCC (fun er -> try ok (fOk input) with e -> er (fErr input e)))

    /// Caches the result(s) of a `Cont` computation.
    let cacheCont (Cont (c: ('a -> 'r) -> ^r)) : Cont< ^r, ^a> =
        let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        Cont (fun k -> c (fun a ->
            match d.TryGetValue(a) with
            | true, r  -> r
            | false, _ -> let r = k a in d.[a] <- r ; r))


    /// Compositional operations on `Cont` values.
    module Compose =

        /// Supplementary Monad operations on the given type.
        module Monad =

            /// Lift a value onto an effectful context.
            let wrap (x: 'a) : Cont<'r, ^a> = Cont (fun k -> k x)

            /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
            let inline bind (k: ^a -> Cont< ^r, ^b>) (Cont c) =
                Cont (fun k' -> c (fun a -> match k a with Cont c' -> c' k'))

            /// Removes one layer of monadic context from a nested monad.
            let inline flatten (Cont cc) : Cont< ^r, ^a> =
                Cont (fun k -> cc (fun (Cont c) -> c k))


            /// Monadic computation builder specialised to the given monad.
            type ContBuilder () =
                member inline s.Bind(m, k) = bind k m
                member inline s.Return x = wrap x
                member inline s.ReturnFrom m : Cont< ^r, ^a> = m
                member inline s.Zero () = s.Return ()
 
                member inline s.Delay f = f ()
                member inline s.Run f = f
 
                member inline s.TryWith (body, handler) = try s.ReturnFrom(body ()) with e -> handler e
                member inline s.TryFinally (body, finalizer) = try s.ReturnFrom(body ()) finally finalizer ()
 
                member inline s.Using(disp: #System.IDisposable, body) =
                    s.TryFinally((fun () -> body disp), fun () -> match box disp with null -> () | _ -> disp.Dispose ())
 
                member inline s.While(guard, body) =
                    let rec loop = function
                    | false -> s.Zero ()
                    | true -> s.Bind(body (), fun x -> loop (guard x))
                    loop (guard ())
 
                member inline s.For(seq: _ seq, body) =
                    s.Using(seq.GetEnumerator(), fun enum -> s.While(enum.MoveNext, s.Delay(fun () -> body enum.Current)))


            /// Build a monad through recursive (effectful) computations. Computation proceeds through the use of a continuation function applied to the intermediate result.
            /// The default monadic 'identity' function is used in each iteration where the continuation is applied.
            let inline recM f x : Cont< ^r, ^a> =
                let rec go m = bind (f wrapgo) m
                and wrapgo x = go (wrap x)
                go (f wrap x)

            /// Build a monad through recursive (effectful) computations. Computation proceeds through the use of a continuation function applied to an 'effect' applied over the intermediate result.
            /// Any constructor can be used in each iteration, in the case of union-types.
            let inline recMp f x : Cont< ^r, ^a> =
                let rec go m = bind (f go) m in go (f id x)          

            /// <summary>Monadic fold over a structure associating to the right.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldrM (f: ^a -> ^s -> Cont< ^r, ^s>) (s0: ^s) (source: ^a seq) : Cont< ^r, ^s> =
                let g k x s = bind k (f x s)
                Seq.fold g wrap source s0

            /// <summary>Monadic fold over a structure associating to the left.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldlM (f: ^s -> ^a -> Cont< ^r, ^s>) (s0: ^s) (source: ^a seq) : Cont< ^r, ^s> =
                let g x k s = bind k (f s x)
                Seq.foldBack g source wrap s0    

  
        /// Supplementary Applicative operations on the given type.
        module Applicative =

            /// Lift a value onto an effectful context.
            let wrap (x: 'a) : Cont<'r, ^a> = Cont (fun k -> k x)

            /// Sequential application on effects.
            let inline ap (Cont mv) (Cont mf) : Cont< ^r, ^b> =
                Cont (fun k -> mf (fun f -> mv (fun a -> k (f a))))

            /// Lift a binary function on effects.
            let inline map2 (f: ^a -> ^b -> ^c) (Cont ca) (Cont cb) : Cont< ^r, ^c> =
                Cont (fun k -> ca (fun a -> cb (fun b -> k (f a b))))

            /// Lift a ternary function on effects.
            let inline map3 (f: ^a -> ^b -> ^c -> ^d) (Cont ca) (Cont cb) (Cont cc) : Cont< ^r, ^d> =
                Cont (fun k -> ca (fun a -> cb (fun b -> cc (fun c -> k (f a b c)))))

            /// Sequentially compose two effects, discarding any value produced by the first.
            let inline andThen (Cont cb) (Cont ca) : Cont< ^r, ^b> =
                Cont (fun k -> ca (fun _ -> cb k))

            /// Conditional execution of effectful expressions.
            let inline when_ condition f : Cont< ^r, unit> =
                if condition then f () else wrap ()

            /// <summary>Generalizes the sequence-based filter function.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline filterA (p: ^a -> Cont< ^r, bool>) (source: ^a seq) : Cont< ^r, ^a list> =
                let cons x b xs = if b then x::xs else xs
                let f x xs = map2 (cons x) (p x) xs
                Seq.foldBack f source (wrap [])

            /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline sequenceA source : Cont< ^r, ^a list> =
                let cons x xs = x::xs
                let f x xs = map2 cons x xs
                Seq.foldBack f source (wrap [])

            /// <summary>Produce an effect for the elements in the sequence from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline forA f (source: ^a seq) : Cont< ^r, ^b list> =
                sequenceA (Seq.map f source)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when either input sequence is null.</exception>
            let inline for2A f (source1: ^a seq) (source2: ^b seq) : Cont< ^r, ^c list> =
                sequenceA (seq { for x in source1 do for y in source2 -> f x y }) 

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right, then evaluate each effect and collect the results.
            /// If one sequence is longer, its extra elements are ignored.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline zipWithA f (source1: ^a seq) (source2: ^b seq) : Cont< ^r, ^c list> =
                sequenceA (Seq.map2 f source1 source2)

            /// Performs the effect 'n' times.
            let inline replicateA count (Cont c) : Cont< ^r, ^a seq> =
                Cont (fun k -> c (fun x -> k (Seq.replicate (max 0 count) x)))


        /// Supplementary Functor operations on the given type.
        module Functor =

            /// Lift a function onto effects.
            let inline map (f: ^a -> ^b) (Cont c) : Cont< ^r, ^b> =
                Cont (fun k -> c (fun x -> k (f x)))

            /// Replace all locations in the input with the same value.
            let inline replace (b: 'b) (Cont (c: ('a -> 'r) -> ^r)) : Cont< ^r, ^b> =
                Cont (fun k -> c (fun _ -> k b))

            /// Perform an operation, store its result, perform an action using both
            /// the input and output, and finally return the output.
            let inline tee (f: ^a -> ^b) (g: ^a -> ^b -> unit) (Cont c) : Cont< ^r, ^b> =
                Cont (fun k -> c (fun a -> let b = f a in g a b; k b))


        /// Supplementary Comonad operations on the given type.
        module Comonad =

            /// Retrieve a value out of a context.
            let extract (Cont (c: ('a -> ^a) -> ^a)) : ^a = c id

            /// Sequentially compose two co-effects.
            let inline extend j (w: Cont< ^r, ^a>) : Cont< ^r, ^b> =
                Cont (fun k -> k (j w))

            /// Takes a comonadic container and produces a container of containers.
            let duplicate (w: Cont<'r, 'a>) : Cont< ^r, Cont< ^r, ^a>> =
                Cont (fun k -> k w)

            /// Deconstructs a comonad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function.
            let inline recW f w : ^r =
                let rec go w = f go w in go (extend (f extract) w)


        /// Types with a binary, associative composition operation.
        module Semigroup =

            /// An associative composition operation.
            let inline sappend (Cont ca) (Cont cb) : Cont< ^r, ^a> =
                Cont (fun k ->
                    ca (fun a ->
                        cb (fun b -> k (^a: (static member Append: ^a -> ^a -> ^a) (a, b)))))


    /// Creates a computation expression for the given type.
    let cont = Compose.Monad.ContBuilder ()



open Cont
open Compose
 
// @ Operators @
type Cont<'R, 'T> with

// @ Primitive @

    /// The result of running a CPS computation with a given final continuation.
    static member inline ( >- ) (m, k) = runCont m k
    /// The result of running a CPS computation with a given final continuation.
    static member inline ( -< ) (k, m) = runCont m k

// @ Monad @

    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
    static member inline ( >>= ) (m, k) = Monad.bind k m
    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
    static member inline ( =<< ) (k, m) = Monad.bind k m

// @ Applicative @

    /// Sequential application on effects.
    static member inline ( <*> ) (ff, fx) = Applicative.ap fx ff
    /// Sequential application on effects.
    static member inline ( <**> ) (fx, ff) = Applicative.ap fx ff

    /// Sequentially compose two effects, discarding any value produced by the first.
    static member inline ( *> ) (fa, fb) = Applicative.andThen fb fa
    /// Sequentially compose two effects, discarding any value produced by the first.
    static member inline ( <* ) (fb, fa) = Applicative.andThen fb fa

// @ Functor @

    /// Lift a function onto effects.
    static member inline ( |%> ) (fa, f) = Functor.map f fa
    /// Lift a function onto effects.
    static member inline ( <%| ) (f, fa) = Functor.map f fa

    /// Replace all locations in the input with the same value.
    static member inline ( %> ) (fa, b) = Functor.replace b fa
    /// Replace all locations in the input with the same value.
    static member inline ( <% ) (b, fa) = Functor.replace b fa

// @ Comonad @

    /// Sequentially compose two co-effects.
    static member inline ( =>> ) (w, j) = Comonad.extend j w
    /// Sequentially compose two co-effects.
    static member inline ( <<= ) (j, w) = Comonad.extend j w

// @ Semigroup @

    /// An associative composition operation.
    static member inline Append (e1, e2) = Semigroup.sappend e1 e2