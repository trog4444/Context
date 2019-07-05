namespace Ptr.Context.Type


/// A CPS ("continuation-passing style") computation that produces an intermediate
/// result of type `a` within a CPS computation whose final result type is `r`.
[<Struct; NoComparison; NoEquality>]
type Cont<'r, 'a> = Cont of ((^a -> ^r) -> ^r)


/// Operations on `Cont` values.
module Cont =

    /// The result of running a CPS computation with a given final continuation.
    let inline runCont (k: ^a -> ^r) (Cont c) = c k

    /// The result of running a CPS computation with a given final continuation.
    let inline runCont' (Cont c) : (^a -> ^r) -> ^r = c

    /// The result of running a CPS computation with the identity as the final continuation.
    let inline evalCont (Cont c) : ^r = c id

    /// Apply a function to transform the result of a continuation-passing computation.
    let inline mapCont f (Cont c) : Cont< ^r, ^a> = Cont (c >> f)

    /// Apply a function to transform the continuation passed to a CPS computation.
    let inline withCont f (Cont c) : Cont< ^r, ^b> = Cont (f >> c)

    /// End the current continuation chain with a specific value.
    let inline exit x : Cont< ^r, ^a> = Cont (fun _ -> x)
  
    /// shift 'f' captures the continuation up to the nearest enclosing 'reset' and passes it to 'f'.
    let inline shift f : Cont< ^r, ^a> = Cont (f >> evalCont)

    /// reset 'm' delimits the continuation of any shift inside 'm'.
    let inline reset m : Cont< ^r0, ^r> = Cont (fun k -> k (evalCont m))  

    /// Call with current continuation.
    let inline callCC f : Cont< ^r, ^a> =
        Cont (fun k -> match f (k >> exit) with Cont c -> c k)

    /// Allows looping with a given continuation function and input.
    let inline getCC x0 : Cont< ^r, ^a * (^a -> Cont< ^r, ^b>)> =
        callCC (fun c -> let rec f x = c (x, f) in Cont (fun k -> k (x0, f)))

    /// Attempt to apply `fOk` on an input and return a continuation. If the function fails,
    /// return a continuation with the result of applying `fErr` to the input and exception.
    let inline tryCC fOk fErr (input: ^a) : Cont< ^r, ^b> =
        callCC (fun ok -> callCC (fun er -> try ok (fOk input) with e -> er (fErr input e)))

    /// Caches the result(s) of a `Cont` computation.
    let inline cacheCont (Cont c) : Cont< ^r, ^a> =
        let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        Cont (fun k -> c (fun a ->
            match d.TryGetValue(a) with
            | true, r -> r
            | false, _ -> let r = k a in d.[a] <- r ; r))



    /// Compositional operations on `Cont` values.
    module Compose =

        /// Supplementary Monad operations on the given type.
        module Monad =

            /// Lift a value onto an effectful context.
            let inline wrap x : Cont< ^r, ^a> = Cont (fun k -> k x)

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
                    s.TryFinally((fun () -> body disp),
                        fun () -> match box disp with null -> () | _ -> disp.Dispose ())
  
                member inline s.While(guard, body) =
                    let rec loop = function
                    | false -> s.Zero ()
                    | true -> s.Bind(body (), guard >> loop)
                    loop (guard ())
  
                member inline s.For(seq: _ seq, body) =
                    s.Using(seq.GetEnumerator(),
                        fun enum -> s.While(enum.MoveNext,
                                        s.Delay(fun () -> body enum.Current)))


            /// Composes two monadic functions together.
            /// Acts as the composition function in the Kleisli category.
            let inline composeM k2 (k1: ^a -> Cont< ^r, ^b>) : ^a -> Cont< ^r, ^c> = k1 >> bind k2
  
            /// Sequentially compose three actions, passing any value produced by the first
            /// two as arguments to the third.
            let inline bind2 (k: ^a -> ^b -> Cont< ^r, ^c>)
                                (ma: Cont< ^r, ^a>)
                                (mb: Cont< ^r, ^b>) : Cont< ^r, ^c> =
                bind (fun a -> bind (k a) mb) ma

            /// Sequentially compose four actions, passing any value produced by the
            /// first two as arguments to the third.
            let inline bind3 (k: ^a -> ^b -> ^c -> Cont< ^r, ^d>)
                                (ma: Cont< ^r, ^a>)
                                (mb: Cont< ^r, ^b>)
                                (mc: Cont< ^r, ^c>) : Cont< ^r, ^d> =
                bind2 (fun a b -> bind (k a b) mc) ma mb

            /// Sequentially compose two actions, creating a third from the result and
            /// lifting a binary function on its effects.
            let inline bindMap (k: ^a -> Cont< ^r, ^b>)
                               (f: ^a -> ^b -> ^c)
                               (m: Cont< ^r, ^a>) : Cont< ^r, ^c> =
                bind (fun a -> match k a with Cont c -> Cont (fun k -> c (f a >> k))) m

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to the intermediate result.
            /// The default monadic 'identity' function is used in each iteration where the continuation is applied.
            let inline recM (f: (^a -> Cont< ^r, ^a>) -> ^a -> Cont< ^r, ^a>) (x: ^a) : Cont< ^r, ^a> =
                let rec go m = bind (f (wrap >> go)) m in go (f wrap x)

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to an 'effect' applied over the intermediate result.
            /// Any constructor can be used in each iteration, in the case of union-types.
            let inline recMp f x =
                let rec go m = bind (f go) m in go (f id x)                   

            /// <summary>Monadic fold over a structure associating to the right.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldrM (f: ^a -> ^s -> Cont< ^r, ^s>) (s0: ^s) (source: ^a seq) : Cont< ^r, ^s> =
                let inline g k x s = bind k (f x s)
                Seq.fold g wrap source s0

            /// <summary>Monadic fold over a structure associating to the left.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldlM (f: ^s -> ^a -> Cont< ^r, ^s>) (s0: ^s) (source: ^a seq) : Cont< ^r, ^s> =
                let inline g x k s = bind k (f s x)
                Seq.foldBack g source wrap s0        


            /// Monadic zipping (combining or decomposing corresponding monadic elements).
            module Zip =
            
                /// Combine the corresponding contents of two monads into a single monad.
                let inline mzipWith (f: ^a -> ^b -> ^c) (Cont fa) (Cont fb) : Cont< ^r, ^c> =
                    Cont (fun k -> fa (fun a -> fb (fun b -> k (f a b))))

                /// Merge the contents (of corresponding pairs) of two monads into a monad of pairs.
                let inline mzip (Cont fa) (Cont fb) : Cont< ^r, ^a * ^b> =
                    Cont (fun k -> fa (fun a -> fb (fun b -> k (a, b))))
                    
                /// Decompose a monad comprised of corresponding pairs of values.
                let inline munzip (Cont ab) : Cont< ^r, ^a> * Cont< ^r, ^b> =
                    Cont (fun k -> ab (fun (a, _) -> k a)), Cont (fun k -> ab (fun (_, b) -> k b))        

    
        /// Supplementary Applicative operations on the given type.
        module Applicative =

            /// Lift a value onto an effectful context.
            let inline wrap x : Cont< ^r, ^a> = Cont (fun k -> k x)

            /// Sequential application on effects.
            let inline ap (Cont mv) (Cont mf) : Cont< ^r, ^b> =
                Cont (fun k -> mf (fun f -> mv (f >> k)))

            /// Lift a binary function on effects.
            let inline map2 (f: ^a -> ^b -> ^c) (Cont ca) (Cont cb) : Cont< ^r, ^c> =
                Cont (fun k -> ca (fun a -> cb (f a >> k)))

            /// Lift a ternary function on effects.
            let inline map3 (f: ^a -> ^b -> ^c -> ^d) (Cont ca) (Cont cb) (Cont cc) : Cont< ^r, ^d> =
                Cont (fun k -> ca (fun a -> cb (fun b -> cc (f a b >> k))))

            /// Sequentially compose two effects, discarding any value produced by the first.
            let inline andThen (Cont cb) (Cont ca) : Cont< ^r, ^b> =
                Cont (fun k -> ca (fun _ -> cb k))

            /// Conditional execution of effectful expressions.
            let inline when_ (condition: bool) f : Cont< ^r, unit> =
                if condition then f () else wrap ()

            /// <summary>Generalizes the sequence-based filter function.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline filterA (p: ^a -> Cont< ^r, bool>) (source: ^a seq) : Cont< ^r, ^a list> =
                Seq.foldBack (fun x xs -> map2 (fun flg xs -> if flg then x::xs else xs) (p x) xs) source (wrap [])

            /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline sequenceA source : Cont< ^r, ^a list> =
                Seq.foldBack (map2 (fun x xs -> x::xs)) source (wrap [])

            /// <summary>Produce an effect for the elements in the sequence from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException"> Thrown when the input sequence is null.</exception>
            let inline forA f (source: ^a seq) : Cont< ^r, ^b list> =
                sequenceA (Seq.map f source)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when either input sequence is null.</exception>
            let inline for2A f (source1: ^a seq) (source2: ^b seq) : Cont< ^r, ^c list> =
                forA ((<||) f) (Seq.allPairs source1 source2)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right, then evaluate each effect and collect the results.
            /// If one sequence is longer, its extra elements are ignored.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline zipWithA f (source1: ^a seq) (source2: ^b seq) : Cont< ^r, ^c list> =
                sequenceA (Seq.map2 f source1 source2) 

            /// Performs the effect 'n' times.
            let inline replicateA (n: uint32) (Cont c) : Cont< ^r, ^a seq> =
                Cont (fun k -> c (Seq.replicate (int n) >> k))


        /// Supplementary Functor operations on the given type.
        module Functor =

            /// Lift a function onto effects.
            let inline map (f: ^a -> ^b) (Cont c) : Cont< ^r, ^b> =
                Cont (fun k -> c (f >> k))

            /// Replace all locations in the input with the same value.
            let inline replace (b: ^b) (Cont c) : Cont< ^r, ^b> =
                Cont (fun k -> c (fun _ -> k b))

            /// Perform an operation, store its result, perform an action using both
            /// the input and output, and finally return the output.
            let inline tee (f: ^a -> ^b) (g: ^a -> ^b -> unit) (Cont c) : Cont< ^r, ^b> =
                Cont (fun k -> c (fun a -> let b = f a in g a b; k b))


        /// Supplementary Comonad operations on the given type.
        module Comonad =

            /// Retrieve a value out of a context.
            let inline extract (Cont c) : ^a = c id

            /// Sequentially compose two co-effects.
            let inline extend j (w: Cont< ^r, ^a>) : Cont< ^r, ^b> =
                Cont (fun k -> k (j w))

            /// Takes a comonadic container and produces a container of containers.
            let inline duplicate w : Cont< ^r, Cont< ^r, ^a>> =
                Cont (fun k -> k w)

            /// Composes two comonadic functions together. Acts as the composition function in the CoKleisli category.
            let inline composeW (f2: Cont< ^r, ^b> -> ^c) f1 : Cont< ^r, ^a> -> ^c = extend f1 >> f2

            /// Sequentially compose two co-actions, creating a third from the result and
            /// lifting a binary function on its effects.
            let inline extendMap j f w : Cont< ^r, ^c> =
                Functor.map (fun a -> f a (j w)) w

            /// Deconstructs a comonad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function.
            let inline recW f w : ^a =
                let rec go w = f go w in go (extend (f extract) w)


        /// Types with a binary, associative composition operation.
        module Semigroup =

            /// An associative composition operation.
            let inline sappend (Cont ca) (Cont cb) : Cont< ^r, ^e> =
                Cont (fun k ->
                    ca (fun a ->
                    cb (fun b -> k (^e: (static member inline ( ++ ): ^e -> ^e -> ^e) (a, b)))))
                

    /// Creates a computation expression for the given type.
    let cont = Compose.Monad.ContBuilder ()



open Cont
open Compose
  
//  @ Operators @
type Cont<'r, 'a> with

// @ Primitive @

    /// The result of running a CPS computation with a given final continuation.
    static member inline ( >- ) (m, k) = runCont k m
    /// The result of running a CPS computation with a given final continuation.
    static member inline ( -< ) (k, m) = runCont k m

// @ Monad @

    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
    static member inline ( >>= ) (m, k) = Monad.bind k m
    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
    static member inline ( =<< ) (k, m) = Monad.bind k m

// @ Applicative @

    /// Sequential application on effects.
    static member inline ( <*> )  (ff, fx) = Applicative.ap fx ff
    /// Sequential application on effects.
    static member inline ( <**> ) (fx, ff) = Applicative.ap fx ff

    /// Sequentially compose two effects, discarding any value produced by the first.
    static member inline ( *> ) (fa, fb) = Applicative.andThen fb fa
    /// Sequentially compose two effects, discarding any value produced by the first.
    static member inline ( <* ) (fb, fa) = Applicative.andThen fb fa

// @ Functor @

    /// Lift a function onto effects.
    static member inline ( |>> ) (fa, f) = Functor.map f fa
    /// Lift a function onto effects.
    static member inline ( <<| ) (f, fa) = Functor.map f fa

    /// Replace all locations in the input with the same value.
    static member inline ( %> ) (b, fx) = Functor.replace b fx
    /// Replace all locations in the input with the same value.
    static member inline ( <% ) (fx, b) = Functor.replace b fx

// @ Comonad @

    /// Sequentially compose two co-effects.
    static member inline ( =>> ) (w, j) = Comonad.extend j w
    /// Sequentially compose two co-effects.
    static member inline ( <<= ) (j, w) = Comonad.extend j w

// @ Semigroup @

    /// An associative composition operation.
    static member inline Append (e1, e2) = Semigroup.sappend e1 e2

    /// An associative composition operation.
    static member inline ( ++ ) (e1, e2) = Semigroup.sappend e1 e2