namespace Ptr.Context.Type


/// A computation that, when given an input environment and an initial state,
/// returns a modified state, a `log`, and an output.
[<Struct; NoComparison; NoEquality>]
type RWS<'env, 'state, '``state*``, 'log, 'value> =
    RWS of (^env -> ^state -> ^``state*`` * ^log * ^value)


/// Operations on `RWS` values.
module RWS =

    /// Unwrap an RWS computation as a function.
    let runRWS (e: 'e) (s: 's) (RWS r) : '``s*`` * 'w * 'a =
        match r e s with s, w, a -> s, w, a

    /// Evaluate a computation with the given initial state and environment,
    /// returning the final value and output, discarding the final state.
    let evalRWS (e: 'e) (s: 's) (RWS r) : 'w * 'a  = match r e s with (_, w, a) -> w, a

    /// Evaluate a computation with the given initial state and environment,
    /// returning the final state and output, discarding the final value.
    let execRWS (e: 'e) (s: 's) (RWS r) : '``s*`` * 'w = match r e s with (s, w, _) -> s, w

    /// Map the return value, final state, and output of a computation using the given function.
    let inline mapRWS (f: ^``s*`` -> ^w0 -> ^a0 -> ^``s**`` * ^w * ^a) (RWS r) =
        RWS (fun (e: ^e) (s: ^s) -> match r e s with (s, w, a) -> f (s: ^``s*``), w, a)

    /// Executes action with an initial environment and state modified by applying 'f'.
    let inline withRWS (f: ^e -> ^s -> ^e0 * ^s0) (RWS r) : RWS< ^e, ^s, ^``s*``, ^w, ^a> =
        RWS (fun e s -> match f e s with e', s' -> r e' s')

    /// Cache the results of a 'RWS' computation.
    let cacheRWS (RWS r) : RWS<'e, 's, '``s*``, 'w, 'a> =
        let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        RWS (fun e s -> let p = (e, s)
                        match d.TryGetValue(p) with
                        | true, res -> res
                        | false, _ -> let res = r e s in d.[p] <- res ; res)


    /// Compositional operations on `RWS` values.
    module Compose =        

        /// Supplementary Monad operations on the given type.
        module Monad =

            /// Lift a value onto an effectful context.
            let inline wrap x : RWS< ^e, ^s, ^s, ^w, ^a> =
                RWS (fun _ s -> s, (^w: (static member Empty: unit -> ^w) ()), x)

            /// Sequentially compose two effects, passing any value produced by the first
            /// as an argument to the second.
            let inline bind (k: ^a -> RWS< ^e, ^``s*``, ^``s**``, ^w, ^b>) (RWS r) = RWS (fun e s ->
                match r e (s: ^s) with
                | (s, w, a) ->
                    match k a with
                    | RWS r -> match r e s with
                               | (s, w', b) ->
                                 s, (^w: (static member Append: ^w -> ^w -> ^w) (w, w')), b)
            
            /// Removes one layer of monadic context from a nested monad.
            let inline flatten (RWS rr) = RWS (fun e s ->
                match rr e (s: ^s0) with
                | (s: ^s1, w, (RWS r)) ->
                    match r e s with
                    | (s: ^``s2``, w', b) ->
                        s, (^w: (static member Append: ^w -> ^w -> ^w) (w, w')), b)


            /// Monadic computation builder specialised to the given monad.
            type RWSBuilder () =
                member inline s.Bind(m, k) = bind k m
                member inline s.Return x = wrap x
                member inline s.ReturnFrom m : RWS< ^e, ^s, ^``s*``, ^w, ^a> = m
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


            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to the intermediate result.
            /// The default monadic 'identity' function is used in each iteration where the continuation is applied.
            let inline recM f x : RWS< ^e, ^s, ^s, ^w, ^a> =
                let rec go m = bind (f (wrap >> go)) m in go (f wrap x)

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to an 'effect' applied over the intermediate result.
            /// Any constructor can be used in each iteration, in the case of union-types.
            let inline recMp f x : RWS< ^e, ^s, ^s, ^w, ^a> =
                let rec go m = bind (f go) m in go (f id x)                    

            /// <summary>Monadic fold over a structure associating to the right.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldrM (f: ^a -> ^z -> RWS< ^e, ^s, ^s, ^w, ^z>) (s0: ^z) (source: ^a seq)
                : RWS< ^e, ^s, ^s, ^w, ^z> =
                let g k x s = bind k (f x s)
                Seq.fold g wrap source s0

            /// <summary>Monadic fold over a structure associating to the left.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldlM (f: ^z -> ^a -> RWS< ^e, ^s, ^s, ^w, ^z>) (s0: ^z) (source: ^a seq)
                : RWS< ^e, ^s, ^s, ^w, ^z> =
                let g x k s = bind k (f s x)
                Seq.foldBack g source wrap s0


        /// Supplementary Applicative operations on the given type.
        module Applicative =

            /// Lift a value onto an effectful context.
            let inline wrap x : RWS< ^e, ^s, ^s, ^w, ^a> =
                RWS (fun _ s -> s, (^w: (static member Empty: unit -> ^w) ()), x)

            /// Sequential application on effects.
            let inline ap (RWS rv) (RWS rf) : RWS< ^e, ^s, ^``s**``, ^w, ^b> = RWS (fun e s ->
                match rf e s with
                | (s, w, f) ->
                    match rv e (s: ^``s*``) with
                    | (s, w', v) ->
                        (s, (^w: (static member Append: ^w -> ^w -> ^w) (w, w')), f v))

            /// Lift a binary function on effects.
            let inline map2 f (RWS ra) (RWS rb) : RWS< ^e, ^s, ^``s**``, ^w, ^c> = RWS (fun e s ->
                match ra e s with
                | s, w, a ->
                    match rb e (s: ^``s*``) with
                    | s, w1, b -> s, (^w: (static member Append: ^w -> ^w -> ^w) (w, w1)), (f a b))

            /// Lift a ternary function on effects.
            let inline map3 f (RWS ra) (RWS rb) (RWS rc) : RWS< ^e, ^s, ^``s***``, ^w, ^d> = RWS (fun e s ->
                let inline app a b = (^w: (static member Append: ^w -> ^w -> ^w) (a, b))
                match ra e s with
                | s, w, a ->
                    match rb e (s: ^``s*``) with
                    | s, w1, b ->
                        match rc e (s: ^``s**``) with
                        | s, w2, c -> s, (app w (app w1 w2)), f a b c)

            /// Sequentially compose two effects, discarding any value produced by the first.
            let inline andThen fb fa : RWS< ^e, ^s, ^``s**``, ^w, ^b> =
                map2 (fun _ b -> b) (fa: RWS< ^e, ^s, ^``s*``, ^w, ^a>) fb

            /// Conditional execution of effectful expressions.
            let inline when_ (condition: bool) f : RWS< ^e, ^s, ^s, ^w, unit> =
                if condition then f () else wrap ()

            /// <summary>Generalizes the sequence-based filter function.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline filterA (p: ^a -> RWS< ^e, ^s, ^s, ^w, bool>) source =    
                Seq.foldBack (fun x -> map2 (fun flg xs -> if flg then x::xs else xs) (p x)) source (wrap [])

            /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline sequenceA (source: RWS< ^e, ^s, ^s, ^w, ^a> seq) : RWS< ^e, ^s, ^s, ^w, ^a list> =
                Seq.foldBack (map2 (fun x xs -> x::xs)) source (wrap [])

            /// <summary>Produce an effect for the elements in the sequence from left to right
            /// then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline forA f source : RWS< ^e, ^s, ^s, ^w, ^b list> =
                sequenceA (Seq.map f source)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right
            /// then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when either input sequence is null.</exception>
            let inline for2A f source1 source2 : RWS< ^e, ^s, ^s, ^w, ^c list> =
                forA ((<||) f) (Seq.allPairs source1 source2)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right,
            /// then evaluate each effect and collect the results.
            /// If one sequence is longer, its extra elements are ignored.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline zipWithA f source1 source2 : RWS< ^e, ^s, ^s, ^w, ^c list> =
                sequenceA (Seq.map2 f source1 source2)

            /// Performs the effect 'n' times.
            let inline replicateA n m : RWS< ^e, ^s, ^s, ^w, ^a list> =
                sequenceA (Seq.replicate (max 0 n) m)


        /// Supplementary Functor operations on the given type.
        module Functor =

            /// Lift a function onto effects.
            let inline map f (RWS r) : RWS< ^e, ^s, ^``s*``, ^w, ^b> =
                RWS (fun e s -> match r e s with (s, w, a) -> s, w, f a)

            /// Replace all locations in the input with the same value.
            let replace (b: 'b) (RWS r) : RWS<'e, 's, '``s*``, 'w, 'b> =
                RWS (fun e s -> match r e s with s, w, _ -> s, w, b)

            /// Perform an operation, store its result, perform an action using both
            /// the input and output, and finally return the output.
            let inline tee f (g: ^a -> ^b -> unit) (RWS rs) : RWS< ^e, ^s, ^``s*``, ^w, ^b> =
                RWS (fun e s -> match rs e s with
                                | s, w, a -> let b = f a in g a b; s, w, b)


        /// A three paramater functor where the first, second, and third arguments are covariant.
        module Trifunctor =

            /// Map over all three arguments at the same time.
            let inline trimap (f: ^``s*`` -> ^``s**``) (g: ^w1 -> ^w2) (h: ^a -> ^b) (RWS r)
                : RWS< ^e, ^s, ^``s**``, ^w2, ^b> =
                    RWS (fun e s -> match r e s with s, w, a -> f s, (g w), h a)

            /// Map over both arguments at the same time.
            let inline bimap (f: ^w1 -> ^w2) (g: ^a -> ^b) (RWS r)
                : RWS< ^e, ^s, ^``s*``, ^w2, ^b> =
                    RWS (fun e s -> match r e s with s, w, a -> s, (f w), g a)

            /// Map covariantly over the first argument.
            let inline mapFst (f: ^``s*`` -> ^``s**``) (RWS r) : RWS< ^e, ^s, ^``s**``, ^w2, ^a> =
                RWS (fun e s -> match r e s with s, w, a -> f s, w, a)

            /// Map covariantly over the second argument.
            let inline mapSnd (g: ^w0 -> ^w1) (RWS r) : RWS< ^e, ^s, ^``s*``, ^w1, ^b> =
                RWS (fun e s -> match r e s with s, w, a -> s, g w, a)

            /// Map covariantly over the third argument.
            let inline mapThr (h: ^a -> ^b) (RWS r) : RWS< ^e, ^s, ^``s*``, ^w, ^b> =
                RWS (fun e s -> match r e s with s, w, a -> s, w, h a)


        /// Types with a binary, associative composition operation.
        module Semigroup =

            /// An associative composition operation.
            let inline sappend (RWS ra) (RWS rb) : RWS< ^e, ^s, ^``s**``, ^w, ^a> = RWS (fun e s ->
                match ra e s with
                | s, w, a ->
                    match rb e (s: ^``s*``) with
                    | s, w1, b ->
                        s, (^w: (static member Append: ^w -> ^w -> ^w) (w, w1)), (^a: (static member Append: ^a -> ^a -> ^a) (a, b)))


    /// Creates a computation expression for the given type.
    let rws = Compose.Monad.RWSBuilder ()



open RWS
open Compose
  
//  @ Operators @
type RWS<'e, 's, '``s*``, 'w, 'a> with

// @ Primitive @

    /// Unwrap an RWS computation as a function.
    static member inline ( >- ) (r, (e, s)) = runRWS e s r
    /// Unwrap an RWS computation as a function.
    static member inline ( -< ) ((e, s), r) = runRWS e s r

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
    static member inline ( |%> ) (fa, f) = Functor.map f fa
    /// Lift a function onto effects.
    static member inline ( <%| ) (f, fa) = Functor.map f fa

    /// Replace all locations in the input with the same value.
    static member inline ( %> ) (b, fa) = Functor.replace b fa
    /// Replace all locations in the input with the same value.
    static member inline ( <% ) (fa, b) = Functor.replace b fa

// @ Semigroup @

    /// An associative composition operation.
    static member inline Append (e1, e2) = Semigroup.sappend e1 e2

    /// An associative composition operation.
    static member inline ( ++ ) (e1, e2) = Semigroup.sappend e1 e2