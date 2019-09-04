namespace PTR.Context.Type


/// Result of a `RWS` computation.
[<Struct>]
type RWSResult<'State, 'Log, 'Result> =
    { State: ^State
    ; Log:   ^Log
    ; Value: ^Result }


/// A computation that, when given an input environment and an initial state,
/// returns a modified state, a 'log', and an output.
[<Struct; NoComparison; NoEquality>]
type RWS<'Env, 'State1, 'State2, 'Log, 'Result> =
    RWS of (^Env -> ^State1 -> RWSResult< ^State2, ^Log, ^Result>)


/// Operations on `RWS` values.
module RWS =

    /// Unwrap an 'RWS' computation as a function.
    let runRWS (e: 'e) (s: 's1) (RWS (r: ^e -> ^s1 -> RWSResult<'s2, 'w, 'a>))
        : RWSResult< ^s2, ^w, ^a> = r e s

    /// Evaluate a computation with the given initial state and environment,
    /// returning the final value and output, discarding the final state.
    let evalRWS (e: 'e) (s: 's1) (RWS (r: ^e -> ^s1 -> RWSResult<'s2, 'w, 'a>)) : ^w * ^a =
        match r e s with wa -> wa.Log, wa.Value

    /// Evaluate a computation with the given initial state and environment,
    /// returning the final state and output, discarding the final value.
    let execRWS (e: 'e) (s: 's1) (RWS (r: ^e -> ^s1 -> RWSResult<'s2, 'w, 'a>)) : ^s2 * ^w =
        match r e s with sw -> sw.State, sw.Log

    /// Map the return value, final state, and output of a computation using the given function.
    let inline mapRWS (f: ^s1 -> ^w0 -> ^a0 -> RWSResult< ^s2, ^w, ^a>) (RWS r) =
        RWS (fun (e: ^e) (s: ^s0) -> match r e s with swa -> f (swa.State: ^s1) swa.Log swa.Value)

    /// Executes action with an initial environment and state modified by applying 'f'.
    let inline withRWS (f: ^e -> ^s1 -> ^e0 * ^s0) (RWS r) : RWS< ^e, ^s1, ^s2, ^w, ^a> =
        RWS (fun e s -> match f e s with e', s' -> r e' s')

    /// Cache the results of a 'RWS' computation.
    let inline cacheRWS (RWS r) : RWS< ^e, ^s1, ^s2, ^w, ^a> =
        let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        RWS (fun e s -> let p = struct (e, s)
                        match d.TryGetValue(p) with
                        | true, res -> res
                        | false, _  -> let res = r e s in d.[p] <- res ; res)


    /// Compositional operations on `RWS` values.
    module Compose =

        /// Supplementary Monad operations on the given type.
        module Monad =

            /// Lift a value onto an effectful context.
            let inline wrap (x: ^a) : RWS< ^e, ^s, ^s, ^w, ^a> =
                RWS (fun _ s ->
                    { RWSResult.State = s
                    ; Log = (^w: (static member inline Empty: unit -> ^w) ())
                    ; Value = x})

            /// Sequentially compose two effects, passing any value produced by the first
            /// as an argument to the second.
            let inline bind (k: ^a -> RWS< ^e, ^s1, ^s2, ^w, ^b>) (RWS r) = RWS (fun e s ->
                match r e (s: ^s0) with
                | swa ->
                    match k swa.Value with
                    | RWS r -> match r e swa.State with
                               | swb -> { swb with RWSResult.Log =
                                                    (^w: (static member Append: ^w -> ^w -> ^w) (swa.Log, swb.Log)) })
      
            /// Removes one layer of monadic context from a nested monad.
            let inline flatten (RWS (r: ^e -> ^s0 -> RWSResult< ^s1, ^w, RWS< ^e, ^s1, ^s2, ^w, ^a>>)) = RWS (fun e s ->
                match r e (s: ^s0) with
                | swa ->
                    match swa.Value with
                    | RWS r -> match r e swa.State with
                               | swb -> { swb with RWSResult.Log =
                                                    (^w: (static member Append: ^w -> ^w -> ^w) (swa.Log, swb.Log)) })
      


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
                    s.TryFinally((fun () -> body disp), fun () -> match box disp with null -> () | _ -> disp.Dispose ())
 
                member inline s.While(guard, body) =
                    let rec loop = function
                    | false -> s.Zero ()
                    | true -> s.Bind(body (), fun x -> loop (guard x))
                    loop (guard ())
 
                member inline s.For(seq: _ seq, body) =
                    s.Using(seq.GetEnumerator(), fun enum -> s.While(enum.MoveNext, s.Delay(fun () -> body enum.Current)))


            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to the intermediate result.
            /// The default monadic 'identity' function is used in each iteration where the continuation is applied.
            let inline recM f x : RWS< ^e, ^s, ^s, ^w, ^a> =
                let rec go m = bind (f wrapgo) m
                and wrapgo x = go (wrap x)
                go (f wrap x)

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
            let inline wrap (x: ^a) : RWS< ^e, ^s, ^s, ^w, ^a> = Monad.wrap x

            /// Sequential application on effects.
            let inline ap (RWS rv) (RWS rf) : RWS< ^e, ^s0, ^s2, ^w, ^b> = RWS (fun e s ->
                match rf e s with
                | swf ->
                    match rv e (swf.State: ^s1) with
                    | swv ->
                        { RWSResult.State = swv.State
                        ; Log = (^w: (static member Append: ^w -> ^w -> ^w) (swf.Log, swv.Log))
                        ; Value = swf.Value swv.Value })

            /// Lift a binary function on effects.
            let inline map2 f (RWS ra) (RWS rb) : RWS< ^e, ^s1, ^s3, ^w, ^c> = RWS (fun e s ->
                match ra e s with
                | swa ->
                    match rb e (swa.State: ^s2) with
                    | swb ->
                        { RWSResult.State = swb.State
                        ; Log = (^w: (static member Append: ^w -> ^w -> ^w) (swa.Log, swb.Log))
                        ; Value = f swa.Value swb.Value })

            /// Lift a ternary function on effects.
            let inline map3 f (RWS ra) (RWS rb) (RWS rc) : RWS< ^e, ^s1, ^s4, ^w, ^d> = RWS (fun e s ->
                let inline app a b = (^w: (static member Append: ^w -> ^w -> ^w) (a, b))
                match ra e s with
                | swa ->
                match rb e (swa.State: ^s2) with
                | swb ->
                match rc e (swb.State: ^s3) with
                | swc ->
                    { RWSResult.State = swc.State
                    ; Log = app swa.Log (app swb.Log swc.Log)
                    ; Value = f swa.Value swb.Value swc.Value })

            /// Sequentially compose two effects, discarding any value produced by the first.
            let inline andThen fb fa : RWS< ^e, ^s1, ^s3, ^w, ^b> =
                map2 (fun _ b -> b) (fa: RWS< ^e, ^s1, ^s2, ^w, ^a>) fb

            /// Conditional execution of effectful expressions.
            let inline when_ condition f : RWS< ^e, ^s, ^s, ^w, unit> =
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

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right,
            /// then evaluate each effect and collect the results.
            /// If one sequence is longer, its extra elements are ignored.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline zipWithA f source1 source2 : RWS< ^e, ^s, ^s, ^w, ^c list> =
                sequenceA (Seq.map2 f source1 source2)

            /// Performs the effect 'n' times.
            let inline replicateA count fa : RWS< ^e, ^s, ^s, ^w, ^a list> =
                sequenceA (Seq.replicate (max 0 count) fa)


        /// Supplementary Functor operations on the given type.
        module Functor =

            /// Lift a function onto effects.
            let inline map f (RWS r) : RWS< ^e, ^s1, ^s2, ^w, ^b> =
                RWS (fun e s ->
                    let swa = r e s
                    { RWSResult.State = swa.State
                    ; Log = swa.Log
                    ; Value = f swa.Value })

            /// Replace all locations in the input with the same value.
            let inline replace (b: ^b) (RWS r) : RWS< ^e, ^s1, ^s2, ^w, ^b> =
                RWS (fun e s ->
                    let sw_ = r e s
                    { RWSResult.State = sw_.State
                    ; Log = sw_.Log
                    ; Value = b })

            /// Perform an operation, store its result, perform an action using both
            /// the input and output, and finally return the output.
            let inline tee f (g: ^a -> ^b -> unit) (RWS r) : RWS< ^e, ^s1, ^s2, ^w, ^b> =
                RWS (fun e s ->
                    let swa = r e s
                    let b = f swa.Value
                    do g swa.Value b
                    { RWSResult.State = swa.State
                    ; Log = swa.Log
                    ; Value = b })


        /// A three paramater functor where the first, second, and third arguments are covariant.
        module Trifunctor =

            /// Map over all three arguments at the same time.
            let inline trimap (f: ^s2 -> ^s3) (g: ^w1 -> ^w2) (h: ^a -> ^b) (RWS r)
                : RWS< ^e, ^s1, ^s3, ^w2, ^b> =
                RWS (fun e s ->
                    let swa = r e s
                    { RWSResult.State = f swa.State
                    ; Log = g swa.Log
                    ; Value = h swa.Value })

            /// Map over both arguments at the same time.
            let inline bimap (f: ^w1 -> ^w2) (g: ^a -> ^b) (RWS r)
                : RWS< ^e, ^s1, ^s2, ^w2, ^b> =
                    RWS (fun e s ->
                        let swa = r e s
                        { RWSResult.State = swa.State
                        ; Log = f swa.Log
                        ; Value = g swa.Value })

            /// Map covariantly over the first argument.
            let inline mapFst (f: ^s2 -> ^s3) (RWS r) : RWS< ^e, ^s1, ^s3, ^w, ^a> =
                RWS (fun e s ->
                    let swa = r e s
                    { RWSResult.State = f swa.State
                    ; Log = swa.Log
                    ; Value = swa.Value })

            /// Map covariantly over the second argument.
            let inline mapSnd (g: ^w0 -> ^w1) (RWS r) : RWS< ^e, ^s1, ^s2, ^w1, ^b> =
                RWS (fun e s ->
                    let swa = r e s
                    { RWSResult.State = swa.State
                    ; Log = g swa.Log
                    ; Value = swa.Value })

            /// Map covariantly over the third argument.
            let inline mapThr (h: ^a -> ^b) (RWS r) : RWS< ^e, ^s1, ^s2, ^w, ^b> =
                RWS (fun e s ->
                    let swa = r e s
                    { RWSResult.State = swa.State
                    ; Log = swa.Log
                    ; Value = h swa.Value })


        /// Types with a binary, associative composition operation.
        module Semigroup =

            /// An associative composition operation.
            let inline sappend (RWS ra) (RWS rb) : RWS< ^e, ^s1, ^s3, ^w, ^a> = RWS (fun e s ->
                match ra e s with
                | swa ->
                match rb e (swa.State: ^s2) with
                | swb ->
                    { RWSResult.State = swb.State
                    ; Log = (^w: (static member Append: ^w -> ^w -> ^w) (swa.Log, swb.Log))
                    ; Value = (^a: (static member Append: ^a -> ^a -> ^a) (swa.Value, swb.Value)) })


    /// Creates a computation expression for the given type.
    let rws = Compose.Monad.RWSBuilder ()



open RWS
open Compose
 
// @ Operators @
type RWS<'E, 'S1, 'S2, 'L, 'R> with

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
    static member inline ( %> ) (b, fa) = Functor.replace b fa
    /// Replace all locations in the input with the same value.
    static member inline ( <% ) (fa, b) = Functor.replace b fa

// @ Semigroup @

    /// An associative composition operation.
    static member inline Append (e1, e2) = Semigroup.sappend e1 e2
