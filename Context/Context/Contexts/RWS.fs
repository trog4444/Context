namespace Ptr.Context.Type


/// The result of running a `RWS` computation, which includes a state, a log, and a value.
[<Struct>]
type RWSResult<'state, 'log, 'value> = { Read: ^log ; State: ^state ; Value: ^value }


/// A computation that, when given an input environment and an initial state,
/// returns a modified state, a `log`, and an output.
[<Struct; NoComparison; NoEquality>]
type RWS<'env, 'state, '``state*``, 'log, 'value> =
    RWS of (^env -> ^state -> RWSResult< ^``state*``, ^log, ^value>)


/// Operations on `RWS` values.
module RWS =

    /// `Read/State/Value`-triple conversion methods.
    type RWSConvert =
    
        /// Create a state/log/value combination.
        static member inline Curried state log value : RWSResult< ^s, ^l, ^v> =
            { State = state ; Read = log ; Value = value }
    
        /// Create a state/log/value combination.
        static member inline OfTriple (state: ^s, log: ^l, value: ^v) : RWSResult< ^s, ^l, ^v> =
            { State = state ; Read = log ; Value = value }
    
        /// Create a state/log/value combination.
        static member inline OfTriple (struct (state: ^s, log: ^l, value: ^v)) : RWSResult< ^s, ^l, ^v> =
            { State = state ; Read = log ; Value = value }
    
        /// Convert a function yielding a triple into a State/Log/Value function.
        static member inline OfTriple f : RWS< ^e, ^s0, ^s1, ^l, ^v> =
            RWS (fun e s -> match f e s with (s, l, v) -> { State = s ; Read = l ; Value = v })
    
        /// Convert a function yielding a pair into a State/Value function.
        static member inline OfTriple f : RWS< ^e, ^s0, ^s1, ^l, ^a> =
            RWS (fun e s -> match f e s with struct (s, l, a) -> { State = s ; Read = l ; Value = a })
    
        /// Create a ref-tuple from a state/log/value pair.
        static member inline ToTriple (r: RWSResult< ^s, ^l, ^v>) = (r.State, r.Read, r.Value)
    
        /// Create a struct-tuple from a state/log/value pair.
        static member inline ToTriple1 (r: RWSResult< ^s, ^l, ^v>) = struct (r.State, r.Read, r.Value)
    

    /// Unwrap an RWS computation as a function.
    let inline runRWS (e: ^e) (s: ^s) (RWS r) : RWSResult< ^``s*``, ^l, ^a> = r e s

    /// Evaluate a computation with the given initial state and environment,
    /// returning the final value and output, discarding the final state.
    let inline evalRWS (e: ^e) (s: ^s) (RWS r) : ^l * ^a  = match r e s with m -> m.Read, m.Value

    /// Evaluate a computation with the given initial state and environment,
    /// returning the final state and output, discarding the final value.
    let inline execRWS (e: ^e) (s: ^s) (RWS r) : ^``s*`` * ^l = match r e s with m -> m.State, m.Read

    /// Map the return value, final state, and output of a computation using the given function.
    let inline mapRWS (f: ^``s*`` -> ^l0 -> ^a0 -> RWSResult< ^``s**``, ^l, ^a>) (RWS r) =
        RWS (fun (e: ^e) (s: ^s) -> match r e s with m -> f (m.State: ^``s*``) m.Read m.Value)

    /// Executes action with an initial environment and state modified by applying 'f'.
    let inline withRWS (f: ^e -> ^s -> ^e0 * ^s0) (RWS r) : RWS< ^e, ^s, ^``s*``, ^l, ^a> =
        RWS (fun e s -> match f e s with e', s' -> r e' s')

    /// Cache the results of a 'RWS' computation.
    let inline cacheRWS (RWS r) : RWS< ^e, ^s, ^``s*``, ^l, ^a> =
        let d = System.Collections.Concurrent.ConcurrentDictionary<(struct(^e * ^s)), RWSResult< ^``s*``, ^l, ^a>>(HashIdentity.Structural)
        let v = ref Unchecked.defaultof<RWSResult< ^``s*``, ^l, ^a>>
        RWS (fun e s ->
            let p = struct (e, s)
            if d.TryGetValue(p, v) then !v
            else d.GetOrAdd(key = p, value = r e s))


    /// Compositional operations on `RWS` values.
    module Compose =

        /// Lift a value onto an effectful context.
        let inline wrap x : RWS< ^e, ^s, ^s, ^l, ^a> = RWS (fun _ s ->
            { State = s ; Read = (^l: (static member Empty: unit -> ^l) ()) ; Value = x })

        /// Sequentially compose two effects, passing any value produced by the first
        /// as an argument to the second.
        let inline bind (k: ^a -> RWS< ^e, ^``s*``, ^``s**``, ^l, ^b>) (RWS r) = RWS (fun e s ->
            match r e (s: ^s) with
            | m -> match k m.Value with
                   | RWS r -> match r e m.State with
                              | m2 -> { m2 with Read = (^l: (static member Append: ^l -> ^l -> ^l) (m.Read, m2.Read)) })
        
        /// Removes one layer of monadic context from a nested monad.
        let inline flatten (mm: RWS< ^e, ^s, ^``s*``, ^l, RWS< ^e, ^``s*``, ^``s**``, ^l, ^a>>) = bind id mm

        /// Sequential application on effects.
        let inline ap (RWS rv) (RWS rf) : RWS< ^e, ^s, ^``s**``, ^l, ^b> = RWS (fun e s ->
            match rf e s with
            | f -> match rv e (f.State: ^``s*``) with
                   | v -> { State = v.State
                          ; Read = (^l: (static member Append: ^l -> ^l -> ^l) (f.Read, v.Read))
                          ; Value = f.Value v.Value })

        /// Lift a function onto effects.
        let inline map f (RWS r) : RWS< ^e, ^s, ^``s*``, ^l, ^b> = RWS (fun e s ->
            match r e s with
            | m -> { State = m.State ; Read = m.Read ; Value = f m.Value })


        /// Supplementary Monad operations on the given type.
        module Monad =

            /// Monadic computation builder specialised to the given monad.
            type RWSBuilder () =
                member inline s.Bind(m, k) = bind k m
                member inline s.Return x = wrap x
                member inline s.ReturnFrom m : RWS< ^e, ^s, ^``s*``, ^l, ^a> = m
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
            let inline composeM k2 k1 : ^a -> RWS< ^e, ^s, ^``s**``, ^l, ^c> =
                k1 >> bind k2
  
            /// Sequentially compose three actions, passing any value produced by the first
            /// two as arguments to the third.
            let inline bind2 (k: ^a -> ^b -> RWS< ^e, ^s2, ^s3, ^l, ^c>)
                (ma: RWS< ^e, ^s0, ^s1, ^l, ^a>)
                (mb: RWS< ^e, ^s1, ^s2, ^l, ^b>) =
                bind (fun a -> bind (k a) mb) ma

            /// Sequentially compose four actions, passing any value produced by the
            /// first two as arguments to the third.
            let inline bind3 (k: ^a -> ^b -> ^c -> RWS< ^e, ^s3, ^s4, ^l, ^c>)
                (ma: RWS< ^e, ^s0, ^s1, ^l, ^a>)
                (mb: RWS< ^e, ^s1, ^s2, ^l, ^b>)
                (mc: RWS< ^e, ^s2, ^s3, ^l, ^c>) =
                bind2 (fun a b -> bind (k a b) mc) ma mb

            /// Sequentially compose two actions, creating a third from the result and
            /// lifting a binary function on its effects.
            let inline bindMap (k: ^a -> RWS< ^e, ^s1, ^s2, ^l, ^b>) (f: ^a -> ^b -> ^c) m : RWS< ^e, ^s0, ^s2, ^l, ^c> =
                bind (fun a -> map (f a) (k a)) m

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to the intermediate result.
            /// The default monadic 'identity' function is used in each iteration where the continuation is applied.
            let inline recM f x : RWS< ^e, ^s, ^s, ^l, ^a> =
                let rec go m = bind (f (wrap >> go)) m in go (f wrap x)

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to an 'effect' applied over the intermediate result.
            /// Any constructor can be used in each iteration, in the case of union-types.
            let inline recMp f x : RWS< ^e, ^s, ^s, ^l, ^a> =
                let rec go m = bind (f go) m in go (f id x)                    

            /// <summary>Monadic fold over a structure associating to the right.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldrM (f: ^a -> ^z -> RWS< ^e, ^s, ^s, ^l, ^z>) (s0: ^z) (source: ^a seq)
                : RWS< ^e, ^s, ^s, ^l, ^z> =
                let g k x s = bind k (f x s)
                Seq.fold g wrap source s0

            /// <summary>Monadic fold over a structure associating to the left.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldlM (f: ^z -> ^a -> RWS< ^e, ^s, ^s, ^l, ^z>) (s0: ^z) (source: ^a seq)
                : RWS< ^e, ^s, ^s, ^l, ^z> =
                let g x k s = bind k (f s x)
                Seq.foldBack g source wrap s0        


            /// Monadic zipping (combining or decomposing corresponding monadic elements).
            module Zip =
            
                /// Combine the corresponding contents of two monads into a single monad.
                let inline mzipWith f (RWS ra) (RWS rb) : RWS< ^e, ^s, ^``s**``, ^l, ^c> = RWS (fun e s ->
                    match ra e s with
                    | a -> match rb e (a.State: ^``s*``) with
                           | b -> { State = b.State
                                  ; Read = (^l: (static member Append: ^l -> ^l -> ^l) (a.Read, b.Read))
                                  ; Value = f a.Value b.Value })

                /// Merge the contents (of corresponding pairs) of two monads into a monad of pairs.
                let inline mzip (RWS ra) (RWS rb) : RWS< ^e, ^s, ^``s**``, ^l, ^a * ^b> = RWS (fun e s ->
                    match ra e s with
                    | a -> match rb e (a.State: ^``s*``) with
                           | b -> { State = b.State
                                  ; Read = (^l: (static member Append: ^l -> ^l -> ^l) (a.Read, b.Read))
                                  ; Value = a.Value, b.Value })
                    
                /// Decompose a monad comprised of corresponding pairs of values.
                let inline munzip (RWS r) : RWS< ^e, ^s, ^``s*``, ^l, ^a> * RWS< ^e, ^s, ^``s*``, ^l, ^b> =
                    RWS (fun e s -> match r e s with
                                    | m -> { State = m.State ; Read = m.Read ; Value = fst (m.Value) }),
                    RWS (fun e s -> match r e s with
                                    | m -> { State = m.State ; Read = m.Read ; Value = snd (m.Value) })


        /// Supplementary Applicative operations on the given type.
        module Applicative =

            /// Lift a binary function on effects.
            let inline map2 f (RWS ra) (RWS rb) : RWS< ^e, ^s, ^``s**``, ^l, ^c> = RWS (fun e s ->
                match ra e s with
                | a -> match rb e (a.State: ^``s*``) with
                       | b -> { State = b.State
                              ; Read = (^l: (static member Append: ^l -> ^l -> ^l) (a.Read, b.Read))
                              ; Value = f a.Value b.Value })

            /// Lift a ternary function on effects.
            let inline map3 f (RWS ra) (RWS rb) (RWS rc) : RWS< ^e, ^s, ^``s***``, ^l, ^d> = RWS (fun e s ->
                let inline app a b = (^l: (static member Append: ^l -> ^l -> ^l) (a, b))
                match ra e s with
                | a -> match rb e (a.State: ^``s*``) with
                       | b -> match rc e (b.State: ^``s**``) with
                              | c -> { State = c.State
                                     ; Read = app a.Read (app b.Read c.Read)
                                     ; Value = f a.Value b.Value c.Value })

            /// Sequentially compose two effects, discarding any value produced by the first.
            let inline andThen fb fa : RWS< ^e, ^s, ^``s**``, ^l, ^b> =
                map2 (fun _ b -> b) (fa: RWS< ^e, ^s, ^``s*``, ^l, ^a>) fb

            /// Conditional execution of effectful expressions.
            let inline when_ (condition: bool) f : RWS< ^e, ^s, ^s, ^l, unit> =
                if condition then f () else wrap ()

            /// <summary>Generalizes the sequence-based filter function.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline filterA (p: ^a -> RWS< ^e, ^s, ^s, ^l, bool>) source =    
                Seq.foldBack (fun x xs -> map2 (fun flg xs -> if flg then x::xs else xs) (p x) xs) source (wrap [])

            /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline sequenceA (source: RWS< ^e, ^s, ^s, ^l, ^a> seq) : RWS< ^e, ^s, ^s, ^l, ^a list> =
                Seq.foldBack (map2 (fun x xs -> x::xs)) source (wrap [])

            /// <summary>Produce an effect for the elements in the sequence from left to right
            /// then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline forA f source : RWS< ^e, ^s, ^s, ^l, ^b list> =
                sequenceA (Seq.map f source)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right
            /// then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when either input sequence is null.</exception>
            let inline for2A f source1 source2 : RWS< ^e, ^s, ^s, ^l, ^c list> =
                forA ((<||) f) (Seq.allPairs source1 source2)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right,
            /// then evaluate each effect and collect the results.
            /// If one sequence is longer, its extra elements are ignored.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline zipWithA f source1 source2 : RWS< ^e, ^s, ^s, ^l, ^c list> =
                sequenceA (Seq.map2 f source1 source2)

            /// Performs the effect 'n' times.
            let inline replicateA (n: uint32) m : RWS< ^e, ^s, ^s, ^l, ^a list> =
                sequenceA (Seq.replicate (int n) m)


        /// Supplementary Functor operations on the given type.
        module Functor =

            /// Replace all locations in the input with the same value.
            let inline replace b fa : RWS< ^e, ^s, ^``s*``, ^l, ^b> = map (fun _ -> b) fa

            /// Perform an operation, store its result, perform an action using both
            /// the input and output, and finally return the output.
            let inline tee f (g: ^a -> ^b -> unit) fa : RWS< ^e, ^s, ^``s*``, ^l, ^b> =
                map (fun a -> let b = f a in g a b; b) fa


        /// A three paramater functor where the first, second, and third arguments are covariant.
        module Trifunctor =

            /// Map over all three arguments at the same time.
            let inline trimap (f: ^``s*`` -> ^``s**``) (g: ^l1 -> ^l2) (h: ^a -> ^b) (RWS r)
                : RWS< ^e, ^s, ^``s**``, ^l2, ^b> = RWS (fun e s ->
                match r e s with
                | m -> { State = f m.State
                        ; Read = g m.Read
                        ; Value = h m.Value })

            /// Map over both arguments at the same time.
            let inline bimap (f: ^l1 -> ^l2) (g: ^a -> ^b) (RWS r) : RWS< ^e, ^s, ^``s*``, ^l2, ^b> = RWS (fun e s ->
                match r e s with
                | m -> { State = m.State
                        ; Read = f m.Read
                        ; Value = g m.Value })

            /// Map covariantly over the first argument.
            let inline mapFst (f: ^``s*`` -> ^``s**``) (RWS r) : RWS< ^e, ^s, ^``s**``, ^l2, ^a> = RWS (fun e s ->
                match r e s with
                | m -> { State = f m.State
                        ; Read = m.Read
                        ; Value = m.Value })

            /// Map covariantly over the second argument.
            let inline mapSnd (g: ^l0 -> ^l1) (RWS r) : RWS< ^e, ^s, ^``s*``, ^l1, ^b> = RWS (fun e s ->
                match r e s with
                | m -> { State = m.State
                        ; Read = g m.Read
                        ; Value = m.Value })

            /// Map covariantly over the third argument.
            let inline mapThr (h: ^a -> ^b) (RWS r) : RWS< ^e, ^s, ^``s*``, ^l, ^b> = RWS (fun e s ->
                match r e s with
                | m -> { State = m.State
                        ; Read = m.Read
                        ; Value = h m.Value })


        /// Types with a binary, associative composition operation.
        module Semigroup =

            /// An associative composition operation.
            let inline sappend (RWS ra) (RWS rb) : RWS< ^e, ^s, ^``s**``, ^l, ^a> = RWS (fun e s ->
                match ra e s with
                | a -> match rb e (a.State: ^``s*``) with
                       | b -> { State = b.State
                              ; Read = (^l: (static member Append: ^l -> ^l -> ^l) (a.Read, b.Read))
                              ; Value = (^a: (static member Append: ^a -> ^a -> ^a) (a.Value, b.Value)) })


    /// Creates a computation expression for the given type.
    let rws = Compose.Monad.RWSBuilder ()



open RWS
open Compose
  
//  @ Operators @
type RWS<'e, 's, '``s*``, 'l, 'a> with

// @ Primitive @

    /// Unwrap an RWS computation as a function.
    static member inline ( >- ) (r, (e, s)) = runRWS e s r
    /// Unwrap an RWS computation as a function.
    static member inline ( -< ) ((e, s), r) = runRWS e s r

// @ Monad @

    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
    static member inline ( >>= ) (m, k) = bind k m
    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
    static member inline ( =<< ) (k, m) = bind k m

// @ Applicative @

    /// Sequential application on effects.
    static member inline ( <*> )  (ff, fx) = ap fx ff
    /// Sequential application on effects.
    static member inline ( <**> ) (fx, ff) = ap fx ff

    /// Sequentially compose two effects, discarding any value produced by the first.
    static member inline ( *> ) (fa, fb) = Applicative.andThen fb fa
    /// Sequentially compose two effects, discarding any value produced by the first.
    static member inline ( <* ) (fb, fa) = Applicative.andThen fb fa

// @ Functor @

    /// Lift a function onto effects.
    static member inline ( |>> ) (fa, f) = map f fa
    /// Lift a function onto effects.
    static member inline ( <<| ) (f, fa) = map f fa

    /// Replace all locations in the input with the same value.
    static member inline ( &> ) (b, fx) = Functor.replace b fx
    /// Replace all locations in the input with the same value.
    static member inline ( <& ) (fx, b) = Functor.replace b fx

// @ Semigroup @

    /// An associative composition operation.
    static member inline Append (e1, e2) = Semigroup.sappend e1 e2

    /// An associative composition operation.
    static member inline ( ++ ) (e1, e2) = Semigroup.sappend e1 e2