namespace Ptr.Context.Incomplete.Type.Sq


///// Wrapper type over sequences - representing non-determinism,
///// i.e. a structure containing 0 or more values.
//[<Struct>]
//type Sq<'a>(xs: ^a seq) =    
//    interface System.Collections.Generic.IEnumerable< ^a> with
//        override s.GetEnumerator() = s.ToSeq().GetEnumerator()
//        override s.GetEnumerator() = s.ToSeq().GetEnumerator() :> System.Collections.IEnumerator
//    /// Create a new Sq<'a>. Null input sequences are converted to 'Seq.empty'.
//    static member inline OfSeq (xs: ^a seq) = match xs with null -> Sq Seq.empty | _ -> Sq xs
//    /// Retrieve the inner sequence.
//    member __.ToSeq () = match xs with null -> Seq.empty | _ -> xs


///// Active patterns on `Sq` values.
//module Pattern =

//    /// Active pattern that can be used to extract the sequence within a `Sq`.
//    ///
//    /// Ex: let f (Sq xs) = ...   OR   match sq with Sq xs -> ...
//    let inline ( |Sq| ) (sq: Sq< ^a>) = Sq (sq.ToSeq ())


///// Standard operations on `Sq` values.
//module Std =

//    /// Apply a function onto the inner seq directly.
//    let inline onSeq (f: ^a seq -> ^b) (xs: Sq< ^a>) = f (xs.ToSeq())

//    /// Perform a seq-to-seq operation on the sequence within a Sq.
//    let inline reSeq (f: ^a seq -> #seq< ^b>) (xs: Sq< ^a>) = Sq (f (xs.ToSeq ()))

//    /// Caches the contents of the inner sequence.
//    let inline cacheSq (xs: Sq< ^a>) : Sq< ^a> = Sq (Seq.cache (xs.ToSeq ()))


///// Convert between values of type `Sq` and related types.
//module Convert =

//    /// Create a Sq from a regular seq. Nulls are converted to empty sequences.
//    let inline ofSeq (xs: ^a seq) = Sq< ^a>.OfSeq xs

//    /// Return the inner seq from the Sq wrapper.
//    let inline toSeq (xs: Sq< ^a>) : ^a seq = xs.ToSeq()


///// Compositional operations on `Sq` values.
//module Composition =

//        /// Lift a value onto an effectful context.
//        let inline wrap x = Sq (Seq.singleton x)

//        /// Sequentially compose two effects, passing any value produced by the first
//        /// as an argument to the second.
//        let inline bind f (xs: Sq< ^a>) : Sq< ^b> =
//            Sq (seq { for x in xs do match f x with (ys: Sq< ^b>) -> yield! ys })

//        /// Removes one layer of monadic context from a nested monad.
//        let inline flatten mm : Sq< ^a> = bind id mm

//        /// Sequential application on effects.
//        let inline ap (vs: Sq< ^a>) (fs: Sq< ^a -> ^b>) : Sq< ^b> =
//            let vs' : ^a seq = Seq.cache vs
//            Sq (seq { for f in fs do for v in vs' -> f v })

//        /// Lift a function onto effects.
//        let inline map (f: ^a -> ^b) (xs: Sq< ^a>) = Sq (Seq.map f xs)


//        /// Supplementary Monad operations on the given type.
//        module Monad =

//            /// Monadic computation builder specialised to the given monad.
//            type SqBuilder () =
//                member inline s.Bind(m, k) = bind k m
//                member inline s.Return x = wrap x
//                member inline s.ReturnFrom m : Sq< ^a> = m
//                member inline s.Zero () = s.Return ()
  
//                member inline s.Delay f = f ()
//                member inline s.Run f = f
  
//                member inline s.TryWith (body, handler) = try s.ReturnFrom(body ()) with e -> handler e
//                member inline s.TryFinally (body, finalizer) = try s.ReturnFrom(body ()) finally finalizer ()
  
//                member inline s.Using(disp: #System.IDisposable, body) =
//                    s.TryFinally((fun () -> body disp),
//                        fun () -> match box disp with null -> () | _ -> disp.Dispose ())
  
//                member inline s.While(guard, body) =
//                    let rec loop = function
//                    | false -> s.Zero ()
//                    | true -> s.Bind(body (), guard >> loop)
//                    loop (guard ())
  
//                member inline s.For(seq: _ seq, body) =
//                    s.Using(seq.GetEnumerator(),
//                        fun enum -> s.While(enum.MoveNext,
//                                        s.Delay(fun () -> body enum.Current)))


//            /// Composes two monadic functions together.
//            /// Acts as the composition function in the Kleisli category.
//            let inline composeM k2 (k1: ^a -> Sq< ^b>) : ^a -> Sq< ^c> = k1 >> bind k2
  
//            /// Sequentially compose three actions, passing any value produced by the first
//            /// two as arguments to the third.
//            let inline bind2 (k: ^a -> ^b -> Sq< ^c>) ma mb =
//                bind (fun a -> bind (k a) mb) ma

//            /// Sequentially compose four actions, passing any value produced by the
//            /// first two as arguments to the third.
//            let inline bind3 (k: ^a -> ^b -> ^c -> Sq< ^d>) ma mb mc =
//                bind2 (fun a b -> bind (k a b) mc) ma mb

//            /// Sequentially compose two actions, creating a third from the result and
//            /// lifting a binary function on its effects.
//            let inline bindMap (k: ^a -> Sq< ^b>) (f: ^a -> ^b -> ^c) m =
//                bind (fun a -> map (f a) (k a)) m

//            /// Build a monad through recursive (effectful) computations.
//            /// Computation proceeds through the use of a continuation function applied to the intermediate result.
//            /// The default monadic 'identity' function is used in each iteration where the continuation is applied.
//            let inline recM f x : Sq< ^a> =
//                let rec go m = bind (f (wrap >> go)) m in go (f wrap x)

//            /// Build a monad through recursive (effectful) computations.
//            /// Computation proceeds through the use of a continuation function applied to an 'effect' applied over the intermediate result.
//            /// Any constructor can be used in each iteration, in the case of union-types.
//            let inline recMp f x =
//                let rec go m = bind (f go) m in go (f id x)

//            /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
//            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//            let inline concatM (source: Sq< ^a> seq) : Sq< ^a seq> =
//                Sq (seq { for xs in source do yield xs :> ^a seq })

//            /// <summary>Produce an effect for the elements in the sequence from left to right
//            /// then evaluate each effect, and collect the results.</summary>
//            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//            let inline mapM f (source: ^a seq) : Sq< ^b seq> =
//                concatM (Seq.map f source)

//            /// <summary>Produce an effect for each pair of elements in the sequences from left to right
//            /// then evaluate each effect, and collect the results.</summary>
//            /// <exception cref="System.ArgumentNullException">Thrown when maybe input sequence is null.</exception>
//            let inline mapM2 f (source1: ^a seq) (source2: ^b seq) : Sq< ^c seq> =
//                mapM ((<||) f) (Seq.allPairs source1 source2)

//            /// <summary>Produce an effect for each pair of elements in the sequences from left to right,
//            /// then evaluate each effect and collect the results.
//            /// If one sequence is longer, its extra elements are ignored.</summary>
//            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//            let inline zipWithM f (source1: ^a seq) (source2: ^b seq) : Sq< ^c seq> =
//                concatM (Seq.map2 f source1 source2)            

//            /// <summary>Monadic fold over a structure associating to the right.</summary>
//            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//            let inline foldrM (f: ^a -> ^s -> Sq< ^s>) s0 source : Sq< ^s> =
//                let g k x s = bind k (f x s)
//                Seq.fold g wrap source s0

//            /// <summary>Monadic fold over a structure associating to the left.</summary>
//            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//            let inline foldlM (f: ^s -> ^a -> Sq< ^s>) s0 source : Sq< ^s> =
//                let g x k s = bind k (f s x)
//                Seq.foldBack g source wrap s0

//            /// Performs the effect 'n' times.
//            let inline replicateM (n: uint32) (xs: Sq< ^a>) : Sq< ^a seq> =
//                Sq (Seq.replicate (int n) (Convert.toSeq xs))


//            /// Monads that also support choice and failure.
//            module Plus =

//                /// The identity of mplus.
//                let inline mzero<'a> : Sq<'a> = Sq Seq.empty

//                /// A monoidal operation on monads, supporting choice and failure.
//                let inline mplus (xs: Sq< ^a>) (ys: Sq< ^a>) : Sq< ^a> =
//                    if   Seq.isEmpty xs then Sq ys
//                    elif Seq.isEmpty ys then Sq xs
//                    else Sq (Seq.append xs ys)

//                /// Conditional blocking of effectful computations.
//                let inline guard condition = if condition then Sq (Seq.singleton ()) else mzero

//                /// Create a new item if the previous was mzero, else keep the original.
//                let inline recover makeNew (xs: Sq< ^a>) : Sq< ^a> =
//                    if Seq.isEmpty xs then makeNew () else xs

//                /// Combine two monads using a 'SQL style' inner join function.
//                let inline relate f (k1: ^a -> ^k) (k2: ^b -> ^k) (ma: Sq< ^a>) (mb: Sq< ^b>) : Sq< ^c> =
//                    Sq (seq { for x in ma do
//                              for y in mb do if k1 x = k2 y then yield f x y })


//                /// Generalizations of functions on other types.
//                module General =

//                    /// <summary>Generalizes list concatenation to monads.</summary>
//                    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//                    let inline msum source =
//                        Seq.foldBack mplus source mzero

//                    /// <summary>Generalizes the 'ofSeq' function.</summary>
//                    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//                    let inline mOfSeq (xs: ^a seq) =
//                        msum (Seq.map wrap xs)

//                    /// Generalizes the 'Seq.where' function.
//                    let inline mwhere p m =
//                        bind (fun a -> if p a then m else mzero) m

//                    /// Opposite of the 'mwhere' function.
//                    let inline mremove p m =
//                        mwhere (not << p) m

//                    /// Generalizes the 'Seq.partition' function.
//                    let inline mpartition p m =
//                        mwhere p m, mremove p m

//                    /// Translate a form of Option.defaultWith to an arbitrary 'MonadPlus' type.
//                    let inline mofOption m =
//                        let inline ofOption b f m = match m with None -> b | Some a -> f a
//                        ofOption mzero wrap m

//                    /// Translate a form of 'Option.defaultWith' to an arbitrary 'MonadPlus' type.
//                    let inline mconcatOption m =
//                        let ofOption b f m = match m with None -> b | Some a -> f a
//                        bind (ofOption mzero wrap) m

//                    /// Generalizes the 'Seq.choose' function.
//                    let inline mchoose (f: ^a -> ^b option) m =
//                        mconcatOption (map f m)

//                    /// Collects the values from Choice1Of2's, while discarding the rest.
//                    let inline mchoice1 m =
//                        let l = function Choice1Of2 a -> Some a | Choice2Of2 _ -> None
//                        mconcatOption (map l m)
                            
//                    /// Collects the values from Choice2Of2's, while discarding the rest.
//                    let inline mchoice2 m =
//                        let r = function Choice2Of2 a -> Some a | Choice1Of2 _ -> None
//                        mconcatOption (map r m)

//                    /// Collects the values from Choice1Of2s on the left, and from Choice2Of2s on the right.
//                    let inline mpartitionChoice m =
//                        mchoice1 m, mchoice2 m


//            /// Monadic zipping (combining or decomposing corresponding monadic elements).
//            module Zip =
                
//                /// Combine the corresponding contents of two monads into a single monad.
//                let inline mzipWith f (xs: Sq< ^a>) (ys: Sq< ^b>) : Sq< ^c> = Sq (Seq.map2 f xs ys)

//                /// Merge the contents (of corresponding pairs) of two monads into a monad of pairs.
//                let inline mzip (xs: Sq< ^a>) (ys: Sq< ^b>) : Sq< ^a * ^b> = Sq (Seq.zip xs ys)

//                /// Decompose a monad comprised of corresponding pairs of values.
//                let inline munzip (ps: Sq< ^a * ^b>) : Sq< ^a> * Sq< ^b> =
//                    Sq (Seq.map fst ps), Sq (Seq.map snd ps)


//        /// Creates a monadic workflow for the given type.
//        let sq = Monad.SqBuilder ()


//        /// Supplementary Applicative operations on the given type.
//        module Applicative =            

//            /// Lift a binary function on effects.
//            let inline map2 (f: ^a -> ^b -> ^c) (xs: Sq< ^a>) (ys: Sq< ^b>) =
//                let ys' = Seq.cache ys
//                Sq (seq { for x in xs do
//                          for y in ys' do
//                            yield f x y })

//            /// Lift a ternary function on effects.
//            let inline map3 (f: ^a -> ^b -> ^c -> ^d) (xs: Sq< ^a>) (ys: Sq< ^b>) (zs: Sq< ^c>) =
//                Sq (seq { for x in xs  do
//                          for y in ys do
//                          for z in zs do
//                            yield f x y z })

//            /// Sequentially compose two effects, discarding any value produced by the first.
//            let inline andThen ys xs : Sq< ^b> =
//                map2 (fun _ y -> y) xs ys

//            /// Conditional execution of effectful expressions.
//            let inline when_ (condition: bool) f : Sq<unit> =
//                if condition then f () else wrap ()

//            /// <summary>Generalizes the sequence-based filter function.</summary>
//            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//            let inline filterA p source =
//                Seq.foldBack (fun x xs -> map2 (fun flg xs -> if flg then x::xs else xs) (p x) xs) source (wrap [])


//            /// A monoid on applicative functors.
//            module Alternative =

//                /// The identity of orElse.
//                let inline empty<'a> : Sq<'a> = Sq Seq.empty

//                /// An associative binary operation on applicative functors.
//                let inline orElse choice2 choice1 = Monad.Plus.mplus choice1 choice2               

//                /// <summary>The sum of a collection of effects.</summary>
//                /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//                let inline asum t_fa = do failwith "need Applicative version as it IS diff from Monad"
//                    //Monad.Plus.General.msum t_fa

//                /// Return one or none results on effects.
//                let inline optional fa : Sq< ^a option> =
//                    orElse (wrap None) (map Some fa)

//                /// Create a new item if the previous was empty, else keep the original.
//                let inline alt def (xs: Sq< ^a>) : Sq< ^a> =
//                    if Seq.isEmpty xs then def () else xs                


//        /// Supplementary Functor operations on the given type.
//        module Functor =

//            /// Replace all locations in the input with the same value.
//            let inline replace b (xs: Sq< ^a>) : Sq< ^b> =
//                Sq (Seq.map (fun _ -> b) xs)

//            /// Perform an operation, store its result, perform an action using both
//            /// the input and output, and finally return the output.
//            let inline tee f (g: ^a -> ^b -> unit) (fa: Sq< ^a>) : Sq< ^b> =
//                Sq (Seq.map (fun a -> let b = f a in g a b; b) fa)


//        /// Types with a binary, associative composition operation.
//        module Semigroup =

//            /// An associative composition operation.
//            let inline sappend (xs: Sq< ^e>) (ys: Sq< ^e>) : Sq< ^e> =
//                Sq (match xs.ToSeq() with
//                    | xs when Seq.isEmpty xs -> ys.ToSeq()
//                    | xs -> match ys.ToSeq() with
//                            | ys when Seq.isEmpty ys -> xs
//                            | ys -> Seq.append xs ys)


//        /// Types with a binary, associative composition operation and an identity element.
//        module Monoid =

//            /// An associative composition operation.
//            let inline mappend e1 e2 = Semigroup.sappend e1 e2

//            /// The identity element for the composition operator.
//            let inline mempty<'a> : Sq<'a> = Sq Seq.empty

//            /// Repeat a value 'n' times.
//            let inline mtimes (n: uint32) e =
//                let rec go acc = function
//                | 0u -> mempty
//                | 1u -> acc
//                | n  -> go (Semigroup.sappend e acc) (n - 1u)
//                go e n

//            /// <summary>Combine elements of a sequence using monoidal composition.</summary>
//            /// <exception cref="System.ArgumentNullException"> Thrown when the input sequence is null.</exception>
//            let inline mconcat source =
//                Seq.foldBack Semigroup.sappend source mempty
        


//open Std
//open Composition
  
////  @ Operators @
//type Sq<'a> with

//// @ Primitive @

//    /// Apply a function onto the inner seq directly.
//    static member inline ( >- ) (m, f) = onSeq f m
//    /// Apply a function onto the inner seq directly.
//    static member inline ( -< ) (f, m) = onSeq f m

//// @ Monad @

//    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
//    static member inline ( >>= ) (m, k) = bind k m
//    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
//    static member inline ( =<< ) (k, m) = bind k m

//// @ Applicative @

//    /// Sequential application on effects.
//    static member inline ( <*> )  (ff, fx) = ap fx ff
//    /// Sequential application on effects.
//    static member inline ( <**> ) (fx, ff) = ap fx ff

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
//    static member inline ( |>> ) (fa, f) = map f fa
//    /// Lift a function onto effects.
//    static member inline ( <<| ) (f, fa) = map f fa

//    /// Replace all locations in the input with the same value.
//    static member inline ( &> ) (b, fx) = Functor.replace b fx
//    /// Replace all locations in the input with the same value.
//    static member inline ( <& ) (fx, b) = Functor.replace b fx

//// @ Semigroup @

//    /// An associative composition operation.
//    static member inline Append (e1, e2) = Semigroup.sappend e1 e2

//    /// An associative composition operation.
//    static member inline ( ++ ) (e1, e2) = Semigroup.sappend e1 e2

//// @ Monoid @

//    /// The identity element for the composition operator.
//    static member inline Empty () = Monoid.mempty