namespace Ptr.Context.Type


/// A difference list is a function that, given a list, returns the original
/// contents of the difference list prepended to the given list.
///
/// This structure supports O(1) `append` and `snoc` operations, making this structure
/// more performant when used in certain, `append-heavy`, contexts.
[<Struct; NoComparison; NoEquality>]
type DList<'a> = DL of (^a list -> ^a list)
with interface System.Collections.Generic.IEnumerable< ^a> with
        override s.GetEnumerator() = match s with DL f -> (seq { for x in f [] -> x }).GetEnumerator()
        override s.GetEnumerator() = (s :> _ seq).GetEnumerator() :> System.Collections.IEnumerator


/// Operations on `DList` values.
module DList =

    /// The result of running a difference list when given an existing list to extend.
    let inline runDList xs (DL dl) = dl xs

    /// The result of running a difference list when given an existing list to extend.
    let inline runDList' (DL dl) = dl

    /// The list-generating function inside a DList.
    let inline fromDList (DL dl) = dl

    /// The result of running a difference list when given an empty list.
    let inline evalDList (DL dl) = dl []

    /// Create a dlist containing no elements.
    let inline empty<'a> : DList< ^a> = DL id

    /// Create a dlist with a single element.
    let inline singleton x = DL (fun xs -> x::xs)

    /// Prepend a single element to a dlist in O(1).
    let inline cons x (DL xs) = DL (xs >> fun ys -> x::ys)
        
    /// Append a single element to a dlist in O(1).
    let inline snoc x (DL xs) = DL (xs << fun ys -> x::ys)
      
    /// Append two dlists in O(1).
    let inline append (DL xs) (DL ys) = DL (xs << ys)
    
    /// <summary>Concatenate dlists in O(spine).</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    let inline concat (xs: DList< ^a> seq) =
        Seq.foldBack append xs empty

    /// Create a dlist of the given number of elements in O(n).
    let inline replicate (n: uint32) x = DL (List.append (List.replicate (int n) x))

    /// List elimination for dlists in O(n).
    let inline listElim (nil: ^b) consit (DL f) =
        match f [] with
        | [] -> nil
        | x::xs -> consit x (DL (List.append xs))
        
    /// Return the head of a dlist if it exists in O(n).
    let inline tryHead (DL f) =
        match f [] with
        | []   -> None
        | x::_ -> Some x

    /// Return the tail of a dlist if it exists in O(n).
    let inline tryTail (DL f) =
        match f [] with
        | []    -> None
        | _::xs -> Some xs

    /// Generate a dlist in O(n).
    let inline unfold g (s0: ^s) =
        DL (List.append (List.unfold g s0))   

    /// Caches a dlist.
    let inline cacheDList (DL f) =
        let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        DL (fun xs -> match d.TryGetValue(xs) with
                      | true, r -> r
                      | false, _ -> let r = f xs in d.[xs] <- r ; r)


    /// Convert between values of type `DList` and related types.
    module Convert =

        /// Convert a list to a dlist.
        let inline ofList xs = DL (List.append xs)

        /// Convert a dlist to a list, using the empty list as the initial list.
        let inline toList (DL dl) = dl []

        /// <summary>Convert a sequence to a dlist.</summary>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        let inline ofSeq xs = DL (List.append (Seq.toList xs))

        /// Convert a dlist to a sequence.
        let inline toSeq (DL xs) = seq { for x in xs [] -> x }


    /// Compositional operations on `DList` values.
    module Compose =

        /// Supplementary Monad operations on the given type.
        module Monad =

            /// Lift a value onto an effectful context.
            let inline wrap x = singleton x

            /// Sequentially compose two effects, passing any value produced by the first
            /// as an argument to the second.
            let inline bind (k: ^a -> DList< ^b>) (DL xs) : DList< ^b> =
                Seq.foldBack (append << k) (xs []) empty

            /// Removes one layer of monadic context from a nested monad.
            let inline flatten mm = bind id mm


            /// Monadic computation builder specialised to the given monad.
            type DListBuilder () =
                member inline s.Bind(m, k) = bind k m
                member inline s.Return x = wrap x
                member inline s.ReturnFrom m : DList< ^a> = m
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
            let inline composeM k2 k1 = k1 >> bind k2
  
            /// Sequentially compose three actions, passing any value produced by the first
            /// two as arguments to the third.
            let inline bind2 k ma mb = bind (fun a -> bind (k a) mb) ma

            /// Sequentially compose four actions, passing any value produced by the
            /// first two as arguments to the third.
            let inline bind3 k ma mb mc =
                bind2 (fun a b -> bind (k a b) mc) ma mb

            /// Sequentially compose two actions, creating a third from the result and
            /// lifting a binary function on its effects.
            let inline bindMap k f m = bind (fun a -> bind (f a >> wrap) (k a)) m

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to the intermediate result.
            /// The default monadic 'identity' function is used in each iteration where the continuation is applied.
            let inline recM f x =
                let rec go m = bind (f (wrap >> go)) m in go (f wrap x)

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to an 'effect' applied over the intermediate result.
            /// Any constructor can be used in each iteration, in the case of union-types.
            let inline recMp f x =
                let rec go m = bind (f go) m in go (f id x)
            
            /// <summary>Monadic fold over a structure associating to the right.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldrM f (s0: ^s) (source: ^a seq) =
                let inline g k x s = bind k (f x s)
                Seq.fold g wrap source s0

            /// <summary>Monadic fold over a structure associating to the left.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldlM f (s0: ^s) (source: ^a seq) =
                let inline g x k s = bind k (f s x)
                Seq.foldBack g source wrap s0


            /// Monads that also support choice and failure.
            module Plus =

                /// The identity of mplus.
                let inline mzero<'a> : DList<'a> = empty

                /// A monoidal operation on monads, supporting choice and failure.
                let inline mplus m1 m2 : DList< ^a> = append m1 m2

                /// Conditional failure of effectful computations.
                let inline guard condition = if condition then wrap () else mzero

                /// Create a new item if the previous was mzero, else keep the original.
                let inline recover makeNew (DL f) =
                    DL (List.append (match f [] with [] -> (match makeNew () with (DL f) -> f []) | xs -> xs))

                /// Combine two monads using a 'SQL style' inner join function.
                let inline relate f (k1: ^a -> ^k) (k2: ^b -> ^k) ma mb =
                    bind2 (fun a b -> if k1 a = k2 b then wrap (f a b) else mzero) ma mb


                /// Generalizations of functions on other types.
                module General =

                    /// <summary>Generalizes list concatenation to monads.</summary>
                    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
                    let inline msum source =
                        Seq.foldBack mplus source mzero

                    /// <summary>Generalizes the 'ofSeq' function.</summary>
                    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
                    let inline mOfSeq (xs: ^a seq) =
                        msum (Seq.map wrap xs)

                    /// Generalizes the 'Seq.where' function.
                    let inline mwhere p m =
                        bind (fun a -> if p a then m else mzero) m

                    /// Opposite of the 'mwhere' function.
                    let inline mremove p m =
                        mwhere (not << p) m

                    /// Generalizes the 'Seq.partition' function.
                    let inline mpartitionSeq p m =
                        mwhere p m, mremove p m

                    /// Translate a form of Option.defaultWith to an arbitrary 'MonadPlus' type.
                    let inline mofOption m =
                        let inline ofOption b f m = match m with None -> b | Some a -> f a
                        ofOption mzero wrap m

                    /// Translate a form of 'Option.defaultWith' to an arbitrary 'MonadPlus' type.
                    let inline mconcatOption m =
                        let inline ofOption b f m = match m with None -> b | Some a -> f a
                        bind (ofOption mzero wrap) m

                    /// Generalizes the 'Seq.choose' function.
                    let inline mchoose (f: ^a -> ^b option) m =
                        mconcatOption (bind (wrap << f) m)

                    /// Collects the values from Choice1Of2's, while discarding the rest.
                    let inline mchoice1 m =
                        let inline l c = match c with Choice1Of2 a -> Some a | Choice2Of2 _ -> None
                        mconcatOption (bind (wrap << l) m)
                        
                    /// Collects the values from Choice2Of2's, while discarding the rest.
                    let inline mchoice2 m =
                        let inline r c = match c with Choice2Of2 a -> Some a | Choice1Of2 _ -> None
                        mconcatOption (bind (wrap << r) m)

                    /// Collects the values from Choice1Of2s on the left, and from Choice2Of2s on the right.
                    let inline mpartitionChoice m =
                        mchoice1 m, mchoice2 m

        
            /// Monadic zipping (combining or decomposing corresponding monadic elements).
            module Zip =
            
                /// Combine the corresponding contents of two monads into a single monad.
                let inline mzipWith (f: ^a -> ^b -> ^c) (DL fa) (DL fb) : DList< ^c> =
                    DL (List.append (Seq.toList (Seq.map2 f (fa []) (fb []))))

                /// Merge the contents (of corresponding pairs) of two monads into a monad of pairs.
                let inline mzip (DL fa) (DL fb) =
                    DL (List.append (Seq.toList (Seq.map2 (fun a b -> a, b) (fa []) (fb []))))
                    
                /// Decompose a monad comprised of corresponding pairs of values.
                let inline munzip m : DList< ^a> * DList< ^b> =
                    let inline fmap f dl = bind (f >> wrap) dl
                    fmap fst m, fmap snd m        


        /// Supplementary Applicative operations on the given type.
        module Applicative =

            /// Lift a value onto an effectful context.
            let inline wrap x = singleton x

            /// Sequential application on effects.
            let inline ap (DL mv) (DL mf) =
                DL (List.append [ let vs = mv [] in for f in mf [] do for v in vs -> f v ])

            /// Lift a binary function on effects.
            let inline map2 (f: ^a -> ^b -> ^c) fa fb =
                Monad.bind2 (fun a b -> wrap (f a b)) fa fb

            /// Lift a ternary function on effects.
            let inline map3 (f: ^a -> ^b -> ^c -> ^d) fa fb fc =
                Monad.bind3 (fun a b c -> wrap (f a b c)) fa fb fc

            /// Sequentially compose two effects, discarding any value produced by the first.
            let inline andThen fb fa = map2 (fun _ b -> b) fa fb

            /// Conditional execution of effectful expressions.
            let inline when_ (condition: bool) f =
                if condition then f () else wrap ()

            /// <summary>Generalizes the sequence-based filter function.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline filterA (p: ^a -> DList<bool>) source =
                Seq.foldBack (fun x xs -> map2 (fun flg xs -> if flg then x::xs else xs) (p x) xs) source (wrap [])

            /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline sequenceA source : DList< ^a list> =
                Seq.foldBack (map2 (fun x xs -> x::xs)) source (wrap [])

            /// <summary>Produce an effect for the elements in the sequence from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline forA f (source: ^a seq) : DList< ^b list> =
                sequenceA (Seq.map f source)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when either input sequence is null.</exception>
            let inline for2A f (source1: ^a seq) (source2: ^b seq) : DList< ^c list> =
                forA ((<||) f) (Seq.allPairs source1 source2)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right,
            /// then evaluate each effect and collect the results.
            /// If one sequence is longer, its extra elements are ignored.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline zipWithA f (source1: ^a seq) (source2: ^b seq) : DList< ^c list> =
                sequenceA (Seq.map2 f source1 source2)

            /// Performs the effect 'n' times.
            let inline replicateA (n: uint32) fa : DList< ^a list> =
                sequenceA (Seq.replicate (int n) fa)


            /// A monoid on applicative functors.
            module Alternative =

                /// The identity of orElse.
                let inline empty<'a> : DList<'a> = empty

                /// An associative binary operation on applicative functors.
                let inline orElse choice2 choice1 = Monad.Plus.mplus choice1 choice2               

                /// Return one or none results on effects.
                let inline optional fa : DList< ^a option> =
                    orElse (wrap None) (ap fa (wrap Some))

                /// Create a new item if the previous was empty, else keep the original.
                let inline alt def (DL f) : DList< ^a> =
                    DL (List.append (match f [] with [] -> runDList [] (def ()) | xs -> xs))

                /// <summary>The sum of a collection of effects.</summary>
                /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
                let inline asum t_fa = Monad.Plus.General.msum t_fa


        /// Supplementary Functor operations on the given type.
        module Functor =

            /// Lift a function onto effects.
            let inline map (f: ^a -> ^b) (DL xs) =
                DL (List.append (List.map f (xs [])))

            /// Replace all locations in the input with the same value.
            let inline replace b fa = map (fun _ -> b) fa

            /// Perform an operation, store its result, perform an action using both
            /// the input and output, and finally return the output.
            let inline tee (f: ^a -> ^b) (g: ^a -> ^b -> unit) fa =
                map (fun a -> let b = f a in g a b; b) fa


        /// Types with a binary, associative composition operation.
        module Semigroup =

            /// An associative composition operation.
            let inline sappend e1 e2 =
                Applicative.map2 (fun a b -> (^a: (static member Append: ^a -> ^a -> ^a) (a, b))) e1 e2


        /// Types with a binary, associative composition operation and an identity element.
        module Monoid =

            /// An associative composition operation.
            let inline mappend e1 e2 = Semigroup.sappend e1 e2

            /// The identity element for the composition operator.
            let inline mempty<'a> : DList<'a> = empty
           
            /// Repeat a value 'n' times.
            let inline mtimes (n: uint32) e =
                let rec go acc = function
                | 0u -> mempty
                | 1u -> acc
                | n  -> go (Semigroup.sappend e acc) (n - 1u)
                go e n

            /// <summary>Combine elements of a sequence using monoidal composition.</summary>
            /// <exception cref="System.ArgumentNullException"> Thrown when the input sequence is null.</exception>
            let inline mconcat source =
                Seq.foldBack Semigroup.sappend source mempty    


    /// Creates a computation expression for the given type.
    let dlist = Compose.Monad.DListBuilder ()



open DList
open Compose
  
//  @ Operators @
type DList<'a> with

// @ Primitive @

    /// The result of running a difference list when given an existing list to extend.
    static member inline ( >- ) (mx, xs) = runDList xs mx
    /// The result of running a difference list when given an existing list to extend.
    static member inline ( -< ) (xs, mx) = runDList xs mx

// @ Monad @

    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
    static member inline ( >>= ) (mx, k) = Monad.bind k mx
    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
    static member inline ( =<< ) (k, mx) = Monad.bind k mx

// @ Applicative @

    /// Sequential application on effects.
    static member inline ( <*> )  (ff, fx) = Applicative.ap fx ff
    /// Sequential application on effects.
    static member inline ( <**> ) (fx, ff) = Applicative.ap fx ff

    /// Sequentially compose two effects, discarding any value produced by the first.
    static member inline ( *> ) (fa, fb) = Applicative.andThen fb fa
    /// Sequentially compose two effects, discarding any value produced by the first.
    static member inline ( <* ) (fb, fa) = Applicative.andThen fb fa

// @ Applicative.Alternative @

    /// An associative binary operation on applicative functors.
    static member inline ( <|> ) (c1, c2) = Applicative.Alternative.orElse c2 c1
    /// An associative binary operation on applicative functors.
    static member inline ( <||> ) (c2, c1) = Applicative.Alternative.orElse c2 c1

// @ Functor @

    /// Lift a function onto effects.
    static member inline ( |>> ) (fx, f) = Functor.map f fx
    /// Lift a function onto effects.
    static member inline ( <<| ) (f, fx) = Functor.map f fx

    /// Replace all locations in the input with the same value.
    static member inline ( %> ) (b, fx) = Functor.replace b fx
    /// Replace all locations in the input with the same value.
    static member inline ( <% ) (fx, b) = Functor.replace b fx

// @ Semigroup @

    /// An associative composition operation.
    static member inline Append (e1, e2) = Semigroup.sappend e1 e2

    /// An associative composition operation.
    static member inline ( ++ ) (e1, e2) = Semigroup.sappend e1 e2

// @ Monoid @

    /// The identity element for the composition operator.
    static member inline Empty () = Monoid.mempty