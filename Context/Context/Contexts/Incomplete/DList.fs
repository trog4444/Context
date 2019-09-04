namespace PTR.Context.Type.Incomplete


/// A difference list is a function that, given a list, returns the original
/// contents of the difference list prepended to the given list.
///
/// This structure supports O(1) `append` and `snoc` operations, making this structure
/// more performant when used in certain, `append-heavy`, contexts.
[<Struct; NoComparison; NoEquality>]
type DList<'T> = DL of (^T list -> ^T list)
with interface System.Collections.Generic.IEnumerable< ^T> with
        override s.GetEnumerator() = match s with DL f -> (seq { for x in f [] -> x }).GetEnumerator()
        override s.GetEnumerator() = (s :> _ seq).GetEnumerator() :> System.Collections.IEnumerator


/// Operations on `DList` values.
module DList =

    /// Return the list-producing function from a dList.
    let inline unDList (DL xs) : ^a list -> ^a list = xs

    /// Convert a list to a dlist.
    let inline fromList (list: ^a list) : ^a DList = DL (fun ys -> list @ ys)

    /// Convert a dlist to a list.
    let toList (DL (xs: 'a list -> ^a list)) : ^a list = xs [] 

    /// Apply a dlist to a list to get the underlying list with an extension.
    let applyDList (list: 'a list) (DL (xs: 'a list -> ^a list)) : ^a list = xs list

    /// Create a dlist containing no elements.
    let empty<'a> : ^a DList = DL id

    /// Create a dlist with a single element.
    let singleton (x: 'a) : ^a DList = DL (fun xs -> x::xs)

    /// Prepend a single element to a dlist. /O(1)/
    let inline cons (x: ^a) (DL xs) : ^a DList = DL (fun ys -> x::(xs ys))

    /// Append a single element to a dlist. /O(1)/
    let inline snoc (x: ^a) (DL xs) : ^a DList = DL (fun ys -> xs (x::ys))

    /// Append two dlists. /O(1)/
    let inline append ``1st`` ``2nd`` : ^a DList =
        DL (fun xs -> unDList ``1st`` (applyDList xs ``2nd``))

    /// <summary>Concatenate dlists. /O(spine)/</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    let inline concat (source: ^a DList seq) : ^a DList = Seq.foldBack append source empty

    /// Create a dlist of the given number of elements. /O(n)/
    let inline replicate count element =
        DL (fun xs -> List.replicate (max 0 count) element @ xs)

    /// List elimination for dlists.
    let inline listElim nil consit (DL xs) : ^b =
        match xs [] with
        | []    -> nil
        | x::xs -> consit x (fromList xs)

    /// If a dlist contains at least one element, return the first element, else return an empty dlist.
    let inline tryHead (DL xs) : ^a DList =
        DL (fun ys -> match xs [] with
                      | []   -> ys
                      | x::_ -> x::ys)

    /// If a dlist contains at least one element, return the tail, else return an empty dlist.
    let inline tryTail (DL xs) : ^a DList =
        DL (fun ys -> match xs [] with
                      | [] -> ys
                      | _::xs -> xs @ ys)
    
    let inline unfoldl g (seed: ^s) : ^a DList =
        let rec go acc = function
        | None -> acc
        | Some (a, s) -> go (snoc a acc) (g s)
        go empty (g seed)

    let inline unfold g (seed: ^s) : ^a DList = DL (fun xs -> List.unfold g seed @ xs)

    let inline unfoldBack g (seed: ^s) : ^a DList =
        let rec go acc = function
        | None -> acc
        | Some (a, s) -> go (cons a acc) (g s)
        go empty (g seed)    

    /// Left-associative fold over the elements of a dlist.
    let inline fold f (seed: ^s) (DL xs) : ^s = List.fold f seed (xs [])

    /// Right-associative fold over the elements of a dlist.
    let inline foldBack f (seed: ^s) (DL xs) : ^s = List.foldBack f (xs []) seed

    /// Map a function across the elements of a dlist.
    let inline mapDList (f: ^a -> ^b) (DL xs) : ^b DList =
        DL (fun ys -> List.map f (xs []) @ ys)

    /// Apply a 'list function' before applying the given dlist.
    let inline withDList f (DL xs) : ^a DList = DL (f >> xs)

    /// Caches a dlist.
    let inline cacheDList (DL f) =
        let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        DL (fun xs -> match d.TryGetValue(xs) with
                      | true, r  -> r
                      | false, _ -> let r = f xs in d.[xs] <- r ; r)


    /// Compositional operations on `DList` values.
    module Compose =

        /// Supplementary Monad operations on the given type.
        module Monad =

            /// Lift a value onto an effectful context.
            let wrap (x: 'a) : ^a DList = singleton x

            /// Sequentially compose two effects, passing any value produced by the first
            /// as an argument to the second.
            let inline bind (k: ^a -> DList< ^b>) (DL xs) : DList< ^b> =
                List.foldBack (fun a dl -> append (k a) dl) (xs []) empty

            /// Removes one layer of monadic context from a nested monad.
            let inline flatten (DL xxs) : ^a DList = List.foldBack append (xxs []) empty

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
            let inline recM f x =
                let rec go m = bind (f wrapgo) m
                and wrapgo x = go (wrap x)
                go (f wrap x)

            /// Build a monad through recursive (effectful) computations. Computation proceeds through the use of a continuation function applied to an 'effect' applied over the intermediate result.
            /// Any constructor can be used in each iteration, in the case of union-types.
            let inline recMp f x =
                let rec go m = bind (f go) m in go (f id x)
      
            /// <summary>Monadic fold over a structure associating to the right.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldrM f (s0: ^s) (source: ^a seq) =
                let g k x s = bind k (f x s)
                Seq.fold g wrap source s0

            /// <summary>Monadic fold over a structure associating to the left.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldlM f (s0: ^s) (source: ^a seq) =
                let g x k s = bind k (f s x)
                Seq.foldBack g source wrap s0


            /// Monads that also support choice and failure.
            module Plus =

                /// The identity of mplus.
                let mzero<'a> : DList< ^a> = empty< ^a>

                /// A monoidal operation on monads, supporting choice and failure.
                let inline mplus m1 m2 : DList< ^a> = append m1 m2

                /// Conditional failure of effectful computations.
                let guard condition = if condition then wrap () else empty<unit>

                /// Create a new item if the previous was mzero, else keep the original.
                let inline recover makeNew (DL f) =
                    DL (fun ys ->
                        match f [] with
                        | [] -> match makeNew () with (DL f) -> f [] @ ys
                        | xs -> xs @ ys)

                /// Combine two monads using a 'SQL style' inner join function.
                let inline relate f (k1: ^a -> ^k) (k2: ^b -> ^k) ma mb =
                    bind (fun a -> bind (fun b -> if k1 a = k2 b then wrap (f a b) else mzero) mb) ma


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
                        mwhere (fun x -> not (p x)) m

                    /// Generalizes the 'Seq.partition' function.
                    let inline mpartitionSeq p m =
                        mwhere p m, mremove p m

                    /// Translate a form of Option.defaultWith to an arbitrary 'MonadPlus' type.
                    let inline mofOption m =
                        let ofOption b f m = match m with None -> b | Some a -> f a
                        ofOption mzero wrap m

                    /// Translate a form of 'Option.defaultWith' to an arbitrary 'MonadPlus' type.
                    let inline mconcatOption m =
                        let ofOption b f m = match m with None -> b | Some a -> f a
                        bind (ofOption mzero wrap) m

                    /// Generalizes the 'Seq.choose' function.
                    let inline mchoose (f: ^a -> ^b option) m =
                        mconcatOption (bind (fun x -> wrap (f x)) m)

                    /// Collects the values from Choice1Of2's, while discarding the rest.
                    let inline mchoice1 m =
                        let l c = match c with Choice1Of2 a -> Some a | Choice2Of2 _ -> None
                        mconcatOption (bind (fun x -> wrap (l x)) m)
            
                    /// Collects the values from Choice2Of2's, while discarding the rest.
                    let inline mchoice2 m =
                        let r c = match c with Choice2Of2 a -> Some a | Choice1Of2 _ -> None
                        mconcatOption (bind (fun x -> wrap (r x)) m)

                    /// Collects the values from Choice1Of2s on the left, and from Choice2Of2s on the right.
                    let inline mpartitionChoice m =
                        mchoice1 m, mchoice2 m   


        /// Supplementary Applicative operations on the given type.
        module Applicative =

            /// Lift a value onto an effectful context.
            let wrap (x: 'a) : ^a DList = singleton x

            /// Sequential application on effects.
            let inline ap (DL mv) (DL mf) =
                DL (fun ys ->
                    let vs = mv []
                    [ for f in mf [] do for v in vs -> f v ] @ ys)

            /// Lift a binary function on effects.
            let inline map2 (f: ^a -> ^b -> ^c) fa fb =
                ap fb (mapDList f fa)

            /// Lift a ternary function on effects.
            let inline map3 (f: ^a -> ^b -> ^c -> ^d) fa fb fc =
                ap fc (map2 f fa fb)

            /// Sequentially compose two effects, discarding any value produced by the first.
            let inline andThen fb fa = map2 (fun _ b -> b) fa fb

            /// Conditional execution of effectful expressions.
            let inline when_ condition f =
                if condition then f () else wrap ()

            /// <summary>Generalizes the sequence-based filter function.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline filterA (p: ^a -> DList<bool>) source =
                Seq.foldBack (fun x -> map2 (fun flg xs -> if flg then x::xs else xs) (p x)) source (wrap [])

            /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline sequenceA source : DList< ^a list> =
                Seq.foldBack (map2 (fun x xs -> x::xs)) source (wrap [])

            /// <summary>Produce an effect for the elements in the sequence from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline forA f (source: ^a seq) : DList< ^b list> =
                sequenceA (Seq.map f source)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right, then evaluate each effect and collect the results.
            /// If one sequence is longer, its extra elements are ignored.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline zipWithA f (source1: ^a seq) (source2: ^b seq) : DList< ^c list> =
                sequenceA (Seq.map2 f source1 source2)

            /// Performs the effect 'n' times.
            let inline replicateA count fa : DList< ^a list> =
                sequenceA (Seq.replicate (max 0 count) fa)


            /// A monoid on applicative functors.
            module Alternative =

                /// The identity of orElse.
                let empty<'a> : DList< ^a> = empty

                /// An associative binary operation on applicative functors.
                let inline orElse choice2 choice1 = Monad.Plus.mplus choice1 choice2        

                /// Return one or none results on effects.
                let inline optional fa : DList< ^a option> =
                    orElse (wrap None) (ap fa (wrap Some))

                /// Create a new item if the previous was empty, else keep the original.
                let inline alt def (DL f) : DList< ^a> =
                    DL (fun ys -> (match f [] with [] -> toList (def ()) | xs -> xs) @ ys)

                /// <summary>The sum of a collection of effects.</summary>
                /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
                let inline asum t_fa = Monad.Plus.General.msum t_fa


        /// Supplementary Functor operations on the given type.
        module Functor =

            /// Lift a function onto effects.
            let inline map (f: ^a -> ^b) (DL xs) =
                DL (List.append (List.map f (xs [])))

            /// Replace all locations in the input with the same value.
            let inline replace (b: ^b) (DL xs) = DL (List.append (List.map (fun _ -> b) (xs [])))

            /// Perform an operation, store its result, perform an action using both
            /// the input and output, and finally return the output.
            let inline tee (f: ^a -> ^b) (g: ^a -> ^b -> unit) (DL xs) =
                let f a = let b = f a in g a b; b
                DL (List.append (List.map f (xs [])))


        /// Types with a binary, associative composition operation.
        module Semigroup =

            /// An associative composition operation.
            let inline sappend e1 e2 =
                Applicative.map2 (fun a b ->
                    (^a: (static member Append: ^a -> ^a -> ^a) (a, b))) e1 e2


        /// Types with a binary, associative composition operation and an identity element.
        module Monoid =

            /// An associative composition operation.
            let inline mappend e1 e2 = Semigroup.sappend e1 e2

            /// The identity element for the composition operator.
            let mempty<'a> : DList< ^a> = empty< ^a>
      
            /// Repeat a value 'n' times.
            let inline mtimes n e =
                let rec go acc = function
                | 0 -> mempty
                | 1 -> acc
                | n -> go (Semigroup.sappend e acc) (n - 1)
                go e (max 0 n)

            /// <summary>Combine elements of a sequence using monoidal composition.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline mconcat source =
                Seq.foldBack Semigroup.sappend source mempty  


    /// Creates a computation expression for the given type.
    let dlist = Compose.Monad.DListBuilder ()



open DList
open Compose
 
// @ Operators @
type DList<'T> with

// @ Primitive @

    /// Apply a dlist to a list to get the underlying list with an extension.
    static member inline ( >- ) (dl, list) = applyDList list dl
    /// Apply a dlist to a list to get the underlying list with an extension.
    static member inline ( -< ) (list, dl) = applyDList list dl

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

// @ Applicative.Alternative @

    /// An associative binary operation on applicative functors.
    static member inline ( <|> ) (c1, c2) = Applicative.Alternative.orElse c2 c1
    /// An associative binary operation on applicative functors.
    static member inline ( <||> ) (c2, c1) = Applicative.Alternative.orElse c2 c1

// @ Functor @

    /// Lift a function onto effects.
    static member inline ( |%> ) (fa, f) = Functor.map f fa
    /// Lift a function onto effects.
    static member inline ( <%| ) (f, fa) = Functor.map f fa

    /// Replace all locations in the input with the same value.
    static member inline ( %> ) (fa, b) = Functor.replace b fa
    /// Replace all locations in the input with the same value.
    static member inline ( <% ) (b, fa) = Functor.replace b fa

// @ Semigroup @

    /// An associative composition operation.
    static member inline Append (e1, e2) = Semigroup.sappend e1 e2

// @ Monoid @

    /// The identity element for the composition operator.
    static member inline Empty () = Monoid.mempty
