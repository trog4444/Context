namespace Ptr.Context.Type


/// List constructor used in the `NonEmpty` sequence type.
[<Struct; NoComparison; NoEquality>]
type UnitList<'a> = Nil | Cons of (unit -> NonEmpty< ^a>)


/// Represents a sequence that has at least one element.
and [<Struct; NoComparison; NoEquality>] NonEmpty<'a> = { Head: ^a ; Tail: ^a UnitList }
with interface System.Collections.Generic.IEnumerable< ^a> with
        override s.GetEnumerator() =
            let rec go t = seq {
                yield t.Head
                match t.Tail with
                | Nil -> ()
                | Cons t -> yield! go (t ()) } in (go s).GetEnumerator()
        override s.GetEnumerator() = (s :> _ seq).GetEnumerator() :> System.Collections.IEnumerator


/// Operations on `NonEmpty` values.
module NonEmpty =

    /// Active patterns on `NonEmpty` values.
    module Pattern =
    
        /// Returns either the Head or Head/Tail pair of a `NonEmpty` sequence.
        let inline ( |Head|Cons| ) (nel: NonEmpty< ^a>) =
            match nel.Tail with
            | Nil    -> Head nel.Head
            | Cons t -> Cons (struct (nel.Head, t))


    open Pattern    

    /// Convert a sequence into a `NonEmpty` collection.
    let inline fromSeq head (tail: ^a seq) : NonEmpty< ^a> =
        match tail with
        | null -> { Head = head ; Tail = Nil }
        | _ -> Seq.foldBack
                (fun x s -> { s with Tail = Cons (fun () -> { s with Head = x })})
                tail
                { Head = head ; Tail = Nil }

    /// Create a singleton `NonEmpty`.
    let inline singleton x = { Head = x ; Tail = Nil }

    /// Add an item to the front of a `NonEmpty` sequence. / O(1) /
    let inline cons x (xs: NonEmpty< ^a>) =
        { Head = x ; Tail = Cons (fun () -> xs) }

    /// Add an item to the end of a `NonEmpty` sequence. / O(n) /
    let inline snoc x (xs: ^a NonEmpty) =
        let rec go = function
        | Head a -> { Head = a ; Tail = Cons (fun () -> singleton x) }
        | Cons (a, t) -> { Head = a ; Tail = Cons (t >> go) }
        go xs

    /// Append two `NonEmpty` sequences together. / O(n) /
    let inline append (xs: NonEmpty< ^a>) (ys: NonEmpty< ^a>) =
        let rec go = function
        | Head a -> { Head = a ; Tail = Cons (fun () -> ys) }
        | Cons (a, t) -> { Head = a ; Tail = Cons (t >> go) }
        go xs

    /// Applies a function to each element of a `NonEmpty` sequence, threading an accumulator through each iteration.
    let inline reduce f (xs: NonEmpty< ^a>) = Seq.reduce f xs


    /// Compositional operations on `NonEmpty` values.
    module Compose =        

        /// Supplementary Monad operations on the given type.
        module Monad =

            /// Lift a value onto an effectful context.
            let inline wrap x : NonEmpty< ^a> = { Head = x ; Tail = Nil }

            /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
            let inline bind (k: ^a -> NonEmpty< ^b>) (m: NonEmpty< ^a>) =
                let xs = Seq.collect k m
                fromSeq (Seq.head xs) (Seq.tail xs)

            /// Removes one layer of monadic context from a nested monad.
            let inline flatten (mm: NonEmpty<NonEmpty< ^a>>) : NonEmpty< ^a> =
                let xs = Seq.concat mm
                fromSeq (Seq.head xs) (Seq.tail xs)


            /// Monadic computation builder specialised to the given monad.
            type NonEmptyBuilder () =
                member inline s.Bind(m, k) = bind k m
                member inline s.Return x = wrap x
                member inline s.ReturnFrom m : NonEmpty< ^a> = m
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
            let inline composeM k2 (k1: ^a -> NonEmpty< ^b>) : ^a -> NonEmpty< ^c> = k1 >> bind k2
  
            /// Sequentially compose three actions, passing any value produced by the first
            /// two as arguments to the third.
            let inline bind2 (k: ^a -> ^b -> NonEmpty< ^c>) ma mb =
                bind (fun a -> bind (k a) mb) ma

            /// Sequentially compose four actions, passing any value produced by the
            /// first two as arguments to the third.
            let inline bind3 (k: ^a -> ^b -> ^c -> NonEmpty< ^d>) ma mb mc =
                bind2 (fun a b -> bind (k a b) mc) ma mb

            /// Sequentially compose two actions, creating a third from the result and
            /// lifting a binary function on its effects.
            let inline bindMap (k: ^a -> NonEmpty< ^b>) (f: ^a -> ^b -> ^c) (m: NonEmpty< ^a>) =
                bind (fun a -> bind (f a >> wrap) (k a)) m

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to the intermediate result.
            /// The default monadic 'identity' function is used in each iteration where the continuation is applied.
            let inline recM f x : NonEmpty< ^a> =
                let rec go m = bind (f (wrap >> go)) m in go (f wrap x)

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to an 'effect' applied over the intermediate result.
            /// Any constructor can be used in each iteration, in the case of union-types.
            let inline recMp f x =
                let rec go m = bind (f go) m in go (f id x)        

            /// <summary>Monadic fold over a structure associating to the right.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldrM f (s0: ^s) (source: ^a seq) : NonEmpty< ^s> =
                let inline g k x s = bind k (f x s)
                Seq.fold g wrap source s0

            /// <summary>Monadic fold over a structure associating to the left.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldlM f (s0: ^s) (source: ^a seq) : NonEmpty< ^s> =
                let inline g x k s = bind k (f s x)
                Seq.foldBack g source wrap s0        


            /// Monadic zipping (combining or decomposing corresponding monadic elements).
            module Zip =

                /// Combine the corresponding contents of two monads into a single monad.
                let inline mzipWith (f: ^a -> ^b -> ^c) (ma: NonEmpty< ^a>) (mb: NonEmpty< ^b>) =
                    let rec go ba bb =
                        match ba with
                        | Head a -> match bb with
                                    | Head b -> { Head = f a b ; Tail = Nil }
                                    | Cons (b, _) -> { Head = f a b ; Tail = Nil }
                        | Cons (a, ta) ->
                            match bb with
                            | Head b -> { Head = f a b ; Tail = Nil }
                            | Cons (b, tb) -> { Head = f a b ; Tail = Cons (fun () -> go (ta ()) (tb ())) }
                    go ma mb

                /// Merge the contents (of corresponding pairs) of two monads into a monad of pairs.
                let inline mzip ma mb = mzipWith (fun a b -> a, b) ma mb
                    
                /// Decompose a monad comprised of corresponding pairs of values.
                let inline munzip (m: NonEmpty< ^a * ^b>) : NonEmpty< ^a> * NonEmpty< ^b> =
                    let rec go1 = function
                    | Head (a, _) -> { Head = a ; Tail = Nil }
                    | Cons ((a, _), t) -> { Head = a ; Tail = Cons (t >> go1) }
                    let rec go2 = function
                    | Head (_, b) -> { Head = b ; Tail = Nil }
                    | Cons ((_, b), t) -> { Head = b ; Tail = Cons (t >> go2) } 
                    go1 m, go2 m        


        /// Supplementary Functor operations on the given type.
        module Functor =

            /// Lift a function onto effects.
            let inline map (f: ^a -> ^b) (fa: NonEmpty< ^a>) : NonEmpty< ^b> =
                let rec go = function
                | Head a -> { Head = f a ; Tail = Nil }
                | Cons (a, t) -> { Head = f a ; Tail = Cons (t >> go) }
                go fa

            /// Replace all locations in the input with the same value.
            let inline replace (b: ^b) fa = map (fun _ -> b) fa

            /// Perform an operation, store its result, perform an action using both
            /// the input and output, and finally return the output.
            let inline tee (f: ^a -> ^b) (g: ^a -> ^b -> unit) fa =
                map (fun a -> let b = f a in g a b; b) fa


        /// Supplementary Applicative operations on the given type.
        module Applicative =

            /// Lift a value onto an effectful context.
            let inline wrap x : NonEmpty< ^a> = { Head = x ; Tail = Nil }

            /// Sequential application on effects.
            let inline ap mv mf = Monad.bind (fun f -> Functor.map f mv) mf

            /// Lift a binary function on effects.
            let inline map2 (f: ^a -> ^b -> ^c) (fa: NonEmpty< ^a>) (fb: NonEmpty< ^b>) : NonEmpty< ^c> =
                Monad.bind2 (fun a b -> wrap (f a b)) fa fb

            /// Lift a ternary function on effects.
            let inline map3 (f: ^a -> ^b -> ^c -> ^d) fa fb fc =
                Monad.bind3 (fun a b c -> wrap (f a b c)) fa fb fc

            /// Sequentially compose two effects, discarding any value produced by the first.
            let inline andThen fb fa : NonEmpty< ^b> =
                Monad.bind (fun _ -> fb) fa

            /// Conditional execution of effectful expressions.
            let inline when_ (condition: bool) f =
                if condition then f () else wrap ()

            /// <summary>Generalizes the sequence-based filter function.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline filterA p (source: ^a seq) =
                Seq.foldBack (fun x -> map2 (fun flg xs -> if flg then x::xs else xs) (p x)) source (wrap [])

            /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline sequenceA source : NonEmpty< ^a list> =
                Seq.foldBack (map2 (fun x xs -> x::xs)) source (wrap [])

            /// <summary>Produce an effect for the elements in the sequence from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline forA f (source: ^a seq) = sequenceA (Seq.map f source)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when either input sequence is null.</exception>
            let inline for2A f (source1: ^a seq) (source2: ^b seq) =
                forA ((<||) f) (Seq.allPairs source1 source2)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right, then evaluate each effect and collect the results.
            /// If one sequence is longer, its extra elements are ignored.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline zipWithA f (source1: ^a seq) (source2: ^b seq) =
                sequenceA (Seq.map2 f source1 source2) 

            /// Performs the effect 'n' times.
            let inline replicateA (n: uint32) fa =
                sequenceA (Seq.replicate (int n) fa)


        /// Supplementary Comonad operations on the given type.
        module Comonad =

            /// Retrieve a value out of a context.
            let inline extract (w: NonEmpty< ^a>) = w.Head

            /// Sequentially compose two co-effects.
            let inline extend j (w: NonEmpty< ^a>) =
                let rec go = function
                | Head _ as w -> { Head = j w ; Tail = Nil }
                | Cons (_, t) as w -> { Head = j w; Tail = Cons (t >> go) }
                go w

            /// Takes a comonadic container and produces a container of containers.
            let inline duplicate w = extend id w

            /// Composes two comonadic functions together. Acts as the composition function in the CoKleisli category.
            let inline composeW (f2: NonEmpty< ^b> -> ^c) f1 : NonEmpty< ^a> -> ^c = extend f1 >> f2

            /// Sequentially compose two co-actions, creating a third from the result and
            /// lifting a binary function on its effects.
            let inline extendMap j f w : NonEmpty< ^c> = Functor.map (fun a -> f a (j w)) w

            /// Deconstructs a comonad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function.
            let inline recW f w : ^a = let rec go w = f go w in go (extend (f extract) w)


        /// Types with a binary, associative composition operation.
        module Semigroup =

            /// An associative composition operation.
            let inline sappend e1 e2 =
                Applicative.map2 (fun a b -> (^a: (static member Append: ^a -> ^a -> ^a) (a, b))) e1 e2


    /// Creates a computation expression for the given type.
    let nonempty = Compose.Monad.NonEmptyBuilder()



open NonEmpty
open Compose
  
//  @ Operators @
type NonEmpty<'a> with

// @ Primitive @

    /// Applies a function to each element of a `NonEmpty` sequence, threading an accumulator through each iteration.
    static member inline ( >- ) (m, f) = reduce f m
    /// Applies a function to each element of a `NonEmpty` sequence, threading an accumulator through each iteration.
    static member inline ( -< ) (f, m) = reduce f m

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
    static member inline ( |>> ) (m, f) = Functor.map f m
    /// Lift a function onto effects.
    static member inline ( <<| ) (f, m) = Functor.map f m

    /// Replace all locations in the input with the same value.
    static member inline ( %> ) (b, fx) = Functor.replace b fx
    /// Replace all locations in the input with the same value.
    static member inline ( <% ) (fx, b) = Functor.replace b fx

// @ Semigroup @

    /// An associative composition operation.
    static member inline Append (e1, e2) = Semigroup.sappend e1 e2

    /// An associative composition operation.
    static member inline ( ++ ) (e1, e2) = Semigroup.sappend e1 e2