namespace Ptr.Context.Type


/// List constructor used in the `NonEmpty` sequence type.
[<Struct; NoComparison; NoEquality>]
type UnitList<'a> = ULNil | ULCons of (unit -> NonEmpty< ^a>)


/// Represents a sequence that has at least one element.
and [<Struct; NoComparison; NoEquality>] NonEmpty<'a> = { Head: ^a ; Tail: ^a UnitList }
with interface System.Collections.Generic.IEnumerable< ^a> with
        override s.GetEnumerator() =
            let rec go t = seq {
                yield t.Head
                match t.Tail with
                | ULNil -> ()
                | ULCons t -> yield! go (t ()) } in (go s).GetEnumerator()
        override s.GetEnumerator() = (s :> _ seq).GetEnumerator() :> System.Collections.IEnumerator


/// Operations on `NonEmpty` values.
module NonEmpty =

    /// Active patterns on `NonEmpty` values.
    module Pattern =
    
        /// Returns either the Head or Head/Tail pair of a `NonEmpty` sequence.
        let inline ( |Head|Cons| ) (nel: NonEmpty< ^a>) =
            match nel.Tail with
            | ULNil    -> Head nel.Head
            | ULCons t -> Cons (struct (nel.Head, t))


    open Pattern    

    /// Convert a sequence into a `NonEmpty` collection.
    let inline fromSeq head (tail: ^a seq) : NonEmpty< ^a> =
        match tail with
        | null -> { NonEmpty.Head = head ; Tail = ULNil }
        | _ -> Seq.foldBack
                (fun x s -> { s with Tail = ULCons (fun () -> { s with Head = x })})
                tail
                { NonEmpty.Head = head ; Tail = ULNil }

    /// Map a function across the elements of a `NonEmpty` sequence.
    let inline mapNonEmpty (f: ^a -> ^b) (fa: NonEmpty< ^a>) : NonEmpty< ^b> =
        let rec go = function
        | Head a -> { NonEmpty.Head = f a ; Tail = ULNil }
        | Cons (a, t) -> { NonEmpty.Head = f a ; Tail = ULCons (fun () -> go (t ())) }
        go fa

    /// Create a singleton `NonEmpty`.
    let singleton x = { NonEmpty.Head = x ; Tail = ULNil }

    /// Add an item to the front of a `NonEmpty` sequence. / O(1) /
    let cons x (xs: NonEmpty<'a>) =
        { NonEmpty.Head = x ; Tail = ULCons (fun () -> xs) }

    /// Add an item to the end of a `NonEmpty` sequence. / O(n) /
    let inline snoc x (xs: ^a NonEmpty) =
        let rec go = function
        | Head a -> { NonEmpty.Head = a ; Tail = ULCons (fun () -> singleton x) }
        | Cons (a, t) -> { NonEmpty.Head = a ; Tail = ULCons (fun () -> go (t ())) }
        go xs

    /// Append two `NonEmpty` sequences together. / O(n) /
    let inline append (xs: NonEmpty< ^a>) (ys: NonEmpty< ^a>) =
        let rec go = function
        | Head a -> { NonEmpty.Head = a ; Tail = ULCons (fun () -> ys) }
        | Cons (a, t) -> { NonEmpty.Head = a ; Tail = ULCons (fun () -> go (t ())) }
        go xs

    /// Applies a function to each element of a `NonEmpty` sequence, threading an accumulator through each iteration.
    let inline reduce f (xs: NonEmpty< ^a>) = Seq.reduce f xs


    /// Compositional operations on `NonEmpty` values.
    module Compose =        

        /// Supplementary Monad operations on the given type.
        module Monad =

            /// Lift a value onto an effectful context.
            let inline wrap x : NonEmpty< ^a> = { NonEmpty.Head = x ; Tail = ULNil }

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
                let g k x s = bind k (f x s)
                Seq.fold g wrap source s0

            /// <summary>Monadic fold over a structure associating to the left.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldlM f (s0: ^s) (source: ^a seq) : NonEmpty< ^s> =
                let g x k s = bind k (f s x)
                Seq.foldBack (fun x k s -> bind k (f s x)) source wrap s0     


        /// Supplementary Applicative operations on the given type.
        module Applicative =

            /// Lift a value onto an effectful context.
            let inline wrap x : NonEmpty< ^a> = { NonEmpty.Head = x ; Tail = ULNil }

            /// Sequential application on effects.
            let inline ap mv mf = Monad.bind (fun f -> mapNonEmpty f mv) mf

            /// Lift a binary function on effects.
            let inline map2 (f: ^a -> ^b -> ^c) (fa: NonEmpty< ^a>) (fb: NonEmpty< ^b>) : NonEmpty< ^c> =
                ap fb (mapNonEmpty f fa)

            /// Lift a ternary function on effects.
            let inline map3 (f: ^a -> ^b -> ^c -> ^d) fa fb fc =
                ap fc (map2 f fa fb)

            /// Sequentially compose two effects, discarding any value produced by the first.
            let inline andThen fb fa : NonEmpty< ^b> =
                map2 (fun _  b -> b) fa fb

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
            let inline replicateA n fa =
                sequenceA (Seq.replicate (max 0 n) fa)


        /// Supplementary Functor operations on the given type.
        module Functor =

            /// Lift a function onto effects.
            let inline map (f: ^a -> ^b) (fa: NonEmpty< ^a>) : NonEmpty< ^b> =
                let rec go = function
                | Head a -> { NonEmpty.Head = f a ; Tail = ULNil }
                | Cons (a, t) -> { NonEmpty.Head = f a ; Tail = ULCons (fun () -> go (t ())) }
                go fa

            /// Replace all locations in the input with the same value.
            let replace (b: 'b) fa =
                let rec go = function
                | Head _ -> { NonEmpty.Head = b ; Tail = ULNil }
                | Cons (_, t) -> { NonEmpty.Head = b ; Tail = ULCons (fun () -> go (t ())) }
                go fa

            /// Perform an operation, store its result, perform an action using both
            /// the input and output, and finally return the output.
            let inline tee (f: ^a -> ^b) (g: ^a -> ^b -> unit) fa =
                let rec go = function
                | Head a -> let b = f a in g a b; { NonEmpty.Head = b ; Tail = ULNil }
                | Cons (a, t) -> let b = f a in g a b; { NonEmpty.Head = b ; Tail = ULCons (fun () -> go (t ())) }
                go fa


        /// Supplementary Comonad operations on the given type.
        module Comonad =

            /// Retrieve a value out of a context.
            let inline extract (w: NonEmpty< ^a>) = w.Head

            /// Sequentially compose two co-effects.
            let inline extend j (w: NonEmpty< ^a>) =
                let rec go = function
                | Head _ as w -> { NonEmpty.Head = j w ; Tail = ULNil }
                | Cons (_, t) as w -> { NonEmpty.Head = j w; Tail = ULCons (fun () -> go (t ())) }
                go w

            /// Takes a comonadic container and produces a container of containers.
            let inline duplicate w = extend id w

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
    static member inline ( |%> ) (m, f) = Functor.map f m
    /// Lift a function onto effects.
    static member inline ( <%| ) (f, m) = Functor.map f m

    /// Replace all locations in the input with the same value.
    static member inline ( %> ) (b, fa) = Functor.replace b fa
    /// Replace all locations in the input with the same value.
    static member inline ( <% ) (fa, b) = Functor.replace b fa

// @ Semigroup @

    /// An associative composition operation.
    static member inline Append (e1, e2) = Semigroup.sappend e1 e2

    /// An associative composition operation.
    static member inline ( ++ ) (e1, e2) = Semigroup.sappend e1 e2