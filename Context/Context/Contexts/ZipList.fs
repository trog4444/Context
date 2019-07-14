namespace Ptr.Context.Type


/// Sequences, but with an Applicative functor based on `zipping` rather than `non-determinism`.
/// Note: the Applicative 'wrap' (aka pure, return, etc) function yields an infinite sequence,
/// so care must be taken when consuming `ZipLists` created via 'wrap'.
[<Struct; NoComparison>]
type ZipList<'a> = internal ZL of ^a seq
with interface System.Collections.Generic.IEnumerable< ^a> with
        override s.GetEnumerator() = (s.ToSeq()).GetEnumerator()
        override s.GetEnumerator() = (s.ToSeq()).GetEnumerator() :> System.Collections.IEnumerator
     member s.ToSeq() = match s with ZL xs -> if isNull xs then Seq.empty else xs
     static member OfSeq (xs: #seq<_>) = ZL xs


/// Standard operations on `ZipList` values.
module ZipList =

    /// Active patterns on `ZipList` values.
    module Pattern =
    
        /// Return the sequence within a `ZipList`. Null sequences are returned as empty sequences instead.
        let inline ( |ZipList| ) (zl: ZipList<_>) = ZipList (zl.ToSeq())


    open Pattern

    /// Create a `ZipList` from a sequence.
    let fromSeq (xs: #seq<_>) = ZipList<_>.OfSeq xs

    /// Initialize a `ZipList` of length `count`.
    let inline init (count: int) initializer =
        ZipList<_>.OfSeq (seq { for i = 1 to max 0 count do yield initializer i })

    /// Initialize a `ZipList` of length `count` using a state-transforming function.
    let inline initWith (count: int) (initialState: ^s) initializer =
        ZipList<_>.OfSeq
            (seq { let mutable s0 = initialState
                   for i = 1 to max 0 count do
                       let (s, x) = initializer i s0
                       s0 <- s
                       yield x })

    /// Initialize a `ZipList` of length `coutn`.
    let inline initLong (count: int64) initializer =
        ZipList<_>.OfSeq (seq { for i = 1L to max 0L count do yield initializer i })

    /// Create a `ZipList` using the given generator function.
    let inline unfold generator seed = ZipList<_>.OfSeq (Seq.unfold generator seed)

    /// Take up to `n` items from a `ZipList`. If `n` exceeds the length of the sequence
    /// then the entire sequence is returned.
    let take (count: int) (ZipList xs) = Seq.truncate (max 0 count) xs

    /// Take elements from a `ZipList` until the given predicate returns false.
    let inline takeWhile predicate (ZipList xs) = Seq.takeWhile predicate xs

    /// Returns true if the sequence is empty; false otherwise.
    let isEmpty (ZipList xs) = Seq.isEmpty xs

    /// Caches the inner sequence.
    let cacheZipList (ZipList xs) = ZipList<_>.OfSeq (Seq.cache xs)


    /// Compositional operations on `ZipList` values.
    module Compose =

        /// Supplementary Applicative operations on the given type.
        module Applicative =

            /// Lift a value onto an effectful context.
            let inline wrap x = ZipList<_>.OfSeq (seq { while true do yield x })

            /// Sequential application on effects.
            let inline ap (ZipList mv) (ZipList mf) = ZipList<_>.OfSeq (Seq.map2 (<|) mf mv)

            /// Lift a binary function on effects.
            let inline map2 f (ZipList xs) (ZipList ys) = ZipList<_>.OfSeq (Seq.map2 f xs ys)

            /// Lift a ternary function on effects.
            let inline map3 f (ZipList xs) (ZipList ys) (ZipList zs) = ZipList<_>.OfSeq (Seq.map3 f xs ys zs)

            /// Sequentially compose two effects, discarding any value produced by the first.
            let inline andThen fb fa = map2 (fun _ b -> b) fa fb

            /// Conditional execution of effectful expressions.
            let inline when_ condition f = if condition then f () else wrap ()

            /// <summary>Generalizes the sequence-based filter function.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline filterA p source =
                Seq.foldBack (fun x -> map2 (fun b xs -> if b then x::xs else xs) (p x)) source (wrap [])

            /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline sequenceA source =
                Seq.foldBack (map2 (fun x xs -> x::xs)) source (wrap [])

            /// <summary>Produce an effect for the elements in the sequence from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline forA f source = sequenceA (Seq.map f source)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when either input sequence is null.</exception>
            let inline for2A f source1 source2 = forA ((<||) f) (Seq.zip source1 source2)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right, then evaluate each effect and collect the results.
            /// If one sequence is longer, its extra elements are ignored.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline zipWithA f source1 source2 = sequenceA (Seq.map2 f source1 source2)

            /// Performs the effect 'n' times.
            let inline replicateA n (ZipList xs) =
                ZipList<_>.OfSeq (Seq.replicate (max 0 n) xs)


            /// A monoid on applicative functors.
            module Alternative =

                /// The identity of orElse.
                let empty<'a> : ZipList< ^a> = ZipList<_>.OfSeq Seq.empty

                /// An associative binary operation on applicative functors.
                let inline orElse choice2 choice1 = 
                    ZipList<_>.OfSeq
                        (seq { match choice1 with
                               | ZipList xs ->
                                yield! xs
                                match choice2 with
                                | ZipList ys -> let n = Seq.length xs
                                                if Seq.length ys > n then yield! Seq.skip n ys })

                /// <summary>The sum of a collection of effects.</summary>
                /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
                let inline asum t_fa : ZipList< ^a> =
                    Seq.foldBack (fun x s -> orElse s x) t_fa empty

                /// Return one or none results on effects.
                let inline optional fa = orElse (wrap None) (ap fa (wrap Some))

                /// Create a new item if the previous was empty, else keep the original.
                let inline alt def fx : ZipList< ^a> = if isEmpty fx then def () else fx


        /// Supplementary Functor operations on the given type.
        module Functor =

            /// Lift a function onto effects.
            let inline map f (ZipList xs) = ZipList<_>.OfSeq (Seq.map f xs)

            /// Replace all locations in the input with the same value.
            let replace (b: 'b) (ZipList xs) =
                ZipList<_>.OfSeq (Seq.map (fun _ -> b) xs)

            /// Perform an operation, store its result, perform an action using both
            /// the input and output, and finally return the output.
            let inline tee f g (ZipList xs) =
                ZipList<_>.OfSeq (Seq.map (fun a -> let b = f a in g a b; b) xs)


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
            let mempty<'a> : ZipList<'a> = ZL Seq.empty

            /// Repeat a value 'n' times.
            let inline mtimes (n: int) e =
                let rec go acc = function
                | 0 -> mempty
                | 1 -> acc
                | n  -> go (Semigroup.sappend e acc) (n - 1)
                go e (max 0 n)
        
            /// <summary>Combine elements of a sequence using monoidal composition.</summary>
            /// <exception cref="System.ArgumentNullException"> Thrown when the input sequence is null.</exception>
            let inline mconcat source =
                Seq.foldBack Semigroup.sappend source mempty



open ZipList
open Compose
  
//  @ Operators @
type ZipList<'a> with

// @ Primitive @

    /// Take elements from a `ZipList` until the given predicate returns false.
    static member inline ( >- ) (m, p) = takeWhile p m
    /// Take elements from a `ZipList` until the given predicate returns false.
    static member inline ( -< ) (p, m) = takeWhile p m

    /// Take up to `n` items from a `ZipList`. If `n` exceeds the length of the sequence
    /// then the entire sequence is returned.
    static member inline ( >- ) (m, n) = take n m
    /// Take up to `n` items from a `ZipList`. If `n` exceeds the length of the sequence
    /// then the entire sequence is returned.
    static member inline ( -< ) (n, m) = take n m


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

// @ Monoid @

    static member inline Empty () : ZipList< ^a> = Monoid.mempty