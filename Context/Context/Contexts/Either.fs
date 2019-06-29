namespace Ptr.Context.Type.Either


/// The Either type represents values with two possibilities:
/// a value of type Either a b is either `Left a` or `Right b`.
[<Struct>]
type Either<'l, 'r> = Left of L: ^l | Right of R: ^r
with interface System.Collections.Generic.IEnumerable< ^r> with
        override s.GetEnumerator() = (match s with Left _ -> Seq.empty | Right a -> Seq.singleton a).GetEnumerator()
        override s.GetEnumerator() = (s :> _ seq).GetEnumerator() :> System.Collections.IEnumerator


/// Standard operations on `Either` values.
module Std =
  
    /// Return True if the given value is a Left-value, False otherwise.
    let inline isLeft either = match either with Left _ -> true | Right _ -> false

    /// Return True if the given value is a Right-value, False otherwise.
    let inline isRight either = not (isLeft either)

    /// Apply one of the given functions to the value held within an Either, depending
    /// on if the value is 'Left' or 'Right.'
    let inline ofEither fRight fLeft (m: Either< ^a, ^b>) : ^c =
        match m with
        | Left e  -> fLeft e
        | Right a -> fRight a

    /// Return the contents of a Left-value or a default value otherwise.
    let inline fromLeft def either =
        match either with Left a -> a | Right _ -> def

    /// Return the contents of a Right-value or a default value otherwise.
    let inline fromRight def either =
        match either with Left _ -> def | Right b -> b

    /// <summary>Extracts from a sequence of Eithers all the Left elements in order.</summary>
    /// <exception cref="System.ArgumentNullException"> Thrown when the input sequence is null.</exception>
    let inline lefts eithers =
        seq { for e in eithers do match e with Left a -> yield a | Right _ -> () }

    /// <summary>Extracts from a sequence of Eithers all the Right elements in order.</summary>
    /// <exception cref="System.ArgumentNullException"> Thrown when the input sequence is null.</exception>
    let inline rights eithers =
        seq { for e in eithers do match e with Left _ -> () | Right b -> yield b }
    
    /// <summary>Partitions a list of Either into two lists. All the Left elements are extracted, in order,
    /// to the first component of the output. Similarly the Right elements are extracted to
    /// the second component of the output.</summary>
    /// <exception cref="System.ArgumentNullException"> Thrown when the input sequence is null.</exception>
    let inline partition eithers =
        match Seq.foldBack (fun e (struct (xs, ys)) ->
            match e with
            | Left  a -> struct (a::xs, ys)
            | Right b -> struct (xs, b::ys)) eithers (struct ([], [])) with struct (a, b) -> a, b

    /// Removes the Left-value type from an Either, carrying Right-value(s) into a Some; None otherwise.
    let inline hush either = match either with Left _ -> None | Right a -> Some a


/// Convert between values of type `Either` and related types.
module Convert =
        
        /// Returns a Right-value with the head of a non-empty sequence and
        /// returns a Left-value if the sequence is empty. The Left-value will
        /// say if the input was either null or empty.
        let inline ofSeq (source: ^a seq) =
            match source with
            | null -> Left "Input sequence was null."
            | _    -> if Seq.isEmpty source then Left "Input sequence was empty."
                      else Right (Seq.head source)

        /// Returns a singleton sequence if thet value is a Right-value, an empty sequence otherwise.
        let inline toSeq either = match either with Left _ -> Seq.empty | Right a -> Seq.singleton a       

        /// Convert a Choice (Of2) to an Either.
        let inline ofChoice choice =
            match choice with
            | Choice1Of2 a -> Right a
            | Choice2Of2 b -> Left b

        /// Convert an Either to a Choice (Of2).
        let inline toChoice either =
            match either with
            | Right a -> Choice1Of2 a
            | Left b  -> Choice2Of2 b

        /// Convert a Result to an Either.
        let inline ofResult result =
            match result with
            | Ok a    -> Right a
            | Error b -> Left b

        /// Convert an Either to a Result.
        let inline toResult result =
            match result with
            | Right a -> Ok a
            | Left b  -> Error b


/// Compositional operations on `Either` values.
module Composition =

        /// Lift a value onto an effectful context.
        let inline wrap x : Either< ^a, ^b> = Right x

        /// Sequentially compose two effects, passing any value produced by the first
        /// as an argument to the second.
        let inline bind (k: ^b -> Either< ^a, ^c>) m =
            match m with Left a -> Left a | Right b -> k b

        /// Removes one layer of monadic context from a nested monad.
        let inline flatten mm : Either< ^a, ^b> = bind id mm

        /// Sequential application on effects.
        let inline ap mv mf : Either< ^a, ^c> =
            match mf with
            | Left  a -> Left a
            | Right f -> match mv with
                         | Left  a -> Left a
                         | Right v -> Right (f v)

        /// Lift a function onto effects.
        let inline map (f: ^b -> ^c) m : Either< ^a, ^c> =
            match m with Left a -> Left a | Right b -> Right (f b)


        /// Supplementary Monad operations on the given type.
        module Monad =

            /// Monadic computation builder specialised to the given monad.
            type EitherBuilder () =
                member inline s.Bind(m, k) = bind k m
                member inline s.Return x = wrap x
                member inline s.ReturnFrom m : Either< ^a, ^b> = m
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
            let inline composeM k2 (k1: ^b -> Either< ^a, ^c>) : ^b -> Either< ^a, ^d> = k1 >> bind k2
  
            /// Sequentially compose three actions, passing any value produced by the first
            /// two as arguments to the third.
            let inline bind2 (k: ^b -> ^c -> Either< ^a, ^d>) mb mc =
                match mb with
                | Left  a -> Left a
                | Right b -> match mc with
                             | Left  a -> Left a
                             | Right c -> k b c

            /// Sequentially compose four actions, passing any value produced by the
            /// first two as arguments to the third.
            let inline bind3 (k: ^b -> ^c -> ^d -> Either< ^a, ^e>) mb mc md =
                match mb with
                | Left  a -> Left a
                | Right b -> match mc with
                             | Left  a -> Left a
                             | Right c -> match md with
                                          | Left  a -> Left a
                                          | Right d -> k b c d

            /// Sequentially compose two actions, creating a third from the result and
            /// lifting a binary function on its effects.
            let inline bindMap (k: ^b -> Either< ^a, ^c>) (f: ^b -> ^c -> ^d) m =
                match m with
                | Left  a -> Left a
                | Right b -> match k b with Left a -> Left a | Right c -> Right (f b c)

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to the intermediate result.
            /// The default monadic 'identity' function is used in each iteration where the continuation is applied.
            let inline recM f (x: ^a) : Either< ^e, ^b> =
                //let rec go m = bind (f (wrap >> go)) m in go (f wrap x)
                let rec go = function
                | Left e  -> Left e
                | Right a -> f (Right >> go) a
                go (Right x)

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to an 'effect' applied over the intermediate result.
            /// Any constructor can be used in each iteration, in the case of union-types.
            let inline recMp f x =
                let rec go m = bind (f go) m in go (f id x)
            
            /// <summary>Monadic fold over a structure associating to the right.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldrM (f: ^b -> ^s -> Either< ^a, ^s>) (s0: ^s) (source: ^b seq) : Either< ^a, ^s> =
                let g k x s = bind k (f x s)
                Seq.fold g wrap source s0

            /// <summary>Monadic fold over a structure associating to the left.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldlM (f: ^s -> ^b -> Either< ^a, ^s>) (s0: ^s) (source: ^b seq) : Either< ^a, ^s> =
                let g x k s = bind k (f s x)
                Seq.foldBack g source wrap s0            


            /// Monadic zipping (combining or decomposing corresponding monadic elements).
            module Zip =
            
                /// Combine the corresponding contents of two monads into a single monad.
                let inline mzipWith (f: ^b -> ^c -> ^d) mb mc : Either< ^a, ^d> =
                    match mb with
                    | Left  a -> Left a
                    | Right b -> match mc with
                                 | Left  a -> Left a
                                 | Right c -> Right (f b c)

                /// Merge the contents (of corresponding pairs) of two monads into a monad of pairs.
                let inline mzip mb mc : Either< ^a, ^b * ^c> =
                    match mb with
                    | Left  a -> Left a
                    | Right b -> match mc with
                                 | Left  a -> Left a
                                 | Right c -> Right (b, c)
                    
                /// Decompose a monad comprised of corresponding pairs of values.
                let inline munzip (m: Either< ^a, ^b * ^c>) =
                    match m with
                    | Left      a  -> (Left a), (Left a)
                    | Right (b, c) -> (Right b), (Right c)        


        /// Creates a monadic workflow for the given type.
        let either = Monad.EitherBuilder ()


        /// Supplementary Applicative operations on the given type.
        module Applicative =

            /// Lift a binary function on effects.
            let inline map2 (f: ^b -> ^c -> ^d) fb fc : Either< ^a, ^d> =
                match fb with
                | Left  a -> Left a
                | Right b -> match fc with
                             | Left  a -> Left a
                             | Right c -> Right (f b c)

            /// Lift a ternary function on effects.
            let inline map3 (f: ^b -> ^c -> ^d -> ^e) fb fc fd : Either< ^a, ^e> =
                match fb with
                | Left  a -> Left a
                | Right b -> match fc with
                             | Left  a -> Left a
                             | Right c -> match fd with
                                          | Left  a -> Left a
                                          | Right d -> Right (f b c d)

            /// Sequentially compose two effects, discarding any value produced by the first.
            let inline andThen fc fb : Either< ^a, ^c> =
                match fb with Left a -> Left a | Right _ -> fc

            /// Conditional execution of effectful expressions.
            let inline when_ (condition: bool) f : Either< ^a, unit> =
                if condition then f () else wrap ()

            /// <summary>Generalizes the sequence-based filter function.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline filterA (p: ^b -> Either< ^a, bool>) source =
                Seq.foldBack (fun x xs -> map2 (fun flg xs -> if flg then x::xs else xs) (p x) xs) source (wrap [])

            /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline sequenceA (source: Either< ^a, ^b> seq) : Either< ^a, ^b seq> =
                let mutable a0 = Unchecked.defaultof< ^a>
                let f = function Left a -> a0 <- a; failwith "Left" | Right b -> b
                try let xs = seq { for x in source -> f x } |> Seq.cache
                    for _ in xs do ()
                    Right xs
                with e when e.Message = "Left" -> Left a0 | e -> raise e

            /// <summary>Produce an effect for the elements in the sequence from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline forA (f: ^b -> Either< ^a, ^c>) source : Either< ^a, ^c seq> =
                sequenceA (Seq.map f source)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when either input sequence is null.</exception>
            let inline for2A (f: ^b -> ^c -> Either< ^a, ^d>) source1 source2 : Either< ^a, ^d seq> =
                forA ((<||) f) (Seq.allPairs source1 source2)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right, then evaluate each effect and collect the results.
            /// If one sequence is longer, its extra elements are ignored.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline zipWithA (f: ^b -> ^c -> Either< ^a, ^d>) source1 source2 : Either< ^a, ^d seq> =
                sequenceA (Seq.map2 f source1 source2)

            /// Performs the effect 'n' times.
            let inline replicateA (n: uint32) (m: Either< ^a, ^b>) : Either< ^a, ^b seq> =
                match m with
                | Left  a -> Left a
                | Right b -> Right (Seq.replicate (int n) b)


        /// Supplementary Functor operations on the given type.
        module Functor =

            /// Replace all locations in the input with the same value.
            let inline replace (b: ^c) fb : Either< ^a, ^c> =
                match fb with Left a -> Left a | Right _ -> Right b

            /// Perform an operation, store its result, perform an action using both
            /// the input and output, and finally return the output.
            let inline tee (f: ^b -> ^c) (g: ^b -> ^c -> unit) fa : Either< ^a, ^c> =
                match fa with
                | Left  a -> Left a
                | Right b -> let c = f b in g b c; Right c


        /// A two paramater functor where both the first and second arguments are covariant.
        module Bifunctor =

            /// Map over both arguments at the same time.
            let inline bimap f g m =
                match m with Left a -> Left (f a) | Right b -> Right (g b)

            /// Map covariantly over the first argument.
            let inline mapFst f m = match m with Left a -> Left a | Right b -> Right (f b)

            /// Map covariantly over the second argument.
            let inline mapSnd g m = match m with Left a -> Left (g a) | Right b -> Right b


        /// Types with a binary, associative composition operation.
        module Semigroup =

            /// An associative composition operation.
            let inline sappend e1 e2 =
                match e1 with
                | Left _  -> e2
                | Right _ -> e1
        


open Std
open Composition
  
//  @ Operators @
type Either<'a, 'b> with

// @ Primitive @

    /// Return the contents of a Right-value or a default value otherwise.
    static member inline ( >- ) (m, d) = fromRight d m
    /// Return the contents of a Right-value or a default value otherwise.
    static member inline ( -< ) (d, m) = fromRight d m

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