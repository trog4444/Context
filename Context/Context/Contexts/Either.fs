namespace PTR.Context.Type


/// The Either type represents values with two possibilities:
/// a value of type Either<A, B> is either `Left A` or `Right B`.
[<Struct>]
type Either<'A, 'B> = Left of L: ^A | Right of R: ^B


/// Operations on `Either` values.
module Either =
 
    /// Return True if the given value is a Left-value, False otherwise.
    let isLeft either = match either with Left _ -> true | Right _ -> false

    /// Return True if the given value is a Right-value, False otherwise.
    let isRight either = match either with Left _ -> false | Right _ -> true

    /// Apply one of the given functions to the value held within an Either, depending
    /// on if the value is 'Left' or 'Right.'
    let inline ofEither fRight fLeft (either: Either< ^a, ^b>) : ^c =
        match either with
        | Left e  -> fLeft e
        | Right a -> fRight a

    /// Return the contents of a Left-value or a default value otherwise.
    let fromLeft (def: 'a) either : ^a =
        match either with Left a -> a | Right _ -> def

    /// Return the contents of a Left-value or a default value otherwise.
    let inline fromLeftWith def either =
        match either with Left a -> a | Right _ -> def ()

    /// Return the contents of a Right-value or a default value otherwise.
    let fromRight (def: 'a) either : ^a =
        match either with Left _ -> def | Right a -> a

    /// Return the contents of a Right-value or a default value otherwise.
    let inline fromRightWith def either =
        match either with Left _ -> def () | Right a -> a

    /// <summary>Extracts from a sequence of Eithers all the Left elements in order.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    let lefts (eithers: Either<'a, 'b> seq) : ^a seq =
        seq { for e in eithers do match e with Left a -> yield a | Right _ -> () }

    /// <summary>Extracts from a sequence of Eithers all the Right elements in order.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    let rights (eithers: Either<'a, 'b> seq) : ^b seq =
        seq { for e in eithers do match e with Left _ -> () | Right b -> yield b }
  
    /// <summary>Partitions a list of Either into two lists. All the Left elements are extracted, in order,
    /// to the first component of the output. Similarly the Right elements are extracted to the second component of the output.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    let inline partition eithers =
        match Seq.foldBack (fun e (struct (xs, ys)) ->
            match e with
            | Left a  -> struct (a::xs, ys)
            | Right b -> struct (xs, b::ys)) eithers (struct ([], [])) with struct (a, b) -> a, b

    /// Removes the Left-value type from an Either, carrying Right-value(s) into a Some; None otherwise.
    let hush (either: Either<'``_``, 'a>) : ^a option =
        match either with Left _ -> None | Right a -> Some a


    /// Convert between values of type `Either` and related types.
    module Convert =
    
        /// Returns a Right-value with the head of a non-empty sequence and
        /// returns a Left-value if the sequence is empty. The Left-value will
        /// say if the input was either null or empty.
        let ofSeq (source: 'a seq) =
            match source with
            | null -> Left "Input sequence was null."
            | _  -> if Seq.isEmpty source then Left "Input sequence was empty."
                    else Right (Seq.head source)

        /// Returns a singleton sequence if thet value is a Right-value, an empty sequence otherwise.
        let toSeq either =
            match either with Left _ -> Seq.empty | Right a -> Seq.singleton a    

        /// Convert a Choice (Of2) to an Either.
        let ofChoice choice =
            match choice with
            | Choice1Of2 a -> Right a
            | Choice2Of2 b -> Left b

        /// Convert an Either to a Choice (Of2).
        let toChoice either =
            match either with
            | Right a -> Choice1Of2 a
            | Left b -> Choice2Of2 b

        /// Convert a Result to an Either.
        let ofResult result =
            match result with
            | Ok a    -> Right a
            | Error b -> Left b

        /// Convert an Either to a Result.
        let toResult result =
            match result with
            | Right a -> Ok a
            | Left b  -> Error b


    /// Compositional operations on `Either` values.
    module Compose =

        /// Supplementary Monad operations on the given type.
        module Monad =

            /// Lift a value onto an effectful context.
            let wrap (x: 'b) : Either<'a, ^b> = Right x

            /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
            let inline bind (k: ^b -> Either< ^a, ^c>) m =
                match m with Left a -> Left a | Right b -> k b

            /// Removes one layer of monadic context from a nested monad.
            let flatten (mm: Either<'a, Either< ^a, 'b>>) : Either< ^a, ^b> =
                match mm with
                | Left e  -> Left e
                | Right m -> m


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
            let inline recM f (x: ^a) : Either< ^e, ^b> =
                let rec go = function
                | Left e  -> Left e
                | Right a -> f (fun x -> go (Right x)) a
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
                Seq.foldBack (fun x k s -> bind k (f s x)) source wrap s0


        /// Supplementary Applicative operations on the given type.
        module Applicative =

            /// Lift a value onto an effectful context.
            let wrap (x: 'b) : Either<'a, ^b> = Right x

            /// Sequential application on effects.
            let inline ap mv mf : Either< ^a, ^c> =
                match mf with
                | Left a -> Left a
                | Right f -> match mv with
                             | Left a  -> Left a
                             | Right v -> Right (f v)

            /// Lift a binary function on effects.
            let inline map2 (f: ^b -> ^c -> ^d) fb fc : Either< ^a, ^d> =
                match fb with
                | Left a  -> Left a
                | Right b -> match fc with
                             | Left a  -> Left a
                             | Right c -> Right (f b c)

            /// Lift a ternary function on effects.
            let inline map3 (f: ^b -> ^c -> ^d -> ^e) fb fc fd : Either< ^a, ^e> =
                match fb with
                | Left a  -> Left a
                | Right b -> match fc with
                             | Left a  -> Left a
                             | Right c -> match fd with
                                          | Left a  -> Left a
                                          | Right d -> Right (f b c d)

            /// Sequentially compose two effects, discarding any value produced by the first.
            let andThen (fc: Either<'a, 'c>) fb : Either< ^a, ^c> =
                match fb with Left a -> Left a | Right _ -> fc

            /// Conditional execution of effectful expressions.
            let inline when_ condition f : Either< ^a, unit> =
                if condition then f () else wrap ()

            /// <summary>Generalizes the sequence-based filter function.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline filterA (p: ^b -> Either< ^a, bool>) source =
                let mutable e = Unchecked.defaultof< ^a>
                try let xs =
                        seq { for x in source do
                                  match p x with
                                  | Left e' -> e <- e'; failwith "Left"
                                  | Right fl -> if fl then yield x } |> Seq.cache
                    do for _ in xs do ()
                    Right xs
                with e when e.Message = "Left" -> Left e | err -> raise err

            /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline sequenceA (source: Either< ^a, ^b> seq) : Either< ^a, ^b seq> =
                let mutable a0 = Unchecked.defaultof<_>
                try let f = function Left a -> a0 <- a; failwith "Left" | Right b -> b
                    (match source with
                    | :? array<Either< ^a, ^b>> as xs -> Array.map f xs :> _ seq
                    | :? list<Either< ^a, ^b>>  as xs -> List.map  f xs :> _ seq
                    | _ ->
                        let xs = Seq.cache (seq { for x in source -> f x })
                        do for _ in xs do ()
                        xs) |> Right
                with e when e.Message = "Left" -> Left a0 | e -> raise e

            /// <summary>Produce an effect for the elements in the sequence from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline forA (f: ^b -> Either< ^a, ^c>) source : Either< ^a, ^c seq> =
                sequenceA (Seq.map f source)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when either input sequence is null.</exception>
            let inline for2A (f: ^b -> ^c -> Either< ^a, ^d>) source1 source2 : Either< ^a, ^d seq> =
                sequenceA (seq { for x in source1 do
                                 for y in source2 do
                                     yield f x y })

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right, then evaluate each effect and collect the results.
            /// If one sequence is longer, its extra elements are ignored.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline zipWithA (f: ^b -> ^c -> Either< ^a, ^d>) source1 source2 : Either< ^a, ^d seq> =
                sequenceA (Seq.map2 f source1 source2)

            /// Performs the effect 'n' times.
            let replicateA count (fa: Either<'a, 'b>) : Either< ^a, ^b seq> =
                match fa with
                | Left a  -> Left a
                | Right b -> Right (Seq.replicate (max 0 count) b)


        /// Supplementary Functor operations on the given type.
        module Functor =

            /// Lift a function onto effects.
            let inline map (f: ^b -> ^c) fa : Either< ^a, ^c> =
                match fa with Left a -> Left a | Right b -> Right (f b)

            /// Replace all locations in the input with the same value.
            let replace (b: 'c) (fb: Either<'a, 'b>) : Either< ^a, ^c> =
                match fb with Left a -> Left a | Right _ -> Right b

            /// Perform an operation, store its result, perform an action using both
            /// the input and output, and finally return the output.
            let inline tee (f: ^b -> ^c) (g: ^b -> ^c -> unit) fa : Either< ^a, ^c> =
                match fa with
                | Left a  -> Left a
                | Right b -> let c = f b in g b c; Right c


        /// A two paramater functor where both the first and second arguments are covariant.
        module Bifunctor =

            /// Map over both arguments at the same time.
            let inline bimap f g (m: Either< ^a, ^c>) : Either< ^b, ^d> =
                match m with Left a -> Left (f a) | Right b -> Right (g b)

            /// Map covariantly over the first argument.
            let inline mapFst f (m: Either< ^a, ^c>) : Either< ^b, ^c> =
                match m with Left a -> Left (f a) | Right b -> Right b

            /// Map covariantly over the second argument.
            let inline mapSnd g (m: Either< ^a, ^c>) : Either< ^a, ^d> =
                match m with Left a -> Left a | Right b -> Right (g b)


        /// Types with a binary, associative composition operation.
        module Semigroup =

            /// An associative composition operation.
            let sappend e1 (e2: Either<'a, 'b>) : Either< ^a, ^b> =
                match e1 with
                | Left _  -> e2
                | Right _ -> e1
    
      
    /// Creates a computation expression for the given type.
    let either = Compose.Monad.EitherBuilder ()



open Either
open Compose
 
// @ Operators @
type Either<'A, 'B> with

// @ Primitive @

    /// Return the contents of a Right-value or a default value otherwise.
    static member inline ( >- ) (m, d) = fromRight d m
    /// Return the contents of a Right-value or a default value otherwise.
    static member inline ( -< ) (d, m) = fromRight d m

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
