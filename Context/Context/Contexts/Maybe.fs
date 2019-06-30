namespace Ptr.Context.Type


/// The Maybe type represents values which may or may not be exist.
[<Struct>]
type Maybe<'a> = Nothing | Just of ^a
with interface System.Collections.Generic.IEnumerable< ^a> with
        override s.GetEnumerator() = (match s with Nothing -> Seq.empty | Just a -> Seq.singleton a).GetEnumerator()
        override s.GetEnumerator() = (s :> _ seq).GetEnumerator() :> System.Collections.IEnumerator


/// Operations on `Maybe` values.
module Maybe =

    /// Return True if the given value is a Just-value, False otherwise.
    let inline isJust maybe = match maybe with Nothing -> false | Just _ -> true

    /// Return True if the given value is a Nothing-value, False otherwise.
    let inline isNothing maybe = match maybe with Nothing -> true | Just _ -> false

    /// Takes a default value, a function, and a Maybe value.
    /// If the Maybe value is Nothing, the function returns the default value.
    /// Otherwise, it applies the function to the value inside the Just and returns the result.
    let inline ofMaybe def f maybe : ^b =
        match maybe with
        | Nothing -> def
        | Just a  -> f a

    /// Return the contents of a Just-value or a default value otherwise.
    let inline fromMaybe def maybe =
        match maybe with Nothing -> def | Just a -> a
    
    /// <summary>The catMaybes function takes a sequence of Maybes and returns a list of all the Just values.</summary>
    /// <exception cref="System.ArgumentNullException"> Thrown when the input sequence is null.</exception>
    let inline catMaybes maybes =
        seq { for m in maybes do
                match m with
                | Nothing -> ()
                | Just a  -> yield a }

    /// <summary>Version of Seq.map that can throw out elements. If the result of 'f' is
    /// a Just value, the result is included, otherwise it is excluded.</summary>
    /// <exception cref="System.ArgumentNullException"> Thrown when the input sequence is null.</exception>
    let inline mapMaybes f source =
        seq { for x in source do
                match f x with
                | Nothing -> ()
                | Just a  -> yield a }

    /// 'Adds' a value/parameter to a Maybe-value by converting Just-values into Choice1Of2's,
    /// otherwise puts a default 'note' value into a Choice2Of2.
    let inline note msg maybe = match maybe with Nothing -> Choice2Of2 msg | Just a -> Choice1Of2 a


    /// Convert between values of type `Maybe` and related types.
    module Convert =
        
        /// Returns a Just-value with the head of a non-empty sequence and returns Nothing if the sequence is empty.
        let inline ofSeq (source: ^a seq) =
            match source with
            | null -> Nothing
            | xs   -> if Seq.isEmpty xs then Nothing else Just (Seq.head xs)

        /// Returns a singleton sequence if thet value is a Just-value, an empty sequence otherwise.
        let inline toSeq (maybe: ^a Maybe) = maybe :> _ seq

        /// Convert a .NET Option-type to a Maybe-type. None => Nothing and Some a => Just a.
        let inline ofOption option = match option with None -> Nothing | Some a -> Just a

        /// Convert a Maybe-type to a .NET Option-type. Nothing => None and Just a => Some a.
        let inline toOption maybe = match maybe with Nothing -> None | Just a -> Some a
        
        /// Create a Just-value if the given object is not null, Nothing otherwise.
        let inline ofObj o = match o with null -> Nothing | _ -> Just o

        /// Create a Just-value if the given object has a value, Nothing otherwise.
        let inline ofNullable (n: System.Nullable< ^a>) = if n.HasValue then Just n.Value else Nothing


    /// Compositional operations on `Maybe` values.
    module Compose =

        /// Lift a value onto an effectful context.
        let inline wrap x : Maybe< ^a> = Just x

        /// Sequentially compose two effects, passing any value produced by the first
        /// as an argument to the second.
        let inline bind (k: ^a -> Maybe< ^b>) m =
            match m with Nothing -> Nothing | Just a -> k a

        /// Removes one layer of monadic context from a nested monad.
        let inline flatten mm : Maybe< ^a> =
            match mm with Nothing -> Nothing | Just m -> m

        /// Sequential application on effects.
        let inline ap mv mf : Maybe< ^b> =
            match mf with
            | Nothing -> Nothing
            | Just  f -> match mv with
                         | Nothing -> Nothing
                         | Just  v -> Just (f v)

        /// Lift a function onto effects.
        let inline map f m : Maybe< ^b> =
            match m with Nothing -> Nothing | Just a -> Just (f a)


        /// Supplementary Monad operations on the given type.
        module Monad =

            /// Monadic computation builder specialised to the given monad.
            type MaybeBuilder () =
                member inline s.Bind(m, k) = bind k m
                member inline s.Return x = wrap x
                member inline s.ReturnFrom m : Maybe< ^a> = m
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
            let inline composeM k2 (k1: ^a -> Maybe< ^b>) : ^a -> Maybe< ^c> = k1 >> bind k2
  
            /// Sequentially compose three actions, passing any value produced by the first
            /// two as arguments to the third.
            let inline bind2 (k: ^a -> ^b -> Maybe< ^c>) ma mb =
                match ma with
                | Nothing -> Nothing
                | Just a  -> match mb with
                             | Nothing -> Nothing
                             | Just  b -> k a b

            /// Sequentially compose four actions, passing any value produced by the
            /// first two as arguments to the third.
            let inline bind3 (k: ^a -> ^b -> ^c -> Maybe< ^d>) ma mb mc =
                match ma with
                | Nothing -> Nothing
                | Just a  -> match mb with
                             | Nothing -> Nothing
                             | Just  b -> match mc with
                                          | Nothing -> Nothing
                                          | Just  c -> k a b c

            /// Sequentially compose two actions, creating a third from the result and
            /// lifting a binary function on its effects.
            let inline bindMap (k: ^a -> Maybe< ^b>) (f: ^a -> ^b -> ^c) m =
                match m with
                | Nothing -> Nothing
                | Just a  -> match k a with Nothing -> Nothing | Just b -> Just (f a b)

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to the intermediate result.
            /// The default monadic 'identity' function is used in each iteration where the continuation is applied.
            let inline recM f (x: ^a) : Maybe< ^b> =
                let rec go = function
                | Nothing -> Nothing
                | Just a  -> f (Just >> go) a
                go (Just x)

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to an 'effect' applied over the intermediate result.
            /// Any constructor can be used in each iteration, in the case of union-types.
            let inline recMp f x : Maybe< ^a> =
                let rec go m = bind (f go) m in go (f id x)        

            /// <summary>Monadic fold over a structure associating to the right.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldrM (f: ^a -> ^s -> Maybe< ^s>) s0 source : Maybe< ^s> =
                let g k x s = bind k (f x s)
                Seq.fold g wrap source s0

            /// <summary>Monadic fold over a structure associating to the left.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldlM (f: ^s -> ^a -> Maybe< ^s>) s0 source : Maybe< ^s> =
                let g x k s = bind k (f s x)
                Seq.foldBack g source wrap s0        
        

            /// Monads that also support choice and failure.
            module Plus =

                /// The identity of mplus.
                let inline mzero<'a> : Maybe< ^a> = Nothing

                /// A monoidal operation on monads, supporting choice and failure.
                let inline mplus m1 m2 =
                    match m1 with
                    | Nothing -> m2
                    | Just  _ -> m1

                /// Conditional blocking of effectful computations.
                let inline guard condition =
                    if condition then Just () else mzero

                /// Create a new item if the previous was mzero, else keep the original.
                let inline recover makeNew m =
                    // if m = mzero then makeNew () else m
                    match m with
                    | Nothing -> makeNew ()
                    | Just _  -> m

                /// Combine two monads using a 'SQL style' inner join function.
                let inline relate f k1 k2 ma mb =
                    // bind2 (fun a b -> if k1 a = k2 b then wrap (f a b) else mzero) ma mb
                    match ma with
                    | Nothing -> Nothing
                    | Just a  -> match mb with
                                 | Nothing -> Nothing
                                 | Just b  -> if k1 a = k2 b then Just (f a b) else Nothing


                /// Generalizations of functions on other types.
                module General =

                    /// <summary>Generalizes list concatenation to monads.</summary>
                    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
                    let inline msum source =
                        // Seq.foldBack mplus source mzero
                        match Seq.tryFind isJust source with
                        | None   -> Nothing
                        | Some m -> m

                    /// <summary>Generalizes the 'ofSeq' function.</summary>
                    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
                    let inline mOfSeq (xs: ^a seq) =
                        // msum (Seq.map wrap xs)
                        if Seq.isEmpty xs then Nothing else Just (Seq.head xs)

                    /// Generalizes the 'Seq.where' function.
                    let inline mwhere p m =
                        // bind (fun a -> if p a then m else mzero) m
                        match m with
                        | Nothing -> Nothing
                        | Just a  -> if p a then m else Nothing

                    /// Opposite of the 'mwhere' function.
                    let inline mremove p m =
                        // mwhere (not << p) m
                        match m with
                        | Nothing -> Nothing
                        | Just a  -> if p a then Nothing else m

                    /// Generalizes the 'Seq.partition' function.
                    let inline mpartition p m =
                        // mwhere p m, mremove p m
                        match m with
                        | Nothing -> Nothing, Nothing
                        | Just a  -> if p a then Just a, Nothing else Nothing, Just a

                    /// Translate a form of Option.defaultWith to an arbitrary 'MonadPlus' type.
                    let inline mofOption m =
                        // let inline ofOption b f m = match m with None -> b | Some a -> f a
                        // ofOption mzero wrap m
                        match m with None -> Nothing | Some a -> Just a

                    /// Translate a form of 'Option.defaultWith' to an arbitrary 'MonadPlus' type.
                    let inline mconcatOption m =
                        // let inline ofOption b f m = match m with None -> b | Some a -> f a
                        // bind (option mzero wrap) m                    
                        match m with
                        | Nothing   -> Nothing
                        | Just None -> Nothing
                        | Just (Some a) -> Just a

                    /// Generalizes the 'Seq.choose' function.
                    let inline mchoose (f: ^a -> ^b option) m =
                        // mconcatOption (map f m)
                        match m with
                        | Nothing -> Nothing
                        | Just a  -> match f a with None -> Nothing | Some b -> Just b

                    /// Collects the values from Choice1Of2's, while discarding the rest.
                    let inline mchoice1 m =
                        // let l = function Choice1Of2 a -> Some a | Choice2Of2 _ -> None
                        // mconcatOption (map l m)
                        match m with
                        | Nothing -> Nothing
                        | Just (Choice1Of2 a) -> Just a
                        | Just (Choice2Of2 _) -> Nothing
                        
                    /// Collects the values from Choice2Of2's, while discarding the rest.
                    let inline mchoice2 m =
                        // let r = function Choice2Of2 a -> Some a | Choice1Of2 _ -> None
                        // mconcatOption (map r m)
                        match m with
                        | Nothing -> Nothing
                        | Just (Choice2Of2 a) -> Just a
                        | Just (Choice1Of2 _) -> Nothing

                    /// Collects the values from Choice1Of2s on the left, and from Choice2Of2s on the right.
                    let inline mpartitionChoice m =
                        // mchoice1 m, mchoice2 m
                        match m with
                        | Nothing -> Nothing, Nothing
                        | Just (Choice1Of2 a) -> Just a, Nothing
                        | Just (Choice2Of2 b) -> Nothing, Just b


            /// Monadic zipping (combining or decomposing corresponding monadic elements).
            module Zip =
            
                /// Combine the corresponding contents of two monads into a single monad.
                let inline mzipWith f ma mb : Maybe< ^c> =
                    match ma with
                    | Nothing -> Nothing
                    | Just a  -> match mb with
                                 | Nothing -> Nothing
                                 | Just  b -> Just (f a b)

                /// Merge the contents (of corresponding pairs) of two monads into a monad of pairs.
                let inline mzip ma mb : Maybe< ^a * ^b> =
                    match ma with
                    | Nothing -> Nothing
                    | Just a  -> match mb with
                                 | Nothing -> Nothing
                                 | Just  b -> Just (a, b)
                    
                /// Decompose a monad comprised of corresponding pairs of values.
                let inline munzip (m: Maybe< ^a * ^b>) =
                    match m with
                    | Nothing     -> Nothing, Nothing
                    | Just (a, b) -> (Just a), (Just b)        


        /// Supplementary Applicative operations on the given type.
        module Applicative =

            /// Lift a binary function on effects.
            let inline map2 f fa fb : Maybe< ^c> =
                match fa with
                | Nothing -> Nothing
                | Just a  -> match fb with
                             | Nothing -> Nothing
                             | Just  b -> Just (f a b)

            /// Lift a ternary function on effects.
            let inline map3 f fa fb fc : Maybe< ^d> =
                match fa with
                | Nothing -> Nothing
                | Just a  -> match fb with
                             | Nothing -> Nothing
                             | Just  b -> match fc with
                                          | Nothing -> Nothing
                                          | Just  c -> Just (f a b c)

            /// Sequentially compose two effects, discarding any value produced by the first.
            let inline andThen fb fa : Maybe< ^b> =
                match fa with Nothing -> Nothing | Just _ -> fb

            /// Conditional execution of effectful expressions.
            let inline when_ (condition: bool) f : Maybe<unit> =
                if condition then f () else wrap ()

            /// <summary>Generalizes the sequence-based filter function.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline filterA p source =
                try let xs = seq { for x in source do
                                    match p x with
                                    | Nothing -> failwith "Nothing"
                                    | Just  g -> if g then yield x } |> Seq.cache
                    do for _ in xs do ()
                    Just xs
                with e when e.Message = "Nothing" -> Nothing | e -> raise e       

            /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline sequenceA source : Maybe< ^a seq> =
                let inline f m = match m with Nothing -> failwith "Nothing" | Just a -> a
                try let xs = Seq.cache (seq { for x in source -> f x })
                    for _ in xs do ()
                    Just xs
                with e when e.Message = "Nothing" -> Nothing | e -> raise e

            /// <summary>Produce an effect for the elements in the sequence from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline forA f (source: ^a seq) : Maybe< ^b seq> =
                sequenceA (Seq.map f source)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when maybe input sequence is null.</exception>
            let inline for2A f (source1: ^a seq) (source2: ^b seq) : Maybe< ^c seq> =
                forA ((<||) f) (Seq.allPairs source1 source2)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right, then evaluate each effect and collect the results.
            /// If one sequence is longer, its extra elements are ignored.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline zipWithA f (source1: ^a seq) (source2: ^b seq) : Maybe< ^c seq> =
                sequenceA (Seq.map2 f source1 source2)

            /// Performs the effect 'n' times.
            let inline replicateA (n: uint32) m : Maybe< ^a seq> =
                match m with
                | Nothing -> Nothing
                | Just a  -> Just (Seq.replicate (int n) a)


            /// A monoid on applicative functors.
            module Alternative =

                /// The identity of orElse.
                let inline empty<'a> : Maybe< ^a> = Nothing

                /// An associative binary operation on applicative functors.
                let inline orElse choice2 choice1 = 
                    match choice1 with
                    | Nothing -> choice2
                    | Just _  -> choice1

                /// <summary>The sum of a collection of effects.</summary>
                /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
                let inline asum t_fa =
                    //do failwith "need Apply vsersion| monad may do diff things"
                    Monad.Plus.General.msum t_fa

                /// Return one or none results on effects.
                let inline optional fa =
                    // orElse (wrap None) (map Some fa)
                    match fa with
                    | Nothing -> Just None
                    | Just a  -> Just (Some a)

                /// Create a new item if the previous was empty, else keep the original.
                let inline alt def fx =
                    match fx with
                    | Nothing -> def ()
                    | Just _  -> fx


        /// Supplementary Functor operations on the given type.
        module Functor =

            /// Replace all locations in the input with the same value.
            let inline replace b fa : Maybe< ^b> =
                match fa with Nothing -> Nothing | Just _ -> Just b

            /// Perform an operation, store its result, perform an action using both
            /// the input and output, and finally return the output.
            let inline tee f (g: ^a -> ^b -> unit) fa : Maybe< ^b> =
                match fa with
                | Nothing -> Nothing
                | Just a  -> let b = f a in g a b; Just b


        /// Types with a binary, associative composition operation.
        module Semigroup =

            /// An associative composition operation.
            let inline sappend e1 e2 =
                match e1 with
                | Nothing -> e2
                | Just a  -> match e2 with
                             | Nothing -> e2
                             | Just b  -> Just (^a: (static member Append: ^a -> ^a -> ^a) (a, b))


        /// Types with a binary, associative composition operation and an identity element.
        module Monoid =

            /// An associative composition operation.
            let inline mappend e1 e2 = Semigroup.sappend e1 e2

            /// The identity element for the composition operator.
            let inline mempty<'a> : Maybe< ^a> = Nothing

            /// Repeat a value 'n' times.
            let inline mtimes (n: uint32) e =            
                match e with
                | Nothing -> Nothing
                | Just a  ->
                    if n = 0u then Nothing
                    else let mutable r : ^a = a
                         for i = 1 to int n do r <- (^a: (static member Append: ^a -> ^a -> ^a) (a, r))
                         Just r

            /// <summary>Combine elements of a sequence using monoidal composition.</summary>
            /// <exception cref="System.ArgumentNullException"> Thrown when the input sequence is null.</exception>
            let inline mconcat source =
                Seq.foldBack Semigroup.sappend source mempty    


    /// Creates a computation expression for the given type.
    let maybe = Compose.Monad.MaybeBuilder ()



open Maybe
open Compose
  
//  @ Operators @
type Maybe<'a> with

// @ Primitive @

    /// Return the contents of a Just-value or a default value otherwise.
    static member inline ( >- ) (m, d) = fromMaybe d m
    /// Return the contents of a Just-value or a default value otherwise.
    static member inline ( -< ) (d, m) = fromMaybe d m

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

// @ Applicative.Alternative @

    /// An associative binary operation on applicative functors.
    static member inline ( <|> ) (c1, c2) = Applicative.Alternative.orElse c2 c1
    /// An associative binary operation on applicative functors.
    static member inline ( <||> ) (c2, c1) = Applicative.Alternative.orElse c2 c1

// @ Functor @

    /// Lift a function onto effects.
    static member inline ( |>> ) (m, f) = map f m
    /// Lift a function onto effects.
    static member inline ( <<| ) (f, m) = map f m

    /// Replace all locations in the input with the same value.
    static member inline ( &> ) (b, fx) = Functor.replace b fx
    /// Replace all locations in the input with the same value.
    static member inline ( <& ) (fx, b) = Functor.replace b fx

// @ Semigroup @

    /// An associative composition operation.
    static member inline Append (e1, e2) = Semigroup.sappend e1 e2

    /// An associative composition operation.
    static member inline ( ++ ) (e1, e2) = Semigroup.sappend e1 e2

// @ Monoid @

    static member inline Empty () : Maybe< ^a> = Nothing