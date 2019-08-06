namespace PTR.Context.Type


/// The Maybe type represents values which may or may not be exist.
[<Struct>]
type Maybe<'T> = Nothing | Just of ^T


/// Operations on `Maybe` values.
module Maybe =

    /// Return True if the given value is a Just-value, False otherwise.
    let isJust maybe = match maybe with Nothing -> false | Just _ -> true

    /// Return True if the given value is a Nothing-value, False otherwise.
    let isNothing maybe = match maybe with Nothing -> true | Just _ -> false

    /// Takes a default value, a function, and a Maybe value.
    /// If the Maybe value is Nothing, the function returns the default value.
    /// Otherwise, it applies the function to the value inside the Just and returns the result.
    let inline ofMaybe def f maybe : ^b =
        match maybe with
        | Nothing -> def
        | Just a -> f a

    /// Takes a default value, a function, and a Maybe value.
    /// If the Maybe value is Nothing, the function returns the default value.
    /// Otherwise, it applies the function to the value inside the Just and returns the result.
    let inline ofMaybeWith def f maybe =
        match maybe with
        | Nothing -> def ()
        | Just a -> f a

    /// Return the contents of a Just-value or a default value otherwise.
    let fromMaybe def maybe =
        match maybe with Nothing -> def | Just a -> a
  
    /// Return the contents of a Just-value or a default value otherwise.
    let inline fromMaybeWith def maybe =
        match maybe with Nothing -> def () | Just a -> a

    /// <summary>The catMaybes function takes a sequence of Maybes and returns a list of all the Just values.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    let catMaybes maybes =
        seq { for m in maybes do
                match m with
                | Nothing -> ()
                | Just a -> yield a }

    /// <summary>Version of Seq.map that can throw out elements. If the result of 'f' is
    /// a Just value, the result is included, otherwise it is excluded.</summary>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    let inline mapMaybes f source =
        seq { for x in source do
                match f x with
                | Nothing -> ()
                | Just a -> yield a }

    /// 'Adds' a value/parameter to a Maybe-value by converting Just-values into Choice1Of2's,
    /// otherwise puts a default 'note' value into a Choice2Of2.
    let note msg maybe =
        match maybe with Just a -> Choice1Of2 a | Nothing -> Choice2Of2 msg


    /// Convert between values of type `Maybe` and related types.
    module Convert =
    
        /// Returns a Just-value with the head of a non-empty sequence and returns Nothing if the sequence is empty.
        let ofSeq source =
            match source with
            | null -> Nothing
            | xs   -> if Seq.isEmpty xs then Nothing else Just (Seq.head xs)

        /// Returns a singleton sequence if thet value is a Just-value, an empty sequence otherwise.
        let toSeq maybe =
            seq { match maybe with Nothing -> () | Just a -> yield a }

        /// Convert a .NET Option-type to a Maybe-type. None => Nothing and Some a => Just a.
        let ofOption option = match option with None -> Nothing | Some a -> Just a

        /// Convert a Maybe-type to a .NET Option-type. Nothing => None and Just a => Some a.
        let toOption maybe = match maybe with Nothing -> None | Just a -> Some a
    
        /// Create a Just-value if the given object is not null, Nothing otherwise.
        let ofObj o = match o with null -> Nothing | _ -> Just o

        /// Create a Just-value if the given object has a value, Nothing otherwise.
        let ofNullable (n: System.Nullable<'a>) =
            if n.HasValue then Just n.Value else Nothing


    /// Compositional operations on `Maybe` values.
    module Compose =

        /// Supplementary Monad operations on the given type.
        module Monad =

            /// Lift a value onto an effectful context.
            let wrap x = Just x

            /// Sequentially compose two effects, passing any value produced by the first
            /// as an argument to the second.
            let inline bind k m =
                match m with Nothing -> Nothing | Just a -> k a

            /// Removes one layer of monadic context from a nested monad.
            let flatten mm =
                match mm with Nothing -> Nothing | Just m -> m


            /// Monadic computation builder specialised to the given monad.
            type MaybeBuilder () =
                member inline s.Bind(m, k) = bind k m
                member inline s.Return x = wrap x
                member inline s.ReturnFrom m : ^a Maybe = m
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
            let inline recM f (x: ^a) : Maybe< ^b> =
                let rec go = function
                | Nothing -> Nothing
                | Just a -> f (fun x -> go (Just x)) a
                go (Just x)

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to an 'effect' applied over the intermediate result.
            /// Any constructor can be used in each iteration, in the case of union-types.
            let inline recMp f x =
                let rec go m = bind (f go) m in go (f id x)    

            /// <summary>Monadic fold over a structure associating to the right.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldrM (f: ^a -> ^s -> Maybe< ^s>) s0 source : Maybe< ^s> =
                let g k x s = bind k (f x s)
                Seq.fold g wrap source s0

            /// <summary>Monadic fold over a structure associating to the left.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldlM (f: ^s -> ^a -> Maybe< ^s>) s0 (source: ^a seq) : Maybe< ^s> =
                //let g x k s = bind k (f s x)
                //Seq.foldBack (fun x k s -> bind k (f s x)) source wrap s0    
                let mutable s = s0
                let mutable fl = true
                use e = source.GetEnumerator()
                while fl && e.MoveNext() do
                    match f s e.Current with
                    | Nothing -> fl <- false
                    | Just s' -> s <- s'
                if fl then Just s else Nothing


            /// Monads that also support choice and failure.
            module Plus =

                /// The identity of mplus.
                let mzero<'a> : ^a Maybe = Nothing

                /// A monoidal operation on monads, supporting choice and failure.
                let mplus m1 m2 =
                    match m1 with
                    | Nothing -> m2
                    | Just _  -> m1

                /// Conditional blocking of effectful computations.
                let guard condition = if condition then Just () else Nothing

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
                    let mOfSeq xs =
                        // msum (Seq.map wrap xs)
                        if Seq.isEmpty xs then Nothing else Just (Seq.head xs)

                    /// Generalizes the 'Seq.where' function.
                    let inline mwhere p m =
                        // bind (fun a -> if p a then m else mzero) m
                        match m with
                        | Nothing -> Nothing
                        | Just a -> if p a then m else Nothing

                    /// Opposite of the 'mwhere' function.
                    let inline mremove p m =
                        // mwhere (fun x -> not (p x)) m
                        match m with
                        | Nothing -> Nothing
                        | Just a -> if p a then Nothing else m

                    /// Generalizes the 'Seq.partition' function.
                    let inline mpartition p m =
                        // mwhere p m, mremove p m
                        match m with
                        | Nothing -> Nothing, Nothing
                        | Just a -> if p a then Just a, Nothing else Nothing, Just a

                    /// Translate a form of Option.defaultWith to an arbitrary 'MonadPlus' type.
                    let mofOption m =
                        // let ofOption b f m = match m with None -> b | Some a -> f a
                        // ofOption mzero wrap m
                        match m with None -> Nothing | Some a -> Just a

                    /// Translate a form of 'Option.defaultWith' to an arbitrary 'MonadPlus' type.
                    let mconcatOption m =
                        // let inline ofOption b f m = match m with None -> b | Some a -> f a
                        // bind (option mzero wrap) m          
                        match m with
                        | Nothing       -> Nothing
                        | Just None     -> Nothing
                        | Just (Some a) -> Just a

                    /// Generalizes the 'Seq.choose' function.
                    let inline mchoose (f: ^a -> ^b option) m =
                        // mconcatOption (map f m)
                        match m with
                        | Nothing -> Nothing
                        | Just a -> match f a with None -> Nothing | Some b -> Just b

                    /// Collects the values from Choice1Of2's, while discarding the rest.
                    let mchoice1 m =
                        // let l = function Choice1Of2 a -> Some a | Choice2Of2 _ -> None
                        // mconcatOption (map l m)
                        match m with
                        | Nothing -> Nothing
                        | Just (Choice1Of2 a) -> Just a
                        | Just (Choice2Of2 _) -> Nothing
            
                    /// Collects the values from Choice2Of2's, while discarding the rest.
                    let mchoice2 m =
                        // let r = function Choice2Of2 a -> Some a | Choice1Of2 _ -> None
                        // mconcatOption (map r m)
                        match m with
                        | Nothing -> Nothing
                        | Just (Choice1Of2 _) -> Nothing
                        | Just (Choice2Of2 a) -> Just a

                    /// Collects the values from Choice1Of2s on the left, and from Choice2Of2s on the right.
                    let mpartitionChoice m =
                        // mchoice1 m, mchoice2 m
                        match m with
                        | Nothing -> Nothing, Nothing
                        | Just (Choice1Of2 a) -> Just a, Nothing
                        | Just (Choice2Of2 b) -> Nothing, Just b


        /// Supplementary Applicative operations on the given type.
        module Applicative =

            /// Lift a value onto an effectful context.
            let wrap x = Just x

            /// Sequential application on effects.
            let inline ap mv mf =
                match mf with
                | Nothing -> Nothing
                | Just f -> match mv with
                            | Nothing -> Nothing
                            | Just v -> Just (f v)

            /// Lift a binary function on effects.
            let inline map2 f fa fb =
                match fa with
                | Nothing -> Nothing
                | Just a -> match fb with
                            | Nothing -> Nothing
                            | Just b -> Just (f a b)

            /// Lift a ternary function on effects.
            let inline map3 f fa fb fc =
                match fa with
                | Nothing -> Nothing
                | Just a -> match fb with
                            | Nothing -> Nothing
                            | Just b -> match fc with
                                        | Nothing -> Nothing
                                        | Just c -> Just (f a b c)

            /// Sequentially compose two effects, discarding any value produced by the first.
            let andThen fb fa = match fa with Nothing -> Nothing | Just _ -> fb

            /// Conditional execution of effectful expressions.
            let inline when_ condition f = if condition then f () else wrap ()

            /// <summary>Generalizes the sequence-based filter function.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline filterA p source =
                try let xs = seq { for x in source do
                                    match p x with
                                    | Nothing -> failwith "Nothing"
                                    | Just fl -> if fl then yield x } |> Seq.cache
                    do for _ in xs do ()
                    Just xs
                with e when e.Message = "Nothing" -> Nothing | e -> raise e    

            /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline sequenceA (source: ^a Maybe seq) =                
                try let f = function Nothing -> failwith "Nothing" | Just a -> a
                    match source with
                    | :? array<Maybe< ^a>> as xs -> Just (Array.map f xs :> ^a seq)
                    | :? list<Maybe< ^a>>  as xs -> Just (List.map  f xs :> ^a seq)
                    | _ ->
                        let xs = Seq.cache (seq { for x in source -> f x })
                        do for _ in xs do ()                
                        Just xs
                with e when e.Message = "Nothing" -> Nothing | e -> raise e

            /// <summary>Produce an effect for the elements in the sequence from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline forA f source = sequenceA (Seq.map f source)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when maybe input sequence is null.</exception>
            let inline for2A f source1 source2 =
                forA ((<||) f) (Seq.allPairs source1 source2)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right, then evaluate each effect and collect the results.
            /// If one sequence is longer, its extra elements are ignored.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline zipWithA f source1 source2 =
                sequenceA (Seq.map2 f source1 source2)

            /// Performs the effect 'n' times.
            let replicateA count fa =
                match fa with
                | Nothing -> Nothing
                | Just a  -> Just (Seq.replicate (max 0 count) a)


            /// A monoid on applicative functors.
            module Alternative =

                /// The identity of orElse.
                let empty<'a> : ^a Maybe = Nothing

                /// An associative binary operation on applicative functors.
                let orElse choice2 choice1 = 
                    match choice1 with
                    | Nothing -> choice2
                    | Just _  -> choice1

                /// <summary>The sum of a collection of effects.</summary>
                /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
                let inline asum t_fa =
                    //do failwith "need Apply vsersion| monad may do diff things"
                    Monad.Plus.General.msum t_fa

                /// Return one or none results on effects.
                let optional fa =
                    // orElse (wrap None) (map Some fa)
                    match fa with
                    | Nothing -> Just None
                    | Just a  -> Just (Some a)

                /// Create a new item if the previous was empty, else keep the original.
                let inline alt def fa =
                    match fa with
                    | Nothing -> def ()
                    | Just _  -> fa


        /// Supplementary Functor operations on the given type.
        module Functor =

            /// Lift a function onto effects.
            let inline map f m =
                match m with Nothing -> Nothing | Just a -> Just (f a)

            /// Replace all locations in the input with the same value.
            let replace b fa =
                match fa with Nothing -> Nothing | Just _ -> Just b

            /// Perform an operation, store its result, perform an action using both
            /// the input and output, and finally return the output.
            let inline tee f g fa =
                match fa with
                | Nothing -> Nothing
                | Just a  -> let b = f a in g a b; Just b


        /// Types with a binary, associative composition operation.
        module Semigroup =

            /// An associative composition operation.
            let inline sappend e1 e2 =
                match e1 with
                | Nothing -> e2
                | Just a -> match e2 with
                            | Nothing -> e2
                            | Just b -> Just (^a: (static member Append: ^a -> ^a -> ^a) (a, b))


        /// Types with a binary, associative composition operation and an identity element.
        module Monoid =

            /// An associative composition operation.
            let inline mappend e1 e2 = Semigroup.sappend e1 e2

            /// The identity element for the composition operator.
            let mempty<'a> : ^a Maybe = Nothing

            /// Repeat a value 'n' times.
            let inline mtimes n e =
                match e with
                | Nothing -> Nothing
                | Just a ->
                    match max 0 n with
                    | 0 -> Nothing
                    | n -> let mutable r : ^a = a
                           for __ = 1 to n do
                               r <- (^a: (static member Append: ^a -> ^a -> ^a) (a, r))
                           Just r

            /// <summary>Combine elements of a sequence using monoidal composition.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline mconcat source = Seq.foldBack Semigroup.sappend source mempty  


    /// Creates a computation expression for the given type.
    let maybe = Compose.Monad.MaybeBuilder ()



open Maybe
open Compose
 
// @ Operators @
type Maybe<'T> with

// @ Primitive @

    /// Return the contents of a Just-value or a default value otherwise.
    static member inline ( >- ) (m, d) = fromMaybe d m
    /// Return the contents of a Just-value or a default value otherwise.
    static member inline ( -< ) (d, m) = fromMaybe d m

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
    static member inline Empty () : ^a Maybe = Nothing