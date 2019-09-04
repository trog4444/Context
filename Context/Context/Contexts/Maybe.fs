namespace PTR.Context.Type


[<Struct>]
type Maybe<'T> = Nothing | Just of ^T


module Maybe =

    [<CompiledName("IsJust")>]
    let isJust maybe = match maybe with Nothing -> false | Just _ -> true

    [<CompiledName("IsNothing")>]
    let isNothing maybe = match maybe with Nothing -> true | Just _ -> false

    [<CompiledName("OfMaybe")>]
    let inline ofMaybe (def: ^b) (f: ^a -> ^b) (maybe: Maybe< ^a>) : ^b =
        match maybe with
        | Nothing -> def
        | Just a  -> f a

    [<CompiledName("FromMaybe")>]
    let fromMaybe def maybe =
        match maybe with Nothing -> def | Just a -> a

    [<CompiledName("CatMaybes")>]
    let catMaybes maybes =
        seq { for m in maybes do
                match m with
                | Nothing -> ()
                | Just a  -> yield a }

    [<CompiledName("MapMaybes")>]
    let inline mapMaybes (f: ^a -> Maybe< ^b>) (source: ^a seq) : ^b seq =
        seq { for x in source do
                match f x with
                | Nothing -> ()
                | Just a  -> yield a }

    [<CompiledName("Note")>]
    let note msg maybe =
        match maybe with Just a -> Choice1Of2 a | Nothing -> Choice2Of2 msg


    module Convert =
    
        [<CompiledName("OfSeq")>]
        let ofSeq source =
            match source with
            | null -> Nothing
            | _    -> if Seq.isEmpty source then Nothing else Just (Seq.head source)

        [<CompiledName("ToSeq")>]
        let toSeq maybe =
            match maybe with Nothing -> Seq.empty | Just a -> Seq.singleton a

        [<CompiledName("OfOption")>]
        let ofOption option =
            match option with None -> Nothing | Some a -> Just a

        [<CompiledName("ToOption")>]
        let toOption maybe =
            match maybe with Nothing -> None | Just a -> Some a
    
        [<CompiledName("OfObj")>]
        let ofObj (o: 'a when 'a: null) : Maybe< ^a> when ^a: null =
            match o with null -> Nothing | _ -> Just o

        [<CompiledName("OfNullable")>]
        let ofNullable (n: System.Nullable<'a>) : Maybe< ^a> =
            if n.HasValue then Just n.Value else Nothing


    module Compose =

        module Monad =

            [<CompiledName("Wrap")>]
            let wrap x = Just x

            [<CompiledName("Bind")>]
            let inline bind (k: ^a -> Maybe< ^b>) m =
                match m with Nothing -> Nothing | Just a -> k a

            [<CompiledName("Flatten")>]
            let flatten mm =
                match mm with Nothing -> Nothing | Just m -> m


            [<Sealed>]
            type MaybeBuilder () =
                member inline s.Bind(m, k) = bind k m
                member inline s.Return x : Maybe< ^a> = wrap x
                member inline s.ReturnFrom m : Maybe< ^a> = m
                member inline s.Zero () = s.Return ()
 
                member inline s.TryWith (body, handler) = try s.ReturnFrom(body ()) with e -> handler e
                member inline s.TryFinally (body, finalizer) : Maybe< ^a> = try body () finally finalizer ()
 
                member inline s.Using(disp: ^d when ^d :> System.IDisposable, body) : Maybe< ^a> =
                    s.TryFinally((fun () -> body disp), disp.Dispose)
 
                member inline s.While(guard, body) =
                    let rec loop = function
                    | false -> s.Zero ()
                    | true  -> s.Bind(body (), fun () -> loop (guard ()))
                    loop (guard ())
 
                member inline s.For(seq: ^a seq, body) =
                    s.Using(seq.GetEnumerator(), fun enum -> s.While(enum.MoveNext, fun () -> body enum.Current))
      

            [<CompiledName("RecM")>]
            let inline recM f (x: ^a) : Maybe< ^b> =
                let rec go = function
                | Nothing -> Nothing
                | Just a  -> f lp a
                and lp x = go (Just x)
                go (Just x)

            [<CompiledName("FoldrM")>]
            let inline foldrM f s0 (source: ^a seq) =
                let inline g k x s = bind k (f x s)
                match source with
                | :? array< ^a> as s -> Array.fold g wrap s s0
                | :? list<  ^a> as s -> List.fold  g wrap s s0
                | _ -> Seq.fold g wrap source s0
            
            [<CompiledName("FoldlM")>]
            let inline foldlM f (s0: ^s) (source: ^a seq) =    
                let mutable s = s0
                let mutable g = true
                use e = source.GetEnumerator()
                while g && e.MoveNext() do
                    match f s e.Current with
                    | Nothing -> g <- false
                    | Just z  -> s <- z
                if g then Just s else Nothing


            module Plus =

                [<CompiledName("MZero")>]
                let mzero<'a> : Maybe< ^a> = Nothing

                [<CompiledName("MPlus")>]
                let mplus m1 m2 =
                    match m1 with
                    | Nothing -> m2
                    | Just _  -> m1

                [<CompiledName("Guard")>]
                let guard condition = if condition then Just () else Nothing

                [<CompiledName("Recover")>]
                let inline recover makeNew m : Maybe< ^a> =
                    match m with
                    | Nothing -> makeNew ()
                    | Just _  -> m

                [<CompiledName("Relate")>]
                let inline relate (f: ^a -> ^b -> ^c) (k1: ^a -> ^k when ^k : equality) k2 ma mb =
                    match ma with
                    | Nothing -> Nothing
                    | Just a  -> match mb with
                                 | Nothing -> Nothing
                                 | Just b  -> if k1 a = k2 b then Just (f a b) else Nothing


                module General =

                    [<CompiledName("MSum")>]
                    let inline msum (source: ^a Maybe seq) =
                        match Seq.tryFind isJust source with
                        | None   -> Nothing
                        | Some m -> m

                    [<CompiledName("MOfSeq")>]
                    let mOfSeq (source: 'a seq) : Maybe< ^a> =
                        if Seq.isEmpty source then Nothing else Just (Seq.head source)

                    [<CompiledName("MWhere")>]
                    let inline mwhere p m : Maybe< ^a> =
                        match m with
                        | Nothing -> Nothing
                        | Just a  -> if p a then m else Nothing

                    [<CompiledName("MRemove")>]
                    let inline mremove p m : Maybe< ^a> =
                        match m with
                        | Nothing -> Nothing
                        | Just a  -> if p a then Nothing else m

                    [<CompiledName("MPartition")>]
                    let inline mpartition p m : Maybe< ^a> * Maybe< ^a> =
                        match m with
                        | Nothing -> Nothing, Nothing
                        | Just a  -> if p a then Just a, Nothing else Nothing, Just a

                    [<CompiledName("MOfOption")>]
                    let mofOption (m: 'a option) : Maybe< ^a> =
                        match m with None -> Nothing | Some a -> Just a

                    [<CompiledName("MConcatOption")>]
                    let mconcatOption (m: 'a option Maybe) : Maybe< ^a> =       
                        match m with
                        | Nothing       -> Nothing
                        | Just None     -> Nothing
                        | Just (Some a) -> Just a

                    [<CompiledName("MChoose")>]
                    let inline mchoose (f: ^a -> ^b option) m =
                        match m with
                        | Nothing -> Nothing
                        | Just a  -> match f a with None -> Nothing | Some b -> Just b

                    [<CompiledName("MChoice1")>]
                    let mchoice1 (m: Maybe<Choice<'a, '``_``>>) =
                        match m with
                        | Nothing -> Nothing
                        | Just (Choice1Of2 a) -> Just a
                        | Just (Choice2Of2 _) -> Nothing
            
                    [<CompiledName("MChoice2")>]
                    let mchoice2 m =
                        match m with
                        | Nothing -> Nothing
                        | Just (Choice1Of2 _) -> Nothing
                        | Just (Choice2Of2 a) -> Just a

                    [<CompiledName("MPartitionChoice")>]
                    let mpartitionChoice m =
                        match m with
                        | Nothing -> Nothing, Nothing
                        | Just (Choice1Of2 a) -> Just a, Nothing
                        | Just (Choice2Of2 b) -> Nothing, Just b


        module Applicative =

            [<CompiledName("Wrap")>]
            let inline wrap (x: ^a) : Maybe< ^a> = Just x

            [<CompiledName("Ap")>]
            let inline ap fv (ff: Maybe<(^a -> ^b)>) =
                match ff with
                | Nothing -> Nothing
                | Just f -> match fv with
                            | Nothing -> Nothing
                            | Just v -> Just (f v)

            [<CompiledName("Map2")>]
            let inline map2 (f: ^a -> ^b -> ^c) fa fb =
                match fa with
                | Nothing -> Nothing
                | Just a -> match fb with
                            | Nothing -> Nothing
                            | Just b -> Just (f a b)

            [<CompiledName("Map3")>]
            let inline map3 (f: ^a -> ^b -> ^c -> ^d) fa fb fc =
                match fa with
                | Nothing -> Nothing
                | Just a -> match fb with
                            | Nothing -> Nothing
                            | Just b -> match fc with
                                        | Nothing -> Nothing
                                        | Just c -> Just (f a b c)

            [<CompiledName("AndThen")>]
            let andThen fb fa = match fa with Nothing -> Nothing | Just _ -> fb

            [<CompiledName("When")>]
            let inline when_ condition f = if condition then f () else Just ()

            [<CompiledName("FilterA")>]
            let inline filterA p (source: ^a seq) =
                try match source with
                    | :? array< ^a> as s ->
                        Just ([| for x = 0 to s.Length - 1 do
                                     match p s.[x] with
                                     | Nothing -> failwith "Nothing"
                                     | Just b  -> if b then yield s.[x] |] :> _ seq)
                    | :? list< ^a> as s ->
                        Just ([| for x in s do
                                     match p x with
                                     | Nothing -> failwith "Nothing"
                                     | Just b  -> if b then yield x |] :> _ seq)
                    | :? ResizeArray< ^a> as s ->
                        Just ([| for x = 0 to s.Count - 1 do
                                     match p s.[x] with
                                     | Nothing -> failwith "Nothing"
                                     | Just b  -> if b then yield s.[x] |] :> _ seq)
                    | _ ->
                        let xs = seq { for x in source do
                                           match p x with
                                           | Nothing -> failwith "Nothing"
                                           | Just b  -> if b then yield x } |> Seq.cache
                        do for _ in xs do ()
                        Just xs
                with e when e.Message = "Nothing" -> Nothing | e -> raise e    

            [<CompiledName("SequenceA")>]
            let inline sequenceA (source: Maybe< ^a> seq) =
                try let f = function Nothing -> failwith "Nothing" | Just a -> a
                    match source with
                    | :? array<Maybe< ^a>> as s -> Just (Array.map f s :> ^a seq)
                    | :? list<Maybe< ^a>>  as s -> Just ([| for x in s -> f x |] :> ^a seq)
                    | :? ResizeArray<Maybe< ^a>> as s -> Just ([| for x = 0 to s.Count - 1 do yield f s.[x] |] :> ^a seq)
                    | _ ->
                        let xs = Seq.cache (Seq.map f source) in do for _ in xs do ()                
                        Just xs
                with e when e.Message = "Nothing" -> Nothing | e -> raise e

            [<CompiledName("ForA")>]
            let inline forA (f: ^a -> Maybe< ^b>) (source: ^a seq) =
                match source with
                | :? array< ^a> as s -> sequenceA (Array.map f s)
                | :? list<  ^a> as s -> sequenceA [| for x in s ->  f x |]
                | _ -> sequenceA (Seq.map f source)

            [<CompiledName("ZipWithA")>]
            let inline zipWithA (f: ^a -> ^b -> Maybe< ^c>) source1 source2 =
                sequenceA (Seq.map2 f source1 source2)

            [<CompiledName("ReplicateA")>]
            let replicateA count fa =
                match fa with
                | Nothing -> Nothing
                | Just a  -> Just (Seq.replicate (max 0 count) a)


            module Alternative =

                [<CompiledName("Empty")>]
                let empty<'a> : Maybe< ^a> = Nothing

                [<CompiledName("OrElse")>]
                let orElse choice2 (choice1: Maybe<'a>) : Maybe< ^a> = 
                    match choice1 with
                    | Nothing -> choice2
                    | Just _  -> choice1

                [<CompiledName("ASum")>]
                let inline asum t_fa : Maybe< ^a> =
                    // do failwith "need Apply vsersion| monad may do diff things"
                    Monad.Plus.General.msum t_fa

                [<CompiledName("Optional")>]
                let optional (fa: Maybe<'a>) : Maybe<Option< ^a>> =
                    // orElse (wrap None) (map Some fa)
                    match fa with
                    | Nothing -> Just None
                    | Just a  -> Just (Some a)

                [<CompiledName("Alt")>]
                let inline alt def fa : Maybe< ^a> =
                    match fa with
                    | Nothing -> def ()
                    | Just _  -> fa


        module Functor =

            [<CompiledName("Map")>]
            let inline map (f: ^a -> ^b) m =
                match m with Nothing -> Nothing | Just a -> Just (f a)

            [<CompiledName("Replace")>]
            let replace (b: 'b) (fa: Maybe<'a>) : Maybe< ^b> =
                match fa with Nothing -> Nothing | Just _ -> Just b

            [<CompiledName("Tee")>]
            let inline tee f (g: ^a -> ^b -> unit) fa =
                match fa with
                | Nothing -> Nothing
                | Just a  -> let b = f a in g a b; Just b


        module Semigroup =

            [<CompiledName("SAppend")>]
            let inline sappend e1 e2 : Maybe< ^a> when ^a: (static member Append: ^a -> ^a -> ^a) =
                match e1 with
                | Nothing -> e2
                | Just a -> match e2 with
                            | Nothing -> e2
                            | Just b -> Just (^a: (static member Append: ^a -> ^a -> ^a) (a, b))


        module Monoid =

            [<CompiledName("MAppend")>]
            let inline mappend e1 e2 : Maybe< ^a> when ^a: (static member Append: ^a -> ^a -> ^a) =
                Semigroup.sappend e1 e2

            [<CompiledName("MEmpty")>]
            let mempty<'a> : Maybe< ^a> = Nothing

            [<CompiledName("MTimes")>]
            let inline mtimes n e =
                match e with
                | Nothing -> Nothing
                | Just a ->
                    match max 0 n with
                    | 0 -> Nothing
                    | n -> let mutable r : ^a = a
                           for i = 1 to n do
                               r <- (^a: (static member Append: ^a -> ^a -> ^a) (a, r))
                           Just r

            [<CompiledName("MConcat")>]
            let inline mconcat (source: Maybe< ^a> seq) =
                match source with
                | s when Seq.isEmpty s -> Nothing
                | :? array<Maybe< ^a>> as s -> Array.reduceBack mappend s
                | :? list<Maybe< ^a>>  as s -> List.reduceBack  mappend s
                | _ -> Seq.reduceBack mappend source



    let maybe = Compose.Monad.MaybeBuilder ()



//open Maybe
//open Compose


//type Maybe<'T> with

//// @ Monad @

//    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
//    static member inline ( >>= ) (m, k) = Monad.bind k m
//    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
//    static member inline ( =<< ) (k, m) = Monad.bind k m

//// @ Applicative @

//    /// Sequential application on effects.
//    static member inline ( <*> ) (ff, fx) = Applicative.ap fx ff
//    /// Sequential application on effects.
//    static member inline ( <**> ) (fx, ff) = Applicative.ap fx ff

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
//    static member inline ( |%> ) (fa, f) = Functor.map f fa
//    /// Lift a function onto effects.
//    static member inline ( <%| ) (f, fa) = Functor.map f fa

//    /// Replace all locations in the input with the same value.
//    static member inline ( %> ) (fa, b) = Functor.replace b fa
//    /// Replace all locations in the input with the same value.
//    static member inline ( <% ) (b, fa) = Functor.replace b fa

//// @ Semigroup @

//    /// An associative composition operation.
//    static member inline Append (e1, e2) = Semigroup.sappend e1 e2

//// @ Monoid @

//    /// The identity element for the composition operator.
//    static member inline Empty () : Maybe< ^a> = Nothing