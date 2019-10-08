namespace PTR.Context.Type.Maybe


[<Struct>]
type Maybe<'T> = Nothing | Just of 'T with

    member inline s.Match(onNothing: System.Func< ^U>, onJust: System.Func< ^T, ^U>) =
        match s with Just a -> onJust.Invoke(a) | Nothing -> onNothing.Invoke()

    static member inline Unit(x: ^a) = Just x
    
    member inline s.Select(f: System.Func< ^T, ^U>) : Maybe< ^U> =
        match s with Just a -> Just (f.Invoke(a)) | Nothing -> Nothing

    member inline s.Select2(second: Maybe< ^U>, f: System.Func< ^T, ^U, ^V>) : Maybe< ^V> =
        match s, second with Just a, Just b -> Just (f.Invoke(a, b))
                           | Nothing, _ | _, Nothing -> Nothing

    member inline s.SelectMany(f: System.Func< ^T, Maybe< ^U>>) : Maybe< ^U> =
        match s with Just a -> f.Invoke(a) | Nothing -> Nothing

    member inline s.SelectMany(f: System.Func< ^T, Maybe< ^U>>, g: System.Func< ^T, ^U, ^V>) : Maybe< ^V> =
        match s with
        | Just a -> match f.Invoke(a) with Just b -> Just (g.Invoke(a, b)) | Nothing -> Nothing
        | Nothing -> Nothing

    member inline s.Join(t: Maybe< ^U>, kt: System.Func< ^T, ^K>, ku: System.Func< ^U, ^K>, rs: System.Func< ^T, ^U, ^V>) : Maybe< ^V> when ^K : equality =
        match s, t with
        | Just a, Just b -> if kt.Invoke(a) = ku.Invoke(b) then Just (rs.Invoke(a, b)) else Nothing
        | Nothing, _ | _, Nothing -> Nothing

    member inline s.Where(p: System.Func< ^T, bool>) : Maybe< ^T> =
        match s with Just a -> if p.Invoke(a) then s else Nothing
                   | Nothing -> Nothing

    member inline s.OrElse second : Maybe< ^T> =
        match s with Just _ -> s | Nothing -> second

    static member inline Append (first, second) : Maybe< ^a> =
        match first with Just _ -> first | Nothing -> second

    static member inline Empty () : Maybe< ^a> = Nothing


module Maybe =

// Primitives

    let inline case onNothing onJust (maybe: Maybe< ^a>) : ^b =
        match maybe with
        | Just a -> onJust a
        | Nothing -> onNothing ()

    [<CompiledName("IsJust")>]
    let isJust (maybe: Maybe<'T>) =
        match maybe with
        | Just _ -> true
        | Nothing -> false

    [<CompiledName("IsNothing")>]
    let isNothing (maybe: Maybe<'T>) =
        match maybe with
        | Just _ -> false
        | Nothing -> true

    [<CompiledName("FromMaybe")>]
    let inline fromMaybe (def: ^b) (f: ^a -> ^b) (maybe: Maybe< ^a>) =
        match maybe with
        | Just a -> f a
        | Nothing -> def

    [<CompiledName("FromMaybeWith")>]
    let inline fromMaybeWith (def: unit -> ^b) (f: ^a -> ^b) (maybe: Maybe< ^a>) =
        match maybe with
        | Just a -> f a
        | Nothing -> def ()

    [<CompiledName("DefaultValue")>]
    let defaultValue (def: 'a) (maybe: Maybe< ^a>) =
        match maybe with
        | Just a -> a
        | Nothing -> def

    [<CompiledName("DefaultWith")>]
    let inline defaultWith (defThunk: unit -> ^a) (maybe: Maybe< ^a>) =
        match maybe with
        | Just a -> a
        | Nothing -> defThunk ()

    [<CompiledName("Justs")>]
    let justs (maybes: #seq<Maybe<'a>>) : seq< ^a> =
        seq { for x in maybes do
                match x with
                | Nothing -> ()
                | Just x -> yield x }


// Isomorphisms

    //[<CompiledName("ToArray")>]
    //let inline toArray (maybe: Maybe< ^a>) : ^a [] =
    //    match maybe with
    //    | Just a -> Array.singleton a
    //    | Nothing -> Array.empty
        
    //[<CompiledName("ToList")>]
    //let inline toList (maybe: Maybe< ^a>) : ^a list =
    //    match maybe with
    //    | Just a -> List.singleton a
    //    | Nothing -> List.empty

    [<CompiledName("ToSeq")>]
    let inline toSeq (maybe: Maybe< ^a>) : seq< ^a> =
        match maybe with
        | Just a -> Seq.singleton a
        | Nothing -> Seq.empty

    [<CompiledName("ToOption")>]
    let inline toOption (maybe: Maybe< ^a>) : Option< ^a> =
        match maybe with
        | Just a -> Some a
        | Nothing -> None

    [<CompiledName("OfOption")>]
    let inline ofOption (option: Option< ^a>) : Maybe< ^a> =
        match option with
        | Some a -> Just a
        | None -> Nothing

    [<CompiledName("ToVOption")>]
    let inline toVOption (maybe: Maybe< ^a>) : ValueOption< ^a> =
        match maybe with
        | Just a -> ValueSome a
        | Nothing -> ValueNone

    [<CompiledName("OfVOption")>]
    let inline ofVOption (voption: ValueOption< ^a>) : Maybe< ^a> =
        match voption with
        | ValueSome a -> Just a
        | ValueNone -> Nothing

    [<CompiledName("ToNullable")>]
    let inline toNullable (maybe: Maybe< ^a>) : System.Nullable< ^a> =
        match maybe with
        | Just a -> System.Nullable< ^a>(a)
        | Nothing -> System.Nullable< ^a>()

    [<CompiledName("OfNullable")>]
    let inline ofNullable (nullable: System.Nullable< ^a>) : Maybe< ^a> =
        if nullable.HasValue then Just nullable.Value
        else Nothing

    [<CompiledName("OfObj")>]
    let inline ofObj (obj: ^a) : Maybe< ^a> when ^a : null =
        if isNull obj then Nothing else Just obj


// Monad

    let unit (x: 'a) : Maybe< ^a> = Just x

    [<CompiledName("Bind")>]
    let inline bind (f: ^a -> Maybe< ^b>) (m: Maybe< ^a>) : Maybe< ^b> =
        match m with
        | Just a -> f a
        | Nothing -> Nothing

    [<CompiledName("Flatten")>]
    let flatten (mm: Maybe<Maybe<'a>>) : Maybe< ^a> =
        match mm with
        | Just m -> m
        | Nothing -> Nothing

    [<CompiledName("RecM")>]
    let inline recM (f: (^a -> Maybe< ^b>) -> ^a -> Maybe< ^b>) (x: ^a) : Maybe< ^b> =
        let rec go = function Just a -> j a | Nothing -> Nothing
        and k a = go (Just a)
        and j a = f k a
        j x

    [<CompiledName("RecM1")>]
    let inline recM1 (f: (Maybe< ^a> -> Maybe< ^b>) -> ^a -> Maybe< ^b>) (x: ^a) : Maybe< ^b> =
        let rec go = function Just a -> k a | Nothing -> Nothing
        and k a = f go a
        k x

    
    module Workflow =

        type MaybeBuilder () =
            member inline _.Return(x: ^a) : Maybe< ^a> = unit x
            member inline _.ReturnFrom (m: Maybe< ^a>) : Maybe< ^a> = m
            member inline _.Bind (m: Maybe< ^a>, f: ^a -> Maybe< ^b>) = bind f m
            
            member inline _.Zero() = unit ()

            member inline _.Using(disp: ^d, body: ^d -> Maybe< ^a>) : Maybe< ^a> when ^d :> System.IDisposable =
                using disp body

            member inline _.TryWith (body: Maybe< ^a>, handler: exn -> Maybe< ^a>) : Maybe< ^a> =
                try body with e -> handler e
            member inline _.TryFinally (body: Maybe< ^a>, finalizer: unit -> unit) : Maybe< ^a> =
                try body finally finalizer ()

            member inline _.While(guard, body) : Maybe<unit> =                    
                let rec go = function
                | false -> unit ()
                | true  -> bind k (body ())
                and k () = go (guard ()) in k ()

            member inline _.For(seq: #seq< ^a>, body) : Maybe<unit> =
                use e = seq.GetEnumerator()
                let rec go = function
                | false -> unit ()
                | true  -> b e.Current
                and b x = bind k (body x)
                and k () = go (e.MoveNext()) in k ()


    let maybe = Workflow.MaybeBuilder()


// MonadPlus

    let empty<'a> : Maybe<'a> = Nothing

    [<CompiledName("Guard")>]
    let guard (condition: bool) : Maybe<unit> = if condition then Just () else Nothing

    let inline join (joiner: ^a -> ^b -> ^c) (k1: ^a -> ^k) (k2: ^b -> ^k) (ma: Maybe< ^a>) (mb: Maybe< ^b>) : Maybe< ^c> when ^k : equality =
        match ma, mb with
        | Just a, Just b -> if k1 a = k2 b then Just (joiner a b) else Nothing
        | Nothing, _ | _, Nothing -> Nothing


    let inline joinBy (joiner: ^a -> ^b -> ^c) (matcher: ^a -> ^b -> bool) (ma: Maybe< ^a>) (mb: Maybe< ^b>) : Maybe< ^c> =
        match ma, mb with
        | Just a, Just b -> if matcher a b then Just (joiner a b) else Nothing
        | Nothing, _ | _, Nothing -> Nothing


// General

    [<CompiledName("Filter")>]
    let inline filter (p: ^a -> bool) (m: Maybe< ^a>) : Maybe< ^a> =
        match m with
        | Just a -> if p a then m else Nothing
        | Nothing -> Nothing


// Applicative

    [<CompiledName("Ap")>]
    let inline ap (fv: Maybe< ^a>) (ff: Maybe<(^a -> ^b)>) : Maybe< ^b> =
        match ff, fv with
        | Just f, Just v -> Just (f v)
        | Nothing, _ | _, Nothing -> Nothing

    [<CompiledName("Map2")>]
    let inline map2 (f: ^a -> ^b -> ^c) (fa: Maybe< ^a>) (fb: Maybe< ^b>) : Maybe< ^c> =
        match fa, fb with
        | Nothing, _ | _, Nothing -> Nothing
        | Just a, Just b -> Just (f a b)

    [<CompiledName("AndThen")>]
    let andThen (second: Maybe<'b>) (first: Maybe<'a>) : Maybe< ^b> =
        match first with
        | Nothing -> Nothing
        | Just _ -> second

    [<CompiledName("When")>]
    let inline when_ condition f : Maybe<unit> = if condition then f () else Nothing


// Alternative

    let orElse (second: Maybe<'a>) (first: Maybe< ^a>) : Maybe< ^a> =
      match first with
      | Just _ -> first
      | Nothing -> second

    let inline orElseWith (second: unit -> Maybe< ^a>) (first: Maybe< ^a>) : Maybe< ^a> =
      match first with
      | Just _ -> first
      | Nothing -> second ()


// Functor

    [<CompiledName("Map")>]
    let inline map (f: ^a -> ^b) (fa: Maybe< ^a>) : Maybe< ^b> =
        match fa with
        | Just a -> Just (f a)
        | Nothing -> Nothing


// Semigroup

    let append (first: Maybe<'a>) (second: Maybe< ^a>) : Maybe< ^a> =
        match first with
        | Just _ -> first
        | Nothing -> second        


// Monoid

    [<CompiledName("MTimes")>]
    let mtimes count (elem: Maybe<'a>) : Maybe< ^a> =
        match elem with
        | Nothing -> Nothing
        | Just _ -> if count <= 0 then Nothing else elem

    [<CompiledName("MConcat")>]
    let inline mconcat (source: #seq<Maybe< ^a>>) =
        let xs = justs source
        if Seq.isEmpty xs then Nothing else Just (Seq.head xs)


// Foldable

    [<CompiledName("Fold")>]
    let inline fold folder seed (source: Maybe< ^a>) : ^s =
        match source with
        | Nothing -> seed
        | Just a -> folder seed a

    [<CompiledName("FoldBack")>]
    let inline foldBack folder seed (source: Maybe< ^a>) : ^s =
        match source with
        | Nothing -> seed
        | Just a -> folder a seed

    [<CompiledName("Foldl")>]
    let inline foldl folder seed (source: Maybe< ^a>) : ^s =
        match source with
        | Nothing -> seed ()
        | Just a -> folder seed a
    
    [<CompiledName("Foldr")>]
    let inline foldr folder seed (source: Maybe< ^a>) : ^s =
        match source with
        | Nothing -> seed ()
        | Just a -> folder a seed

    [<CompiledName("Foldm")>]
    let inline foldm f (source: Maybe< ^a>) : ^m when ^m : (static member Append: ^m -> ^m -> ^m) =
        match source with
        | Just a -> f a
        | Nothing -> (^m : (static member Empty: unit -> ^m) ())

    [<CompiledName("MapFold")>]
    let inline mapFold mapping (seed: ^s) (source: Maybe< ^a>) : Maybe< ^b> * ^s =
        match source with
        | Nothing -> Nothing, seed
        | Just a -> let r, s = mapping seed a in Just r, s

    [<CompiledName("MapFoldBack")>]
    let inline mapFoldBack mapping (seed: ^s) (source: Maybe< ^a>) : Maybe< ^b> * ^s =
        match source with
        | Nothing -> Nothing, seed
        | Just a -> let r, s = mapping a seed in Just r, s


// Traversable

    [<CompiledName("Sequence")>]
    let inline sequence (source: #seq<Maybe< ^a>>) : Maybe<seq< ^a>> =
        use e = source.GetEnumerator()
        let mutable b = true
        let ra = ResizeArray< ^a>()
        while b && e.MoveNext() do
            match e.Current with
            | Nothing -> b <- false ; ra.Clear()
            | Just a -> ra.Add(a)
        if b then Just (System.Linq.Enumerable.AsEnumerable(ra)) else Nothing

    [<CompiledName("Traverse")>]
    let inline traverse (f: ^a -> Maybe< ^b>) (source: #seq< ^a>) : Maybe<seq< ^b>> =
        sequence (System.Linq.Enumerable.Select(source, f))
