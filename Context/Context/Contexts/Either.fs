namespace PTR.Context.Type.Either


[<Struct>]
type Either<'L, 'R> = Left of L: 'L | Right of R: 'R with

    member inline s.Match (onLeft: System.Func< ^L, ^T>, onRight: System.Func< ^R , ^T>) : ^T =
        match s with
        | Right r -> onRight.Invoke(r)
        | Left l -> onLeft.Invoke(l)        

    static member inline Unit(x: ^a) : Either< ^e, ^a> = Right x

    member inline s.Select(f: System.Func< ^R, ^S>) : Either< ^L, ^S> =
        match s with Right a -> Right (f.Invoke(a)) | Left e -> Left e

    member inline s.Select2 (second: Either< ^L, ^S>, f: System.Func< ^R, ^S, ^T>) : Either< ^L, ^T> =
        match s, second with
        | Right a, Right b -> Right (f.Invoke(a, b))
        | Left e, _ | _, Left e -> Left e
    
    member inline s.SelectMany (f: System.Func< ^R, Either< ^L, ^S>>) : Either< ^L, ^S> =
        match s with Right a -> f.Invoke(a) | Left e -> Left e

    member inline s.SelectMany(f: System.Func< ^R, Either< ^L, ^S>>, g: System.Func< ^R, ^S, ^T>) : Either< ^L, ^T> =
        match s with
        | Left e -> Left e
        | Right a -> match f.Invoke(a) with
                     | Right b -> Right (g.Invoke(a, b))
                     | Left e -> Left e

    member inline s.Join(t: Either< ^L, ^S>, kt: System.Func< ^R, ^K>, ku: System.Func< ^S, ^K>, rs: System.Func< ^R, ^S, ^T>) : Either< ^L, ^T> =
        s.Select2(t, rs)

    member inline s.OrElse (second: Either< ^L, ^R>) : Either< ^L, ^R> =
        match s with Right _ -> s | Left _ -> second

    static member inline Append (first: Either< ^e, ^a>, second: Either< ^e, ^a>) : Either< ^e, ^a> =
        match first, second with
        | Right a, Right b -> Right (^a : (static member Append: ^a -> ^a -> ^a) (a, b))
        | Left e, _ | _, Left e -> Left e


module Either =

// Primitives

    let inline case onLeft onRight (either: Either< ^e, ^a>) : ^r =
        match either with
        | Right a -> onRight a
        | Left e -> onLeft e

    [<CompiledName("IsLeft")>]
    let isLeft (either: Either<'a, 'b>) =
        match either with
        | Right _ -> false
        | Left _ -> true

    [<CompiledName("IsRight")>]
    let isRight (either: Either<'a, 'b>) =
        match either with        
        | Right _ -> true
        | Left _ -> false

    [<CompiledName("FromLeft")>]
    let fromLeft (def: 'a) (either: Either< ^a, 'b>) : ^a =
        match either with
        | Right _ -> def
        | Left l -> l        

    [<CompiledName("FromLeftWith")>]
    let inline fromLeftWith (defThunk: unit -> ^a) (either: Either< ^a, ^b>) : ^a =
        match either with
        | Right _ -> defThunk ()
        | Left l -> l

    [<CompiledName("FromRight")>]
    let fromRight (def: 'b) (either: Either<'a, ^b>) : ^b =
        match either with
        | Right r -> r
        | Left _ -> def

    [<CompiledName("FromRightWith")>]
    let inline fromRightWith (defThunk: unit -> ^b) (either: Either< ^a, ^b>) : ^b =
        match either with
        | Right r -> r
        | Left _ -> defThunk ()


// Isomorphisms

    //[<CompiledName("ToArray")>]
    //let inline toArray (either: Either< ^e, ^a>) : ^a [] =
    //    match either with        
    //    | Right a -> Array.singleton a
    //    | Left _ -> Array.empty

    //[<CompiledName("ToList")>]
    //let inline toList (either: Either< ^e, ^a>) : ^a list =
    //    match either with        
    //    | Right a -> List.singleton a
    //    | Left _ -> List.empty

    [<CompiledName("ToSeq")>]
    let inline toSeq (either: Either< ^e, ^a>) : seq< ^a> =
        match either with
        | Right a -> Seq.singleton a
        | Left _ -> Seq.empty        

    [<CompiledName("ToChoice")>]
    let inline toChoice (either: Either< ^e, ^a>) : Choice< ^a, ^e> =
        match either with
        | Right a -> Choice1Of2 a
        | Left e -> Choice2Of2 e

    [<CompiledName("OfChoice")>]
    let inline ofChoice (choice: Choice< ^a, ^e>) : Either< ^e, ^a> =
        match choice with
        | Choice1Of2 a -> Right a
        | Choice2Of2 e -> Left e

    [<CompiledName("ToResult")>]
    let inline toResult (either: Either< ^e, ^a>) : Result< ^a, ^e> =
        match either with
        | Right a -> Ok a
        | Left e -> Error e

    [<CompiledName("OfResult")>]
    let inline ofResult (result: Result< ^a, ^e>) : Either< ^e, ^a> =
        match result with
        | Ok a -> Right a
        | Error e -> Left e


// Collections

    [<CompiledName("Lefts")>]
    let lefts (eithers: #seq<Either<'a, 'b>>) : ^a seq =
        seq { for x in eithers do
                  match x with
                  | Right _ -> ()
                  | Left a -> yield a }

    [<CompiledName("Rights")>]
    let rights (eithers: #seq<Either<'a, 'b>>) : ^b seq =
        seq { for x in eithers do
                  match x with
                  | Right b -> yield b
                  | Left _ -> () }

    [<CompiledName("Partition")>]
    let partition (eithers: #seq<Either<'a, 'b>>) : ^a seq * ^b seq =
        let xs = Seq.cache eithers
        seq { for x in xs do
                  match x with
                  | Right _ -> ()
                  | Left a -> yield a },
        seq { for x in xs do
                  match x with
                  | Right b -> yield b
                  | Left _ -> () }

// Monad

    let unit (x: 'a) : Either<'e, ^a> = Right x

    [<CompiledName("Bind")>]
    let inline bind f (m: Either< ^e, ^a>) : Either< ^e, ^b> =
        match m with
        | Right a -> f a
        | Left e -> Left e

    [<CompiledName("Flatten")>]
    let flatten (mm: Either<'e, Either< ^e, 'a>>) : Either< ^e, ^a> =
        match mm with
        | Right m -> m
        | Left e -> Left e

    [<CompiledName("RecM")>]
    let inline recM f (x: ^a) : Either< ^e, ^b> =
        let rec go m = bind j m
        and k a = go (unit a)
        and j a = f k a
        j x

    [<CompiledName("RecM1")>]
    let inline recM1 f (x: ^a) : Either< ^e, ^b> =
        let rec go m = bind k m
        and k a = f go a
        k x

    
    module Workflow =

        type EitherBuilder () =
            member inline _.Return(x: ^a) : Either< ^e, ^a> = unit x
            member inline _.ReturnFrom (m: Either< ^e, ^a>) : Either< ^e, ^a> = m
            member inline _.Bind (m: Either< ^e, ^a>, f) = bind f m
            
            member inline _.Zero() : Either< ^e, unit> = unit ()

            member inline _.Using (disp: ^d, body: ^d -> Either< ^e, ^a>) : Either< ^e, ^a> when ^d :> System.IDisposable =
                using disp body

            member inline _.TryWith(body, handler) : Either< ^e, ^a> =
                try body with e -> handler e
            member inline _.TryFinally(body, finalizer) : Either< ^e, ^a> =
                try body finally finalizer ()

            member inline _.While(guard, body) : Either< ^e, unit> =
                let rec go = function
                | false -> unit ()
                | true  -> bind k (body ())
                and k () = go (guard ()) in k ()

            member inline _.For(seq: #seq< ^a>, body) : Either< ^e, unit> =
                use e = seq.GetEnumerator()
                let rec go = function
                | false -> unit ()
                | true  -> b e.Current
                and b x = bind k (body x)
                and k () = go (e.MoveNext()) in k ()


    let either = Workflow.EitherBuilder()


// Applicative

    [<CompiledName("Ap")>]
    let inline ap (fv: Either< ^e, ^a>) (ff: Either< ^e, (^a -> ^b)>) : Either< ^e, ^b> =
        match ff, fv with
        | Right f, Right v -> Right (f v)
        | Left e, _ | _, Left e -> Left e        

    [<CompiledName("Map2")>]
    let inline map2  (f: ^a -> ^b -> ^c) (fa: Either< ^e, ^a>) (fb: Either< ^e, ^b>) : Either< ^e, ^c> =
        match fa, fb with
        | Right a, Right b -> Right (f a b)
        | Left e, _ | _, Left e -> Left e

    [<CompiledName("AndThen")>]
    let andThen (second: Either<'e, 'b>) (first: Either< ^e, 'a>) : Either< ^e, ^b> =
        match first with
        | Right _ -> second
        | Left e -> Left e

    [<CompiledName("When")>]
    let inline when_ condition f : Either< ^e, unit> =
        if condition then f () else unit ()


// Alternative

    let orElse (second: Either<'e, 'a>) (first: Either< ^e, ^a>) : Either< ^e, ^a> =
      match first with
      | Right _ -> first
      | Left _ -> second

    let inline orElseWith (second: unit -> Either< ^e, ^a>) (first: Either< ^e, ^a>) : Either< ^e, ^a> =
      match first with
      | Right _ -> first
      | Left _ -> second ()


// Functor

    [<CompiledName("Map")>]
    let inline map (f: ^a -> ^b) (fa: Either< ^e, ^a>) : Either< ^e, ^b> =
        match fa with
        | Right a -> Right (f a)
        | Left e -> Left e


// Bifunctor

    [<CompiledName("Bimap")>]
    let inline bimap (f: ^a -> ^c) (g: ^b -> ^d) (bf: Either< ^a, ^b>) : Either< ^c, ^d> =
        match bf with        
        | Right b -> Right (g b)
        | Left a -> Left (f a)

    [<CompiledName("MapFst")>]
    let inline mapFst (f: ^a -> ^c) (bf: Either< ^a, ^b>) : Either< ^c, ^b> =
        match bf with
        | Right b -> Right b
        | Left a -> Left (f a)        

    [<CompiledName("MapSnd")>]
    let inline mapSnd (g: ^b -> ^c) (bf: Either< ^a, ^b>) : Either< ^a, ^c> =
        match bf with
        | Right b -> Right (g b)
        | Left a -> Left a


// Semigroup

    let append (first: Either<'e, 'a>) (second: Either< ^e, ^a>) : Either< ^e, ^a> =
        match first with        
        | Right _ -> first
        | Left _ -> second


// Traversable

    [<CompiledName("Sequence")>]
    let inline sequence (source: #seq<Either< ^e, ^a>>) : Either< ^e, seq< ^a>> =
        use e = source.GetEnumerator()
        let mutable b = true
        let mutable er = Unchecked.defaultof< ^e>
        let ra = ResizeArray< ^a>()
        while b && e.MoveNext() do
            match e.Current with
            | Right a -> ra.Add(a)
            | Left er' -> b <- false ; er <- er'
        if b then Right (System.Linq.Enumerable.AsEnumerable(ra)) else Left er
            
    [<CompiledName("Traverse")>]
    let inline traverse (f: ^a -> Either< ^e, ^b>) (source: #seq< ^a>) : Either< ^e, seq< ^b>> =
        sequence (System.Linq.Enumerable.Select(source, f))


// Foldable

    [<CompiledName("Fold")>]
    let inline fold folder seed (source: Either< ^e, ^a>) : ^s =
        match source with
        | Right a -> folder seed a
        | Left _ -> seed        

    [<CompiledName("FoldBack")>]
    let inline foldBack folder seed (source: Either< ^e, ^a>) : ^s =
        match source with
        | Right a -> folder a seed
        | Left _ -> seed

    [<CompiledName("Foldl")>]
    let inline foldl folder seed (source: Either< ^e, ^a>) : ^s =
        match source with
        | Right a -> folder seed a
        | Left _ -> seed ()        
    
    [<CompiledName("Foldr")>]
    let inline foldr folder seed (source: Either< ^e, ^a>) : ^s =
        match source with
        | Right a -> folder a seed
        | Left _ -> seed ()

    [<CompiledName("Foldm")>]
    let inline foldm f (source: Either< ^e, ^a>) : ^m when ^m : (static member Append: ^m -> ^m -> ^m) =
        match source with
        | Right a -> f a
        | Left _ -> (^m : (static member Empty: unit -> ^m) ())

    [<CompiledName("MapFold")>]
    let inline mapFold mapping (seed: ^s) (source: Either< ^e, ^a>) : Either< ^e, ^b> * ^s =
        match source with
        | Right a -> let r, s = mapping seed a in Right r, s
        | Left e -> Left e, seed

    [<CompiledName("MapFoldBack")>]
    let inline mapFoldBack mapping (seed: ^s) (source: Either< ^e, ^a>) : Either< ^e, ^b> * ^s =
        match source with
        | Right a -> let r, s = mapping a seed in Right r, s
        | Left e -> Left e, seed


// Bifoldable

    [<CompiledName("Bifold")>]
    let inline bifold (fold1: ^s -> ^a -> ^s) (fold2: ^s -> ^b -> ^s) (seed: ^s) (source: Either< ^a, ^b>) : ^s =
        match source with
        | Right b -> fold2 seed b
        | Left a -> fold1 seed a

    [<CompiledName("BifoldBack")>]
    let inline bifoldBack (fold1: ^a -> ^s -> ^s) (fold2: ^b -> ^s -> ^s) (seed: ^s) (source: Either< ^a, ^b>) : ^s =
        match source with
        | Right b -> fold2 b seed
        | Left a -> fold1 a seed

    [<CompiledName("Bifoldl")>]
    let inline bifoldl (fold1: (unit -> ^s) -> ^a -> ^s) (fold2: (unit -> ^s) -> ^b -> ^s) (seed: unit -> ^s) (source: Either< ^a, ^b>) : ^s =
        match source with
        | Right b -> fold2 seed b
        | Left a -> fold1 seed a

    [<CompiledName("Bifoldr")>]
    let inline bifoldr (fold1: ^a -> (unit -> ^s) -> ^s) (fold2: ^b -> (unit -> ^s) -> ^s) (seed: unit -> ^s) (source: Either< ^a, ^b>) : ^s =
        match source with
        | Right b -> fold2 b seed
        | Left a -> fold1 a seed

    [<CompiledName("Bifoldm")>]
    let inline bifoldm (f1: ^a -> ^m) (f2: ^b -> ^m) (source: Either< ^a, ^b>)
        : ^m when ^m : (static member Append: ^m -> ^m -> ^m) =
        match source with
        | Right b -> f2 b
        | Left a -> f1 a

    [<CompiledName("BimapFold")>]
    let inline bimapFold (mapping1: ^s -> ^a -> ^b * ^s) (mapping2: ^s -> ^c -> ^d * ^s) (seed: ^s) (source: Either< ^a, ^c>) : Either< ^b, ^d> * ^s =
        match source with
        | Right b -> let r, s = mapping2 seed b in Right r, s
        | Left a -> let r, s = mapping1 seed a in Left r, s

    [<CompiledName("BimapFoldBack")>]
    let inline bimapFoldBack (mapping1: ^a -> ^s -> ^b * ^s) (mapping2: ^c -> ^s -> ^d * ^s) (seed: ^s) (source: Either< ^a, ^c>) : Either< ^b, ^d> * ^s =
        match source with
        | Right b -> let r, s = mapping2 b seed in Right r, s
        | Left a -> let r, s = mapping1 a seed in Left r, s