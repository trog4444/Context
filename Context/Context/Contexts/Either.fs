namespace Rogz.Context.Data.Either


module Either =

// Primitives

    let inline caseof onLeft onRight (either: Either< ^l, ^r>) : ^a =
        match either with Right r -> onRight r | Left l -> onLeft l

    let isLeft (either: Either<'l, 'r>) =
        match either with Left _ -> true | Right _ -> false

    let isRight (either: Either<'l, 'r>) =
        match either with Left _ -> false | Right _ -> true

    let rights (source: Either<'l, 'r> seq) =
        seq { for x in source do
                 match x with
                 | Left _ -> ()
                 | Right y -> yield y }

    let lefts (source: Either<'l, 'r> seq) =
        seq { for x in source do
                  match x with
                  | Left y -> yield y
                  | Right _ -> () }

    let partition (source: Either<'l, 'r> seq) =
        let source = Seq.cache source in struct (lefts source, rights source)


//// Isomorphisms

    //let toSeq (either: Either<'e, 'a>) =
    //    match either with Left _ -> Seq.empty | Right a -> Seq.singleton a

    //let toChoice either : Choice<'a, 'e> =
    //    match either with
    //    | Right a -> Choice1Of2 a
    //    | Left e  -> Choice2Of2 e

    //let ofChoice choice : Either<'e, 'a> =
    //    match choice with
    //    | Choice1Of2 a -> Right a
    //    | Choice2Of2 e -> Left e

    //let toResult either : Result<'a, 'e> =
    //    match either with
    //    | Right a -> Ok a
    //    | Left e  -> Error e

    //let ofResult result : Either<'e, 'a> =
    //    match result with
    //    | Ok a    -> Right a
    //    | Error e -> Left e


//// Recur

//    let inline recur (loop: ^a -> Either< ^a, ^b>) seed =
//        let rec go a =
//            match loop a with
//            | Left a  -> go a
//            | Right b -> b            
//        go seed


// Functor

    let inline map (f: ^a -> ^b) (fa: Either< ^e, ^a>) =
        match fa with Right a -> Right (f a) | Left e -> Left e


// Bifunctor

    let inline bimap (f: ^a -> ^c) (g: ^b -> ^d) (bf: Either< ^a, ^b>) =
        match bf with Right b -> Right (g b) | Left a -> Left (f a)

    let inline mapFirst (f: ^a -> ^c) (bf: Either< ^a, ^b>) =
        match bf with Right b -> Right b | Left a -> Left (f a)


// Applicative

    let unit (value: 'a) : Either<'e, ^a> = Right value

    let inline ap fv (ff: Either< ^e, (^a -> ^b)>) =
        match ff, fv with
        | Right f, Right v -> Right (f v)
        | Left e, _ -> Left e
        | _, Left e -> Left e

    let inline map2 (f: ^a -> ^b -> ^c) fa fb : Either< ^e, ^c> =
        match fa, fb with
        | Right a, Right b -> Right (f a b)
        | Left e, _ -> Left e
        | _, Left e -> Left e

    //let andthen fb (fa: Either<'e, 'a>) : Either< ^e, 'b> =
    //    match fa with Left e -> Left e | Right _ -> fb

    let sequence (source: #seq<Either<'e, 'a>>) =
        let xs = ResizeArray<_>()
        let mutable flag = true
        use e = source.GetEnumerator()
        let mutable er = Unchecked.defaultof<_>
        while flag && e.MoveNext() do
            match e.Current with
            | Right a -> xs.Add a
            | Left e  -> flag <- false; er <- e
        if flag then Right (System.Linq.Enumerable.AsEnumerable(xs))
        else Left er

    let inline traverse f (source: #seq< ^a>) : Either< ^e, ^b seq> =
        let xs = ResizeArray<_>()
        let mutable flag = true
        use e = source.GetEnumerator()
        let mutable er = Unchecked.defaultof<_>
        while flag && e.MoveNext() do
            match f e.Current with
            | Right a -> xs.Add a
            | Left e  -> flag <- false; er <- e
        if flag then Right (System.Linq.Enumerable.AsEnumerable(xs))
        else Left er


// Alternative

    let orElse second first : Either<'e, 'a> =
        match first with Right _ -> first | Left _ -> second

    let inline orElseWith second first : Either< ^e, ^a> =
        match first with Right _ -> first | Left _ -> second ()


// Monad    

    let inline bind f (ma: Either< ^e, ^a>) : Either< ^e, ^b> =
        match ma with Right a -> f a | Left e -> Left e

    let flatten mm : Either<'e, 'a> =
        match mm with Right m -> m | Left e -> Left e    

    let inline fixM loop (em: Choice< ^a, Either< ^e, ^a>>) : Either< ^e, ^b> =
        let rec go = function Right a -> k a | Left e -> Left e
        and k a = loop k go a
        match em with Choice1Of2 a -> k a | Choice2Of2 m -> go m

    //// foldlM
    //// foldrM

    [<RequireQualifiedAccess>]
    module Workflow =

        type EitherBuilder() =
            member _.Return(x) : Either<'e, 'a> = Right x
            member _.ReturnFrom(m) : Either<'e, 'a> = m
            member inline _.Bind(m: Either< ^e, ^a>, f) : Either< ^e, ^b> = bind f m
            member _.Zero() : Either<'e, unit> = Right ()
            //member inline _.Using(disp: ^d, f) : Either< ^e, ^a> when ^d :> System.IDisposable = using disp f
            //member inline _.TryWith(m, h) : Either< ^e, ^a> = try m with e -> h e
            //member inline _.TryFinally(m, f) : Either< ^e, ^a> = try m finally f ()
            //abstract member Using: disp: 'd * f: ('d -> Either<'e, 'a>) -> Either<'e, 'a> when 'd :> System.IDisposable
            //abstract member TryWith: m: Either<'e, 'a> * h: (exn -> Either<'e, 'a>) -> Either<'e, 'a>
            //abstract member TryFinally: m: Either<'e, 'a> * f: (unit -> unit) -> Either<'e, 'a>
            //member _.Using(disp: 'd, f) : Either<'e, 'a> when 'd :> System.IDisposable = using disp f
            //default _.TryWith(m, h) : Either<'e, 'a> = try m with e -> h e
            //default _.TryFinally(m, f) : Either<'e, 'a> = try m finally f ()


    let either = Workflow.EitherBuilder()


// Semigroup

    let inline append first second : Either< ^e, ^a> =
        match first, second with
        | Right a, Right b -> Right (^a : (static member Append: ^a -> ^a -> ^a) (a, b))
        | Left _ , _ -> second
        | _, Left _-> first


// Foldable

    let inline fold folder (seed: ^s) (ta: Either< ^e, ^a>) =
        match ta with Left _ -> seed | Right a -> folder seed a

    let inline foldBack folder (seed: ^s) (ta: Either< ^e, ^a>) =
        match ta with Left _ -> seed | Right a -> folder a seed

    //let inline foldl folder (seed: unit -> ^s) (ta: Either< ^e, ^a>) =
    //    match ta with Left _ -> seed () | Right a -> folder seed a

    //let inline foldr folder (seed: unit -> ^s) (ta: Either< ^e, ^a>) =
    //    match ta with Left _ -> seed () | Right a -> folder a seed

    let inline mapFold mapping (seed: ^s) (ta: Either< ^e, ^a>) : struct (Either< ^e, ^b> * ^s) =
        match ta with
        | Left e  -> Left e, seed
        | Right a -> let struct (r, s) = mapping seed a in Right r, s

    let inline mapFoldBack mapping (seed: ^s) (ta: Either< ^e, ^a>) : struct (Either< ^e, ^b> * ^s) =
        match ta with
        | Left e  -> Left e, seed
        | Right a -> let struct (r, s) = mapping a seed in Right r, s


// Bifoldable

    let inline bifold (fold1: ^s -> ^a -> ^s) (fold2: ^s -> ^b -> ^s) (seed: ^s) (t: Either< ^a, ^b>) : ^s =
        match t with
        | Right b -> fold2 seed b
        | Left a  -> fold1 seed a

    let inline bifoldBack (fold1: ^a -> ^s -> ^s) (fold2: ^b -> ^s -> ^s) (seed: ^s) (t: Either< ^a, ^b>) : ^s =
        match t with
        | Right b -> fold2 b seed
        | Left a  -> fold1 a seed

    //let inline bifoldl (fold1: (unit -> ^s) -> ^a -> ^s) (fold2: (unit -> ^s) -> ^b -> ^s) (seed: unit -> ^s) (t: Either< ^a, ^b>) : ^s =
    //    match t with
    //    | Right b -> fold2 seed b
    //    | Left a  -> fold1 seed a

    //let inline bifoldr (fold1: ^a -> (unit -> ^s) -> ^s) (fold2: ^b -> (unit -> ^s) -> ^s) (seed: unit -> ^s) (t: Either< ^a, ^b>) : ^s =
    //    match t with
    //    | Right b -> fold2 b seed
    //    | Left a  -> fold1 a seed

    let inline bimapFold (mapping1: ^s -> ^a -> struct (^b * ^s)) (mapping2: ^s -> ^c -> struct (^d * ^s)) (seed: ^s) (t: Either< ^a, ^c>) : struct (Either< ^b, ^d> * ^s) =
        match t with
        | Right b -> let struct (r, s) = mapping2 seed b in Right r, s
        | Left a  -> let struct (r, s) = mapping1 seed a in Left r, s

    let inline bimapFoldBack (mapping1: ^a -> ^s -> struct (^b * ^s)) (mapping2: ^c -> ^s -> struct (^d * ^s)) (seed: ^s) (t: Either< ^a, ^c>) : struct (Either< ^b, ^d> * ^s) =
        match t with
        | Right b -> let struct (r, s) = mapping2 b seed in Right r, s
        | Left a  -> let struct (r, s) = mapping1 a seed in Left r, s