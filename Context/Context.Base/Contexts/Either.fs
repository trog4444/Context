namespace Rogz.Context.Base


[<Struct>]
type Either<'L, 'R> = Left of left: 'L | Right of right: 'R
with

// Union Type

    member this.TryLeft([<System.Runtime.InteropServices.Out>] value: outref<_>) =
        match this with
        | Left a  -> value <- a; true
        | Right _ -> false

    member this.TryRight([<System.Runtime.InteropServices.Out>] value: outref<_>) =
        match this with
        | Left _  -> false
        | Right a -> value <- a; true

    member this.Match(ifLeft: System.Func<'L, 'T>, ifRight: System.Func<'R, 'T>) =
        match this with
        | Left a  -> ifLeft.Invoke(a)
        | Right b -> ifRight.Invoke(b)

    member this.Match(ifLeft: System.Action<'L>, ifRight: System.Action<'R>) =
        match this with
        | Left a  -> ifLeft.Invoke(a)
        | Right b -> ifRight.Invoke(b)

// Alternative

    //[<CompiledName("OrElse")>]
    //static member ( <|> ) (first, second) : Either<'A,'B> =
    //    match first with
    //    | Left _ -> second
    //    | _      -> first

// Semigroup
    
    //[<CompiledName("Append")>]
    static member inline ( + ) (first, second) : Either< ^A, ^B> =
        match first, second with
        | Right a, Right b -> Right (a + b)
        | Left _, _        -> second
        | _, Left _        -> first        


module Either =

// Primitives

    let inline caseof ifLeft ifRight (either: Either< ^a, ^b>) : ^c =
        match either with
        | Left l  -> ifLeft l
        | Right r -> ifRight r

    let lefts (source: Either<'l, 'r> seq) =
        seq { for x in source do
                  match x with
                  | Left y  -> yield y
                  | Right _ -> () }

    let rights (source: Either<'l, 'r> seq) =
        seq { for x in source do
                 match x with
                 | Left _  -> ()
                 | Right y -> yield y }

    let isLeft (either: Either<'l, 'r>) =
        match either with
        | Left _  -> true
        | Right _ -> false

    let isRight (either: Either<'l, 'r>) =
        match either with
        | Left _  -> false
        | Right _ -> true

    let fromLeft defaultValue (either: Either<'e,'a>) =
        match either with
        | Left e  -> e
        | Right _ -> defaultValue

    let fromRight defaultValue (either: Either<'e,'a>) =
        match either with
        | Left _  -> defaultValue
        | Right a -> a

    let partition (source: Either<'l, 'r> seq) = struct (lefts source, rights source)


// Functor

    let inline map (f: ^a -> ^b) (fa: Either< ^e, ^a>) =
        match fa with
        | Left e  -> Left e
        | Right a -> Right (f a)


// Bifunctor

    let inline bimap (f: ^a -> ^c) (g: ^b -> ^d) (bf: Either< ^a, ^b>) =
        match bf with
        | Left a  -> Left (f a)
        | Right b -> Right (g b)

    let inline mapFirst (f: ^a -> ^c) (bf: Either< ^a, ^b>) =
        match bf with
        | Left a  -> Left (f a)
        | Right b -> Right b


// Applicative

    [<CompiledName("Unit")>]
    let unit (value: 'a) : Either<'e, ^a> = Right value

    let inline ap fv (ff: Either< ^e, (^a -> ^b)>) =
        match ff, fv with
        | Left e, _        -> Left e
        | _, Left e        -> Left e
        | Right f, Right v -> Right (f v)        

    let inline map2 (f: ^a -> ^b -> ^c) fa fb : Either< ^e, ^c> =
        match fa, fb with        
        | Left e, _        -> Left e
        | _, Left e        -> Left e
        | Right a, Right b -> Right (f a b)

    let sequence (source: #seq<Either<'e, 'a>>) =
        let xs = ResizeArray<_>()
        use e = source.GetEnumerator()
        let rec go () =
            if e.MoveNext() then
                match e.Current with
                | Left e  -> Left e
                | Right a -> xs.Add(a); go ()
            else Right (xs :> seq<_>)
        go ()

    let inline traverse f (source: #seq< ^a>) : Either< ^e, ^b seq> =
        let xs = ResizeArray<_>()
        use e = source.GetEnumerator()
        let rec go () =
            if e.MoveNext() then
                match f e.Current with
                | Left e  -> Left e
                | Right a -> go (xs.Add(a))
            else Right (xs :> seq<_>)
        go ()


// Alternative

    let orElse second first : Either<'e, 'a> =
        match first with
        | Left _  -> second
        | Right _ -> first

    let orElseWith second first : Either<'e, 'a> =
        match first with
        | Left _  -> second ()
        | Right _ -> first


// Monad    

    let inline bind f (ma: Either< ^e, ^a>) : Either< ^e, ^b> =
        match ma with Right a -> f a | Left e -> Left e

    let flatten mm : Either<'e, 'a> =
        match mm with Right m -> m | Left e -> Left e    

    let inline fixM loop (em: Choice< ^a, Either< ^e, ^a>>) : Either< ^e, ^b> =
        let rec go = function Right a -> k a | Left e -> Left e
        and k a = loop k go a
        match em with Choice1Of2 a -> k a | Choice2Of2 m -> go m


    [<RequireQualifiedAccess>]
    module Workflow =

        type EitherBuilder() =
            member _.Return(x) : Either<'e, 'a> = Right x
            member _.ReturnFrom(m) : Either<'e, 'a> = m
            member _.Zero() : Either<'e, unit> = Right ()
            member inline _.Bind(m: Either< ^e, ^a>, f) : Either< ^e, ^b> = bind f m            

    let either = Workflow.EitherBuilder()


// Semigroup

    let inline append (second: Either< ^a, ^b>) first : Either< ^a, ^b> =
        match first, second with
        | Right a, Right b -> Right (a + b)
        | Left _, _        -> first
        | _, Left _        -> second        


// Foldable

    let inline fold folder (seed: ^s) (t: Either< ^e, ^a>) =
        match t with Left _ -> seed | Right a -> folder seed a

    let inline foldBack folder (seed: ^s) (t: Either< ^e, ^a>) =
        match t with Left _ -> seed | Right a -> folder a seed

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

    let inline bimapFold (mapping1: ^s -> ^a -> struct (^b * ^s)) (mapping2: ^s -> ^c -> struct (^d * ^s)) (seed: ^s) (t: Either< ^a, ^c>) : struct (Either< ^b, ^d> * ^s) =
        match t with
        | Right b -> let struct (r, s) = mapping2 seed b in Right r, s
        | Left a  -> let struct (r, s) = mapping1 seed a in Left r, s

    let inline bimapFoldBack (mapping1: ^a -> ^s -> struct (^b * ^s)) (mapping2: ^c -> ^s -> struct (^d * ^s)) (seed: ^s) (t: Either< ^a, ^c>) : struct (Either< ^b, ^d> * ^s) =
        match t with
        | Right b -> let struct (r, s) = mapping2 b seed in Right r, s
        | Left a  -> let struct (r, s) = mapping1 a seed in Left r, s