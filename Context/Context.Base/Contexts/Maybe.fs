namespace Rogz.Context.Base


[<Struct>]
type Maybe<'T> = Nothing | Just of 'T
with

// Union Type

    member this.TryJust([<System.Runtime.InteropServices.Out>] value: outref<_>) =
        match this with
        | Nothing -> false
        | Just a  -> value <- a; true

    member this.Match(ifNothing: System.Func<'U>, ifJust: System.Func<'T, 'U>) =
        match this with
        | Nothing -> ifNothing.Invoke()
        | Just a  -> ifJust.Invoke(a)
    
    member this.Match(ifNothing: System.Action, ifJust: System.Action<'T>) =
        match this with
        | Nothing -> ifNothing.Invoke()
        | Just a  -> ifJust.Invoke(a)

//// Alternative

//    [<CompiledName("OrElse")>]
//    static member ( <|> ) (first, second) : Maybe<'T> =
//        match first with
//        | Nothing -> second
//        | _       -> first

    //static member Empty : Maybe<'T> = Nothing

// Semigroup

    //[<CompiledName("Append")>]
    static member inline ( + ) (first: Maybe< ^M>, second: Maybe< ^M>) : Maybe< ^M> =
        match first, second with
        | Just a, Just b -> Just (a + b)
        | _, Nothing     -> first
        | Nothing, _     -> second

// Monoid

    static member Zero : Maybe<'T> = Nothing


module Maybe =

// Haskell Primitives

    let inline caseof ifNothing (ifJust: ^a -> ^b) maybe =
        match maybe with
        | Nothing -> ifNothing ()    
        | Just a  -> ifJust a   

    let isJust (maybe: Maybe<'a>) =
        match maybe with
        | Nothing -> false
        | Just _  -> true

    let isNothing (maybe: Maybe<'a>) =
        match maybe with
        | Nothing -> true
        | Just _  -> false

    let fromMaybe ifNothing maybe =
        match maybe with
        | Nothing -> ifNothing
        | Just a  -> a

    let ofSeq (source: #seq<'a>) : Maybe< ^a> =
        if Seq.isEmpty source then Nothing
        else Just (Seq.head source)

    let toSeq maybe : 'a list =
        match maybe with
        | Nothing -> []
        | Just a  -> [a]

    let inline mapMaybes (f: ^a -> Maybe< ^b>) (source: #seq< ^a>) : ^b seq =
        seq { for a in source do
                  match f a with
                  | Nothing -> ()
                  | Just b  -> yield b }


// F# Primitives

    let ofObj (obj: 'a) : Maybe<'a> when 'a : null =
        if isNull obj then Nothing else Just obj

    let ofNullable (nullable: System.Nullable<'a>) =
        if nullable.HasValue then Just nullable.Value else Nothing


// Functor

    let inline map (f: ^a -> ^b) fa =
        match fa with
        | Nothing -> Nothing
        | Just a  -> Just (f a)        


// Applicative

    [<CompiledName("Unit")>]
    let unit (value: 'a) = Just value

    let inline ap fv (ff: Maybe<(^a -> ^b)>) =
        match ff, fv with
        | Nothing, _ | _, Nothing -> Nothing
        | Just f, Just v          -> Just (f v)        

    let inline map2 (f: ^a -> ^b -> ^c) fa fb =
        match fa, fb with        
        | Nothing, _ | _, Nothing -> Nothing
        | Just a, Just b          -> Just (f a b)   

    let sequence (source: #seq<Maybe<'a>>) : Maybe< ^a seq> =
        use e = source.GetEnumerator()
        let xs = ResizeArray<_>()
        let rec go () =
            if e.MoveNext() then
                match e.Current with
                | Nothing -> Nothing
                | Just a  -> go (xs.Add(a))
            else Just (xs :> seq<_>)
        go ()

    let inline traverse f (source: #seq< ^a>) : Maybe< ^b seq> =
        use e = source.GetEnumerator()
        let xs = ResizeArray<_>()
        let rec go () =
            if e.MoveNext() then
                match f e.Current with
                | Nothing -> Nothing
                | Just a  -> go (xs.Add(a))
            else Just (xs :> seq<_>)
        go ()


// Alternative

    let empty<'a> : Maybe< ^a> = Nothing

    let orElse second first : Maybe<'a> =
        match first with
        | Nothing -> second
        | Just _  -> first

    let orElseWith second first : Maybe<'a> =
        match first with
        | Nothing -> second ()
        | Just _  -> first

    let guard condition = if condition then Just () else Nothing

    let concat (source: #seq<Maybe<'a>>) =
        System.Linq.Enumerable.FirstOrDefault(source, System.Func<_,_>(function Just _ -> true | Nothing -> false))    


// Monad    

    let inline bind f (m: Maybe< ^a>) : Maybe< ^b> =
        match m with
        | Nothing -> Nothing
        | Just a  -> f a

    let flatten mm : Maybe<'a> =
        match mm with
        | Nothing -> Nothing
        | Just m  -> m

    let inline fixM loop (em: Choice< ^a, Maybe< ^a>>) : Maybe< ^b> =
        let rec go = function Just a -> k a | Nothing -> Nothing
        and k a = loop k go a
        match em with
        | Choice1Of2 a -> k a
        | Choice2Of2 m -> go m

    //let inline foldlM f s (ms: #seq< ^a>) : Maybe< ^s> =
    //    use e = ms.GetEnumerator()
    //    let rec go s =
    //        if e.MoveNext() then
    //            match f s e.Current with
    //            | Nothing -> Nothing
    //            | Just z  -> go z
    //        else Just s
    //    go s
    //
    //let inline foldrM f s (xs: #seq< ^a>) : Maybe< ^s> =
    //    let inline f' k x z = bind k (f x z)
    //    Seq.fold f' unit xs s


    [<RequireQualifiedAccess>]
    module Workflow =

        type MaybeBuilder() =
            member _.Return(x: 'a) = Just x
            member _.ReturnFrom m : Maybe<'a> = m
            member _.Zero() = Just ()
            member inline _.Bind(m: Maybe< ^a>, f) : Maybe< ^b> = bind f m

    let maybe = Workflow.MaybeBuilder()


// MonadPlus    

    let inline filter p m : Maybe< ^a> =
        match m with
        | Nothing -> m
        | Just a  -> if p a then m else Nothing        

    let inline join p (f: ^a -> ^b -> ^c) ma mb =
        match ma, mb with
        | Nothing, _ | _, Nothing -> Nothing
        | Just a, Just b          -> if p a b then Just (f a b) else Nothing        


// Semigroup

    let inline append (second: Maybe< ^a>) (first: Maybe< ^a>) =
        match first, second with
        | Just a, Just b -> Just (a + b)
        | _, Nothing     -> first
        | Nothing, _     -> second


// Monoid

    let zero<'a> : Maybe<'a> = Nothing

    let inline sum (source: #seq<Maybe< ^a>>) =
        //let mutable s = Nothing
        //for x in source do s <- append x s
        //s
        let f t s =
            match t, s with
            | Just t, Just s -> Just (t + s)
            | Nothing, _     -> s
            | _, Nothing     -> t
        Seq.foldBack f source Nothing


// Foldable

    let inline fold (folder: ^s -> ^a -> ^s) seed t =
        match t with
        | Nothing -> seed
        | Just a  -> folder seed a

    let inline foldBack (folder: ^a -> ^s -> ^s) seed t =
        match t with
        | Nothing -> seed
        | Just a  -> folder a seed

    let inline mapFold mapping (seed: ^s) (t: Maybe< ^a>) : struct (Maybe< ^b> * ^s) =
        match t with
        | Nothing -> Nothing, seed
        | Just a  -> let struct (r, s) = mapping seed a in Just r, s

    let inline mapFoldBack mapping (seed: ^s) (t: Maybe< ^a>) : struct (Maybe< ^b> * ^s) =
        match t with
        | Nothing -> Nothing, seed
        | Just a  -> let struct (r, s) = mapping a seed in Just r, s