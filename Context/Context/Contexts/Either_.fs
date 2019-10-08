//#nowarn "0052" // used when .Match is called -> one possible way around this
//// is to do like Left a -> let a = a in onLeft.Invoke(a)

namespace PTR.Context.Type.Either


[<Struct>]
type Either<'A, 'B> = Left of L: 'A | Right of R: 'B with
   member inline s.Match(fLeft: System.Func< ^A, ^C>, fRight: System.Func< ^B, ^C>) : ^C =
    match s with
    | Left a  -> fLeft.Invoke(a)
    | Right b -> fRight.Invoke(b)


module Either =
 
    [<CompiledName("IsLeft")>]
    let isLeft either = match either with Left _ -> true | Right _ -> false

    [<CompiledName("IsRight")>]
    let isRight either = match either with Left _ -> false | Right _ -> true

    [<CompiledName("OfEither")>]
    let inline ofEither fLeft fRight (either: Either< ^a, ^b>) : ^c =
        match either with Left a -> fLeft a | Right b -> fRight b

    [<CompiledName("FromLeft")>]
    let fromLeft (def: 'a) either : ^a = match either with Left a -> a | Right _ -> def

    [<CompiledName("FromRight")>]
    let fromRight (def: 'b) either : ^b = match either with Left _ -> def | Right b -> b

    [<CompiledName("Lefts")>]
    let lefts eithers = seq { for e in eithers do match e with Left a -> yield a | Right _ -> () }

    [<CompiledName("Rights")>]
    let rights eithers = seq { for e in eithers do match e with Left _ -> () | Right b -> yield b }

    [<CompiledName("Partition")>]
    let partition (eithers: Either<'a, 'b> seq) =
        let f e (struct (ls, rs)) = match e with Left a -> struct (a::ls, rs) | Right b -> struct (ls, b::rs)
        let z = struct ([], [])
        match (match eithers with
               | :? array<Either< ^a, ^b>> as s -> Array.foldBack f s z
               | :? list< Either< ^a, ^b>> as s -> List.foldBack  f s z
               | _ -> Seq.foldBack f eithers z) with struct (a, b) -> a, b

    [<CompiledName("Hush")>]
    let hush (either: Either<'``_``, 'a>) : ^a option =
        match either with Left _ -> None | Right a -> Some a


    module Convert =
    
        [<CompiledName("OfSeq")>]
        let ofSeq (source: 'a seq) : Either<string, ^a> =
            if isNull source then Left "Input sequence was null."
            elif Seq.isEmpty source then Left "Input sequence was empty."
            else Right (Seq.head source)

        [<CompiledName("ToSeq")>]
        let toSeq (either: Either<'a, 'b>) : ^b seq =
            match either with Left _ -> Seq.empty | Right a -> Seq.singleton a    

        [<CompiledName("OfChoice")>]
        let ofChoice (choice: Choice<'b, 'a>) : Either< ^a, ^b> =
            match choice with Choice1Of2 b -> Right b | Choice2Of2 a -> Left a

        [<CompiledName("ToChoice")>]
        let toChoice (either: Either<'a, 'b>) : Choice< ^b, ^a> =
            match either with Right b -> Choice1Of2 b | Left a -> Choice2Of2 a

        [<CompiledName("OfResult")>]
        let ofResult (result: Result<'b, 'err>) : Either< ^err, ^b> =
            match result with Ok b -> Right b | Error err -> Left err

        [<CompiledName("ToResult")>]
        let toResult (either: Either<'err, 'b>) : Result< ^b, ^err> =
            match either with Right b -> Ok b | Left err -> Error err


    module Compose =

        module Monad =

            [<CompiledName("Wrap")>]
            let inline wrap x : Either< ^a, ^b> = Right x

            [<CompiledName("Bind")>]
            let inline bind (k: ^b -> Either< ^a, ^c>) m =
                match m with Left a -> Left a | Right b -> k b

            [<CompiledName("Flatten")>]
            let flatten (mm: Either<'a, Either< ^a, 'b>>) : Either< ^a, ^b> =
                match mm with
                | Left e  -> Left e
                | Right m -> m


            type EitherBuilder () =
                member inline _.Bind(m, k) = bind k m
                member inline _.Return x = wrap x
                member inline _.ReturnFrom m : Either< ^a, ^b> = m
                member inline s.Zero () = s.Return ()
 
                member inline _.TryWith (body, handler) : Either< ^a, ^b> = try body () with e -> handler e
                member inline _.TryFinally (body, finalizer) : Either< ^a, ^b> = try body () finally finalizer ()
 
                member inline _.Using(disp: ^d when ^d :> System.IDisposable, body) : Either< ^a, ^b> =
                    try body disp finally disp.Dispose ()
 
                member inline _.While(guard, body) : Either< ^a, unit> =                    
                    let rec loop = function
                    | false -> wrap ()
                    | true  -> bind k (body ())
                    and k () = loop (guard ()) in k ()
 
                member inline s.For(seq: #seq< ^b>, body) : Either< ^a, unit> =
                    s.Using(seq.GetEnumerator(), fun enum -> s.While(enum.MoveNext, fun () -> body enum.Current))


            [<CompiledName("RecM")>]
            let inline recM f (x: ^b) : Either< ^a, ^c> =
                let rec go = function
                | Left a  -> Left a
                | Right b -> f lp b
                and lp b = go (Right b)
                go (Right x)

            [<CompiledName("RecM1")>]
            let inline recM1 f (x: ^b) : Either< ^a, ^c> =
                let rec go m = bind k m
                and k a = f go a
                k x

            [<CompiledName("FoldrM")>]
            let inline foldrM (f: ^b -> ^s -> Either< ^a, ^s>) (s0: ^s) (source: ^b seq) : Either< ^a, ^s> =
                let g k x s = bind k (f x s)
                match source with
                | :? array< ^b> as s -> Array.fold g wrap s s0
                | :? list<  ^b> as s -> List.fold  g wrap s s0
                | _ -> Seq.fold g wrap source s0

            [<CompiledName("FoldlM")>]
            let inline foldlM (f: ^s -> ^b -> Either< ^a, ^s>) (s0: ^s) (source: ^b seq) : Either< ^a, ^s> =
                use e = source.GetEnumerator()
                let mutable s = s0
                let mutable g = true
                let mutable a0 = Unchecked.defaultof< ^a>
                while g && e.MoveNext() do
                    match f s e.Current with
                    | Left a  -> g <- false ; a0 <- a
                    | Right b -> s <- b
                if g then Right s else Left a0


        module Applicative =

            [<CompiledName("Wrap")>]
            let inline wrap (x: ^b) : Either< ^a, ^b> = Right x

            [<CompiledName("Ap")>]
            let inline ap fv (ff: Either< ^a, (^b -> ^c)>) : Either< ^a, ^c> =
                match ff with
                | Left a  -> Left a
                | Right f -> match fv with
                             | Left a  -> Left a
                             | Right v -> Right (f v)

            [<CompiledName("Map2")>]
            let inline map2 (f: ^b -> ^c -> ^d) fc fd : Either< ^a, ^d> =
                match fc with
                | Left a  -> Left a
                | Right b -> match fd with
                             | Left a  -> Left a
                             | Right c -> Right (f b c)

            [<CompiledName("Map3")>]
            let inline map3 (f: ^b -> ^c -> ^d -> ^e) fb fc fd : Either< ^a, ^e> =
                match fb with
                | Left a  -> Left a
                | Right b -> match fc with
                             | Left a  -> Left a
                             | Right c -> match fd with
                                          | Left a  -> Left a
                                          | Right d -> Right (f b c d)

            [<CompiledName("AndThen")>]
            let andThen (fc: Either<'a, 'c>) fb : Either< ^a, ^c> =
                match fb with Left a -> Left a | Right _ -> fc

            [<CompiledName("When")>]
            let inline when_ condition f : Either< ^a, unit> =
                if condition then f () else wrap ()

            [<CompiledName("FilterA")>]
            let inline filterA (p: ^b -> Either< ^a, bool>) (source: ^b seq) =
                let mutable a0 = Unchecked.defaultof< ^a>
                let mutable g = true
                use e = source.GetEnumerator()
                let xs = seq {
                    while g && e.MoveNext() do
                        match p e.Current with
                        | Left a      -> a0 <- a ; g <- false
                        | Right false -> ()
                        | Right true  -> yield e.Current } |> Seq.cache
                do for _ in xs do ()
                if g then Right xs else Left a0


            [<CompiledName("SequenceA")>]
            let inline sequenceA (source: Either< ^a, ^b> seq) : Either< ^a, ^b seq> =
                let mutable a0 = Unchecked.defaultof< ^a>
                let mutable g = true
                use e = source.GetEnumerator()
                let xs = seq {
                    while g && e.MoveNext() do
                        match e.Current with
                        | Left a  -> a0 <- a ; g <- false
                        | Right b -> yield b } |> Seq.cache
                do for _ in xs do ()
                if g then Right xs else Left a0

            [<CompiledName("ForA")>]
            let inline forA (f: ^b -> Either< ^a, ^c>) (source: #seq< ^b>) : Either< ^a, ^c seq> =
                sequenceA (System.Linq.Enumerable.Select(source, System.Func<_,_>f))

            [<CompiledName("ZipWithA")>]
            let inline zipWithA (f: ^b -> ^c -> Either< ^a, ^d>) (source1: #seq< ^b>) (source2: #seq< ^c>) : Either< ^a, ^d seq> =
                sequenceA (System.Linq.Enumerable.Zip(source1, source2, System.Func<_,_,_>f))

            [<CompiledName("ReplicateA")>]
            let replicateA count (fb: Either<'a, 'b>) : Either< ^a, ^b seq> =
                match fb with Left a -> Left a | Right b -> Right (Seq.replicate (max 0 count) b)


        module Functor =

            [<CompiledName("Map")>]
            let inline map (f: ^b -> ^c) fa : Either< ^a, ^c> =
                match fa with Left a -> Left a | Right b -> Right (f b)

            [<CompiledName("Replace")>]
            let replace (b: 'c) (fb: Either<'a, 'b>) : Either< ^a, ^c> =
                match fb with Left a -> Left a | Right _ -> Right b


        module Bifunctor =

            [<CompiledName("Bimap")>]
            let inline bimap f g (bf: Either< ^a, ^b>) : Either< ^c, ^d> =
                match bf with Left a -> Left (f a) | Right b -> Right (g b)

            [<CompiledName("MapFst")>]
            let inline mapFst f (bf: Either< ^a, ^b>) : Either< ^c, ^b> =
                match bf with Left a -> Left (f a) | Right b -> Right b

            [<CompiledName("MapSnd")>]
            let inline mapSnd g (bf: Either< ^a, ^b>) : Either< ^a, ^c> =
                match bf with Left a -> Left a | Right b -> Right (g b)


        module Semigroup =

            [<CompiledName("SAppend")>]
            let sappend e1 (e2: Either<'a, 'b>) : Either< ^a, ^b> =
                match e1 with
                | Left _  -> e2
                | Right _ -> e1
    

    let either = Compose.Monad.EitherBuilder ()



//open Either
//open Compose
 
//// @ Operators @
//type Either<'A, 'B> with

//// @ Primitive @

//    /// Return the contents of a Right-value or a default value otherwise.
//    static member inline ( >- ) (m, d) = fromRight d m
//    /// Return the contents of a Right-value or a default value otherwise.
//    static member inline ( -< ) (d, m) = fromRight d m

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

//// @ Functor @    

//    /// Lift a function onto effects.
//    static member inline ( |%> ) (fa, f) = Functor.map f fa
//    /// Lift a function onto effects.
//    static member inline ( <%| ) (f, fa) = Functor.map f fa

//    /// Replace all locations in the input with the same value.
//    static member inline ( %> ) (b, fa) = Functor.replace b fa
//    /// Replace all locations in the input with the same value.
//    static member inline ( <% ) (fa, b) = Functor.replace b fa

//// @ Semigroup @

//    /// An associative composition operation.
//    static member inline Append (e1, e2) = Semigroup.sappend e1 e2
