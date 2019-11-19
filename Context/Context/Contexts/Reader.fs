namespace Rogz.Context.Data.Reader


module Reader =

// Interop

    [<CompiledName("Make")>]
    let inline make (f: System.Func< ^e, ^a>) = Reader f.Invoke


// Minimal

    [<CompiledName("Ask")>]
    let ask<'e> : Reader< ^e, ^e> = Reader id

    let inline local (localize: ^e -> ^e) (Reader r) : Reader< ^e, ^a> =
        Reader (fun e -> r (localize e))


// Primitives

    let inline runReader (env: ^e) (Reader r) : ^a = r env

    let inline flip (f: ^a -> ^e-> ^r) = Reader (fun e a -> f a e)

    let inline curry (f: ^a * ^b -> ^c) = Reader (fun a b -> f (a, b))

    let inline curry1 (f: struct (^a * ^b) -> ^c) = Reader (fun a b -> f (struct (a, b)))

    let inline uncurry (f: ^a -> ^b -> ^c) : Reader< ^a * ^b, ^c> =
        Reader (fun (a, b) -> f a b)

    let inline uncurry1 (f: ^a -> ^b -> ^c) : Reader< struct (^a * ^b), ^c> =
        Reader (fun (struct (a, b)) -> f a b)

    let inline cache (Reader r) : Reader< ^e, ^a> when ^e: equality =
        let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        Reader (fun e -> match d.TryGetValue(e) with
                         | true, res -> res
                         | false, _ -> let res = r e in d.[e] <- res; res)

    let inline register (event: ^e -> unit) (Reader r) : Reader< ^e, unit> =
        Reader (fun e -> r e; event e)


// Isomorphisms

    [<CompiledName("ToFunc")>]
    let inline toFunc (Reader r) : System.Func< ^e, ^a> = System.Func<_,_>r


// Functor

    let inline map (f: ^a -> ^b) (Reader r) : Reader< ^e, ^b> =
        Reader (fun e -> f (r e))


// Profunctor

    let inline dimap (f: ^c -> ^a) (g: ^b -> ^d) (Reader r) : Reader< ^c, ^d> =
        Reader (fun c -> g (r (f c)))

    let inline mapl (f: ^c -> ^a) (Reader r) : Reader< ^c, ^b> =
        Reader (fun c -> r (f c))

    let inline mapr (g: ^b -> ^d) (Reader r) : Reader< ^a, ^d> =
        Reader (fun a -> g (r a))


// Applicative

    let inline unit value : Reader< ^e, ^a> = Reader (fun _ -> value)

    let inline ap (Reader rv) (Reader rf) : Reader< ^e, ^b> =
        Reader (fun e -> rf e ((rv e): ^a))

    let inline map2 (f: ^a -> ^b -> ^c) (Reader ra) (Reader rb) : Reader< ^e, ^c> =
        Reader (fun e -> f (ra e) (rb e))

    let inline andthen (Reader rb) (Reader ra) : Reader< ^e, ^b> =
        Reader (fun e -> ignore ((ra e): ^a); rb e)


// Monad

    let inline bind (f: ^a -> Reader< ^e, ^b>) (Reader r) =
        Reader (fun e -> let (Reader r2) = f (r e) in r2 e)

    let inline flatten (Reader rr) : Reader< ^e, ^a> =
        Reader (fun e -> let (Reader r) = rr e in r e)

    let inline fixM (loop: (^a -> Reader< ^e, ^b>) -> (Reader< ^e, ^a> -> Reader< ^e, ^b>) -> ^a -> Reader< ^e, ^b>) em =
        Reader (fun e ->
            let rec go (Reader r) = k (r e)
            and k a = loop k go a
            runReader e (match em with
                         | Rogz.Context.Data.Either.Left a  -> k a
                         | Rogz.Context.Data.Either.Right m -> go m))

    // foldlM
    // foldrM

    [<RequireQualifiedAccess>]
    module Workflow =

        type ReaderBuilder() =
            member inline _.Return(x) : Reader< ^e, ^a> = unit x
            member inline _.ReturnFrom(m) : Reader< ^e, ^a> = m
            member inline _.Bind(m, f: ^a -> Reader< ^e, ^b>) = bind f m
            member inline _.Zero() : Reader< ^e, unit> = unit ()
            member inline _.Using(disp: ^d, f: ^d -> Reader< ^e, ^a>) : Reader< ^e, ^a> when ^d :> System.IDisposable = using disp f
            member inline _.TryWith(m: Reader< ^e, ^a>, handler) = try m with e -> handler e
            member inline _.TryFinally(m: Reader< ^e, ^a>, finalizer) = try m finally finalizer ()


    let reader = Workflow.ReaderBuilder()


// Semigroup

    let inline append (Reader f) (Reader s) : Reader< ^e, ^a> =
        Reader (fun e ->
            (^a: (static member Append: ^a -> ^a -> ^a) (f e, s e)))


// Traversable

    let inline sequence (source: seq<Reader< ^e, ^a>>) : Reader< ^e, seq< ^a>> =
        Reader (fun e ->
            System.Linq.Enumerable.Select(source, fun (Reader r) -> r e))

    let inline traverse (f: ^a -> Reader< ^e, ^b>) (source: seq< ^a>) : Reader< ^e, seq< ^b>> =
        Reader (fun e ->
            System.Linq.Enumerable.Select(source,
                                          fun x -> let (Reader r) = f x in r e))


// Cat

    [<CompiledName("Identity")>]
    let identity<'a> : Reader< ^a, ^a> = Reader id

    [<CompiledName("Compose")>]
    let inline compose (Reader rbc) (Reader rab) : Reader< ^a, ^c> =
        Reader (fun a -> rbc ((rab a): ^b))


// Arrow

    let inline arr f : Reader< ^a, ^b> = Reader f

    let inline first (Reader r) : Reader< ^a * ^c, ^b * ^c> =
        Reader (fun (a, c) -> r a, c)

    let inline second (Reader r) : Reader< ^c * ^a, ^c * ^b> =
        Reader (fun (c, a) -> c, r a)

    let inline split (Reader cd) (Reader ab) : Reader< ^a * ^c, ^b * ^d> =
        Reader (fun (a, c) -> ab a, cd c)

    let inline fanout (Reader ac) (Reader ab) : Reader< ^a, ^b * ^c> =
        Reader (fun a -> ab a, ac a)


// Arrow.Choice

    open Rogz.Context.Data.Either

    let inline feedl (Reader ab) : Reader<Either< ^a, ^c>, Either< ^b, ^c>> =
        Reader (function Left a  -> Left (ab a)
                       | Right c -> Right c)

    let inline feedr (Reader ab) : Reader<Either< ^c, ^a>, Either< ^c, ^b>> =
        Reader (function Left c  -> Left c
                       | Right a -> Right (ab a))

    let inline merge (Reader cd) (Reader ab) : Reader<Either< ^a, ^c>, Either< ^b, ^d>> =
        Reader (function Left a  -> Left (ab a)
                       | Right c -> Right (cd c))

    let inline fanin (Reader cb) (Reader ab) : Reader<Either< ^a, ^c>, ^b> =
        Reader (function Left a  -> ab a
                       | Right c -> cb c)


// Arrow.Apply

    let app<'a, 'b> : Reader<Reader< ^a, ^b> * ^a, ^b> =
        Reader (fun ((Reader r), a) -> r a)