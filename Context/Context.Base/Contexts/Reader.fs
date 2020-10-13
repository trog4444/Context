namespace Rogz.Context.Base


[<Struct; NoComparison; NoEquality>]
type Reader<'Env, 'T> = Reader of ('Env -> 'T)
with

// Function

    member this.Invoke(env: ^Env) = let (Reader f) = this in f env

    static member Of(func: System.Func<'Env, 'T>) : Reader<'Env, 'T> = Reader func.Invoke

// Semigroup

    //[<CompiledName("Append")>]
    static member inline ( + ) ((Reader (r1: ^E -> ^A)), Reader (r2: ^E -> ^A)) : Reader< ^E, ^A> =
        Reader (fun e -> r1 e + r2 e)


module Reader =

// Haskell Primitives

    let ask<'e> : Reader< ^e, ^e> = Reader id

    let asks (func: System.Func<'e,'a>) = Reader (func.Invoke)

    let inline local (localize: ^e -> ^e) (Reader r) : Reader< ^e, ^a> =
        Reader (fun e -> r (localize e))

    let runReader (env: 'e) (Reader r) : 'a = r env


// Functor

    let inline map (f: ^a -> ^b) (Reader r) : Reader< ^e, ^b> =
        Reader (fun e -> f (r e))


// Profunctor

    let inline dimap (f: ^c -> ^a) (g: ^b -> ^d) (Reader r) : Reader< ^c, ^d> =
        Reader (fun c -> g (r (f c)))

    let inline mapl (f: ^c -> ^a) (Reader r) : Reader< ^c, ^b> =
        Reader (fun c -> r (f c))


// Applicative

    [<CompiledName("Unit")>]
    let unit (value: 'a) : Reader<'e, ^a> = Reader (fun _ -> value)

    let inline ap (Reader rv) (Reader rf) : Reader< ^e, ^b> =
        Reader (fun e -> rf e ((rv e): ^a))

    let inline map2 (f: ^a -> ^b -> ^c) (Reader ra) (Reader rb) : Reader< ^e, ^c> =
        Reader (fun e -> f (ra e) (rb e))

    let sequence (source: #seq<Reader<'e, 'a>>) : Reader< ^e, seq< ^a>> =
        Reader (fun e -> System.Linq.Enumerable.Select(source, fun (Reader r) -> r e))

    let inline traverse (f: ^a -> Reader< ^e, ^b>) (source: #seq< ^a>) : Reader< ^e, seq< ^b>> =
        Reader (fun e ->
            System.Linq.Enumerable.Select(source,
                                          fun x -> let (Reader r) = f x in r e))


// Monad

    let inline bind (f: ^a -> Reader< ^e, ^b>) (Reader r) =
        Reader (fun e -> let (Reader r2) = f (r e) in r2 e)

    let flatten (Reader rr) : Reader<'e, 'a> =
        Reader (fun e -> let (Reader r) = rr e in r e)

    let inline fixM (loop: (^a -> Reader< ^e, ^b>) -> (Reader< ^e, ^a> -> Reader< ^e, ^b>) -> ^a -> Reader< ^e, ^b>) em =
        Reader (fun e ->
            let rec go (Reader r) = k (r e)
            and k a = loop k go a
            runReader e (match em with
                         | Choice1Of2 a  -> k a
                         | Choice2Of2 m -> go m))


    [<RequireQualifiedAccess>]
    module Workflow =

        type ReaderBuilder() =
            member _.Return(x) : Reader<'e, 'a> = unit x
            member _.ReturnFrom(m) : Reader<'e, 'a> = m
            member _.Zero() : Reader<'e, unit> = unit ()
            member inline _.Bind(m, f: ^a -> Reader< ^e, ^b>) = bind f m            

    let reader = Workflow.ReaderBuilder()


// Semigroup

    let inline append (Reader (second: ^e -> ^a)) (Reader (first: ^e -> ^a)) : Reader< ^e, ^a> =
        Reader (fun e -> first e + second e)


//// Cat

//    let identity<'a> : Reader< ^a, ^a> = Reader id

//    let inline compose (Reader rbc) (Reader rab) : Reader< ^a, ^c> =
//        Reader (fun a -> rbc ((rab a): ^b))


//// Arrow

//    [<System.Obsolete("Use Reader constructor instead.")>]
//    let inline arr f : Reader< ^a, ^b> = Reader f

//    [<System.Obsolete("Use Reader constructor instead.")>]
//    let inline first (Reader r) : Reader< ^a * ^c, ^b * ^c> =
//        Reader (fun (a, c) -> r a, c)

//    let inline second (Reader r) : Reader< ^c * ^a, ^c * ^b> =
//        Reader (fun (c, a) -> c, r a)

//    [<System.Obsolete("Use Reader constructor instead.")>]
//    let inline split (Reader cd) (Reader ab) : Reader< ^a * ^c, ^b * ^d> =
//        Reader (fun (a, c) -> ab a, cd c)

//    let inline fanout (Reader ac) (Reader ab) : Reader< ^a, ^b * ^c> =
//        Reader (fun a -> ab a, ac a)


//// Arrow.Choice

//    open Rogz.Context.Data.Either

//    let inline feedl (Reader ab) : Reader<Either< ^a, ^c>, Either< ^b, ^c>> =
//        Reader (function Left a  -> Left (ab a)
//                       | Right c -> Right c)

//    let inline feedr (Reader ab) : Reader<Either< ^c, ^a>, Either< ^c, ^b>> =
//        Reader (function Left c  -> Left c
//                       | Right a -> Right (ab a))

//    let inline merge (Reader cd) (Reader ab) : Reader<Either< ^a, ^c>, Either< ^b, ^d>> =
//        Reader (function Left a  -> Left (ab a)
//                       | Right c -> Right (cd c))

//    [<System.Obsolete("Use Reader constructor instead.")>]
//    let inline fanin (Reader cb) (Reader ab) : Reader<Either< ^a, ^c>, ^b> =
//        Reader (function Left a  -> ab a
//                       | Right c -> cb c)