namespace Rogz.Context.Data.Reader


module Reader =


    //let inline ( == ) (a: ^a) (b: ^a) : bool when ^a :> System.IEquatable< ^a> = a.Equals(b)

    //let inline ( /= ) a b = not (a == b)

    //[<Struct>]
    //type M<'a> = Noth | Just of 'a
    //let a = Just 1
    //let b = Just 2
    //let c = a == b


// Interop

    let inline fromFunc (f: System.Func< ^e, ^a>) = Reader f.Invoke


// Minimal

    let ask<'e> : Reader< ^e, ^e> = Reader id

    let inline local (localize: ^e -> ^e) (Reader r) : Reader< ^e, ^a> =
        Reader (fun e -> r (localize e))


// Primitives

    let inline runReader (env: ^e) (Reader r) : ^a = r env

    //let inline flip (f: ^a -> ^e-> ^r) = Reader (fun e a -> f a e)

    //let inline cache (Reader r) : Reader< ^e, ^a> when ^e: equality =
    //    let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
    //    Reader (fun e -> match d.TryGetValue(e) with
    //                     | true, res -> res
    //                     | false, _  -> let res = r e in d.[e] <- res; res)


// Functor

    let inline map (f: ^a -> ^b) (Reader r) : Reader< ^e, ^b> =
        Reader (fun e -> f (r e))


// Profunctor

    let inline dimap (f: ^c -> ^a) (g: ^b -> ^d) (Reader r) : Reader< ^c, ^d> =
        Reader (fun c -> g (r (f c)))

    let inline mapl (f: ^c -> ^a) (Reader r) : Reader< ^c, ^b> =
        Reader (fun c -> r (f c))


// Applicative

    let unit (value: 'a) : Reader<'e, ^a> = Reader (fun _ -> value)

    let inline ap (Reader rv) (Reader rf) : Reader< ^e, ^b> =
        Reader (fun e -> rf e ((rv e): ^a))

    let inline map2 (f: ^a -> ^b -> ^c) (Reader ra) (Reader rb) : Reader< ^e, ^c> =
        Reader (fun e -> f (ra e) (rb e))

    //let inline andthen (Reader rb) (Reader ra) : Reader< ^e, ^b> =
    //    Reader (fun e -> ignore ((ra e): ^a); rb e)

    let inline sequence (source: seq<Reader< ^e, ^a>>) : Reader< ^e, seq< ^a>> =
        Reader (fun e ->
            System.Linq.Enumerable.Select(source, fun (Reader r) -> r e))

    let inline traverse (f: ^a -> Reader< ^e, ^b>) (source: seq< ^a>) : Reader< ^e, seq< ^b>> =
        Reader (fun e ->
            System.Linq.Enumerable.Select(source,
                                          fun x -> let (Reader r) = f x in r e))


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
                         | Choice1Of2 a  -> k a
                         | Choice2Of2 m -> go m))

    // foldlM
    // foldrM


    [<RequireQualifiedAccess>]
    module Workflow =

        type ReaderBuilder() =
            member _.Return(x) : Reader<'e, 'a> = unit x
            member _.ReturnFrom(m) : Reader<'e, 'a> = m
            member inline _.Bind(m, f: ^a -> Reader< ^e, ^b>) = bind f m
            member _.Zero() : Reader<'e, unit> = unit ()
            //member inline _.Using(disp: ^d, f: ^d -> Reader< ^e, ^a>) : Reader< ^e, ^a> when ^d :> System.IDisposable = using disp f
            //member inline _.TryWith(m: Reader< ^e, ^a>, handler) = try m with e -> handler e
            //member inline _.TryFinally(m: Reader< ^e, ^a>, finalizer) = try m finally finalizer ()
            //abstract member Using: disp: 'd * f: ('d -> Reader<'e, 'a>) -> Reader<'e, 'a> when 'd :> System.IDisposable
            //abstract member TryWith: m: Reader<'e, 'a> * h: (exn -> Reader<'e, 'a>) -> Reader<'e, 'a>
            //abstract member TryFinally: m: Reader<'e, 'a> * f: (unit -> unit) -> Reader<'e, 'a>
            member _.Using(disp: 'd, f) : Reader<'e, 'a> when 'd :> System.IDisposable = using disp f
            //default _.TryWith(m, h) : Reader<'e, 'a> = try m with e -> h e
            //default _.TryFinally(m, f) : Reader<'e, 'a> = try m finally f ()


    let reader = Workflow.ReaderBuilder()


// Semigroup

    let inline append (Reader f) (Reader s) : Reader< ^e, ^a> =
        Reader (fun e -> (^a: (static member Append: ^a -> ^a -> ^a) (f e, s e)))


// Cat

    let identity<'a> : Reader< ^a, ^a> = Reader id

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