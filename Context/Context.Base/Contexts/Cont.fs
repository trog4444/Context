namespace Rogz.Context.Base


[<Struct; NoComparison; NoEquality>]
type Cont<'R, 'T> = Cont of (('T -> 'R) -> 'R)
with

// Function

    member inline this.Invoke(k: System.Func< ^T, ^R>) = let (Cont cc) = this in cc k.Invoke

    static member Of(func: System.Func<System.Func<'T, 'R>, 'R>) : Cont<'R, 'T> =
        Cont(fun k -> func.Invoke(fun a -> k a))


module Cont =

// Haskell Primitives

    let runCont (k: 'a -> 'r) (Cont cc) = cc k
    
    let evalCont (Cont cc) : 'r = cc id

    let inline mapCont (f: ^r -> ^r) (Cont cc) : Cont< ^r, ^a> =
        Cont (fun k -> cc (fun a -> f (k a)))

    let inline withCont (f: (^b -> ^r) -> ^a -> ^r) (Cont cc) =
        Cont (fun k -> cc (fun a -> f k a))

    let inline shift f : Cont< ^r, ^a> = Cont (fun k -> let (Cont cc) = f k in cc id)
    
    let reset (Cont cc) : Cont<'r0, 'r> = Cont (fun k -> k (cc id))

    let inline callCC (f: ((^a -> Cont< ^r, ^``_``>) -> Cont< ^r, ^a>)) =
        Cont (fun k -> let (Cont cc) = f (fun x -> Cont (fun _ -> k x)) in cc k)

    let getCC (x0: 'a) : Cont< 'r, struct ('a * ('a -> Cont< 'r, '``_``>))> =
        // Original based on calling callCC directly:
        //  callCC (fun c -> let rec f x = c (struct (x, f)) in Cont (fun k -> k (x0, f)))
        // Version where the use of callCC is embedded directly into the `getCC` code.
        // This allows `getCC` to be non-inline and provides a performance benefit.
        Cont (fun k -> let (Cont cc) = (fun c -> let rec f x = c (struct (x, f)) in Cont (fun k -> k (struct (x0, f)))) (fun x -> Cont (fun _ -> k x)) in cc k)
        

// Functor

    let inline map (f: ^a -> ^b) (Cont cc) : Cont< ^r, ^b> =
        Cont (fun k -> cc (fun a -> k (f a)))


// Applicative

    [<CompiledName("Unit")>]
    let unit (value: 'a) : Cont<'r, ^a> = Cont (fun k -> k value)

    let inline ap (Cont cv) (Cont cf) : Cont< ^r, ^b> =
        Cont (fun k -> cf (fun f -> cv (fun (v: ^a) -> k (f v))))

    let inline map2 (f: ^a -> ^b -> ^c) (Cont ca) (Cont cb) : Cont< ^r, ^c> =
        Cont (fun k -> ca (fun a -> cb (fun b -> k (f a b))))

    let sequence (source: #seq<Cont<'r, 'a>>) : Cont< ^r, seq< ^a>> =
        Cont (fun k ->
            use e = source.GetEnumerator()
            let xs = ResizeArray<_>()
            let rec go () =
                if e.MoveNext() then let (Cont c) = e.Current in c (xs.Add >> go)
                else k xs
            go ())

    let inline traverse (f: ^a -> Cont< ^r, ^b>) (source: #seq< ^a>) : Cont< ^r, seq< ^b>> =
        Cont (fun k ->
            use e = source.GetEnumerator()
            let xs = ResizeArray<_>()
            let rec go () =
                if e.MoveNext() then let (Cont c) = f e.Current in c addgo
                else k xs
            and addgo x = go (xs.Add x)
            go ())


// Monad

    let inline bind (f: ^a -> Cont< ^r, ^b>) (Cont m) =
        Cont (fun k -> m (fun a -> let (Cont n) = f a in n k))

    let flatten (Cont ccc) : Cont<'r, 'a> =
        Cont (fun k -> ccc (fun (Cont cc) -> cc k))

    let inline fixM loop em : Cont< ^r, ^b> =
        let rec go m = bind k m
        and k a = loop k go a
        match em with
        | Choice1Of2 a -> k (a: ^a)
        | Choice2Of2 m -> go m


    [<RequireQualifiedAccess>]
    module Workflow =

        type ContBuilder() =
            member _.Return(x) : Cont< 'r, 'a> = unit x
            member _.ReturnFrom(m) : Cont<'r, 'a> = m
            member _.Zero() : Cont<'r, unit> = unit ()
            member inline _.Bind(m, f: (^a -> Cont< ^r, ^b>)) = bind f m

    let cont = Workflow.ContBuilder()