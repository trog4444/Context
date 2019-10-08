namespace PTR.Context.Type.Incomplete.Suspend


(*
    | TO DO
    -- remake bind, ap, map2... etc
        so that they preserve the
        'semantics' of the supplied suspension
        ie given a Thunk, return a Thunk
        and DONT eagerly eval the result to supply to k
    -- if ^^ is NOT done,
        ALL computations will have to rely on the [eagerly, lazily, memoingly] primitives
*)


[<Struct; NoComparison; NoEquality>]
// Rename ??
type Suspend<'T> =
    | Forced of Value: ^T
    | Thunk  of Thunk: (unit -> ^T)
    | Cache  of Cache: Lazy< ^T>


module Suspend =

    let inline force (susp: ^a Suspend) =
        match susp with
        | Forced a -> a
        | Thunk t  -> t ()
        | Cache l  -> l.Force()
    

    let inline eagerly (f: ^a Suspend -> ^b Suspend) susp =
        match f susp with
        | Forced _ as r -> r
        | Thunk t -> Forced (t ())
        | Cache l -> Forced (l.Force())

    let inline lazily (f: ^a Suspend -> ^b Suspend) susp =
        Thunk (fun () -> force (f susp))

    let inline memoingly (f: ^a Suspend -> ^b Suspend) susp =
        Cache (lazy (force (f susp)))


    let inline cache susp : ^a Suspend =
        match susp with
        | Forced _ -> susp
        | Thunk t  -> Cache (lazy (t ()))
        | Cache _  -> susp

    [<AutoOpen>]
    module Compose =

        [<AutoOpen>]
        module Monad =
        
            let inline wrap (x: ^a) = Forced x

            let inline bind (k: ^a -> ^b Suspend) m =
                match m with
                | Forced a -> k a
                | Thunk a  -> Thunk (fun () -> force (k (a ())))
                | Cache a  -> Cache (lazy (force (k (a.Force()))))

            let inline flatten (mm: ^a Suspend Suspend) =
                match mm with
                | Forced m -> m
                | Thunk m  -> m ()
                | Cache m  -> m.Force()


            type SuspendBuilder () =
                member inline _.Bind(m: Suspend< ^a>, k) : Suspend< ^b> = bind k m
                member inline _.Return x : Suspend< ^a> = wrap x
                member inline _.ReturnFrom m : Suspend< ^a> = m
                member inline s.Zero() = unit ()
 
                member inline _.TryWith(body, handler) : Suspend< ^a> = try body () with e -> handler e
                member inline _.TryFinally(body, finalizer) : Suspend< ^a> = try body () finally finalizer ()
 
                member inline _.Using(disp: ^d when ^d :> System.IDisposable, body) : Suspend< ^a> =
                    try body disp finally disp.Dispose ()
 
                member inline _.While(guard, body) : Suspend<unit> =               
                    let rec loop = function
                    | false -> wrap ()
                    | true  -> bind k (body ())
                    and k () = loop (guard ()) in k ()
 
                member inline s.For(seq: #seq< ^a>, body) : Suspend<unit> =
                    s.Using(seq.GetEnumerator(), fun enum -> s.While(enum.MoveNext, fun () -> body enum.Current)) 


            let inline recM f (x: ^a) : ^b Suspend =
                let rec go m = bind j m
                and k a = go (wrap a)
                and j a = f k a
                j x

            let inline recM1 f (x: ^a) : ^b Suspend =
                let rec go m = bind k m
                and k a = f go a
                k x


        module Functor =

            let inline map (f: ^a -> ^b) fa =
                match fa with
                | Forced a -> Forced (f a)
                | Thunk t  -> Thunk (fun () -> f (t ()))
                | Cache l  -> Cache (lazy (f (l.Force())))

            let inline replace (b: ^b) (fa: ^a Suspend) =
                match fa with
                | Forced _ -> Forced b
                | Thunk t  -> Thunk (fun () -> ignore (t ()); b)
                | Cache l  -> Cache (lazy (ignore (l.Force()) ; b))


        module Applicative =

            let inline wrap (x: ^a) = Forced x

            let inline ap (fv: ^a Suspend) (ff: (^a -> ^b) Suspend) =
                match ff, fv with
                | Forced f, Forced v -> Forced (f v)
                | Forced f, Thunk v  -> Thunk (fun () -> f (v ()))
                | Forced f, Cache v  -> Cache (lazy (f (v.Force())))
                | Thunk f, Forced v  -> Thunk (fun () -> f () v)
                | Thunk f, Thunk v   -> Thunk (fun () -> f () (v ()))
                | Thunk f, Cache v   -> Cache (lazy (f () (v.Force())))
                | Cache f, Forced v  -> Cache (lazy (f.Force() v))
                | Cache f, Thunk v   -> Cache (lazy (f.Force() (v ())))
                | Cache f, Cache v   -> Cache (lazy (f.Force() (v.Force())))

            let inline map2 (f: ^a -> ^b -> ^c) fa fb =
                match fa, fb with
                | Forced a, Forced b -> Forced (f a b)
                | Forced a, Thunk b  -> Thunk (fun () -> f a (b ()))
                | Forced a, Cache b  -> Cache (lazy (f a (b.Force())))
                | Thunk a, Forced b  -> Thunk (fun () -> f (a ()) b)
                | Thunk a, Thunk b   -> Thunk (fun () -> f (a ()) (b ()))
                | Thunk a, Cache b   -> Cache (lazy (f (a ()) (b.Force())))
                | Cache a, Forced b  -> Cache (lazy (f (a.Force()) b))
                | Cache a, Thunk b   -> Cache (lazy (f (a.Force()) (b ())))
                | Cache a, Cache b   -> Cache (lazy (f (a.Force()) (b.Force())))

            let inline lift2 f fa fb =
                match fa, fb with
                | Cache _, _ | _, Cache _ -> Cache (lazy (f (force fa) (force fb)))
                | Thunk _, _ | _, Thunk _ -> Thunk (fun () -> f (force fa) (force fb))
                | Forced a, Forced b -> Forced (f a b)

            let inline map3 (f: ^a -> ^b -> ^c -> ^d) fa fb fc =
                Monad.bind (fun a -> map2 (f a) fb fc) fa 

            // TO DO        


        module Comonad =

            let inline extract (w: ^a Suspend) =
                match w with
                | Forced a -> a
                | Thunk t  -> t ()
                | Cache l  -> l.Force()

            let inline extend (j: ^a Suspend -> ^b) (w: ^a Suspend) =
                match w with
                | Forced _ -> Forced (j w)
                | Thunk _  -> Thunk (fun () -> j w)
                | Cache _  -> Cache (lazy (j w))

            let inline duplicate (w: ^a Suspend) =
                match w with
                | Forced _ -> Forced w
                | Thunk _  -> Thunk (fun () -> w)
                | Cache _  -> Cache (lazy w)


        module Semigroup =

            let inline sappend e1 e2 : ^a Suspend when ^a : (static member Append: ^a -> ^a -> ^a) =
                Applicative.map2 (fun a b -> (^a: (static member Append: ^a -> ^a -> ^a) (a, b))) e1 e2


    let suspend = Compose.Monad.SuspendBuilder ()


    let a =
        memoingly (fun a ->
            suspend {
                let! a = a
                let! b = Thunk (fun () -> printfn "memo thunk" ; 2)
                let! c = Cache (lazy (printfn "memo cache" ; 3))
                return a, b, c }) <| Forced 1

    let b =
        lazily (fun a ->
            suspend {
                let! a = a
                let! b = Thunk (fun () -> printfn "lazy thunk" ; 2)
                let! c = Cache (lazy (printfn "lazy cache" ; 3))
                return a, b, c }) <| Forced 1

    let c =
        eagerly (fun a ->
            suspend {
                let! a = a
                let! b = Thunk (fun () -> printfn "eager thunk" ; 2)
                let! c = Cache (lazy (printfn "eager cache" ; 3))
                return a, b, c }) <| Forced 1


    printfn "--------------"
    force a |> ignore
    printfn "--------------"
    force b |> ignore
    printfn "--------------"
    force c |> ignore