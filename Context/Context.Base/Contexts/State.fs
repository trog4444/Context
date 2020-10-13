namespace Rogz.Context.Base


[<Struct; NoComparison; NoEquality>]
type State<'S, 'T> = State of ('S -> struct('S * 'T))
with

// Function

    member this.Invoke(state) = let (State f) = this in f state

    static member Of(func: System.Func<'s, struct ('s * 'a)>) : State<'s, 'a> =
        State(fun s -> let struct (s, a) = func.Invoke(s) in (s, a))

// Semigroup

    //[<CompiledName("Append")>]
    static member inline ( + ) ((State a), (State b)) : State< ^s, ^a> =
        State (fun (s: ^s) ->
            let struct (s, a: ^a) = a s
            let struct (s, b: ^a) = b s
            in struct (s, a + b))


module State =

// Haskell Primitives

    let get<'s> : State< ^s, ^s> = State (fun s -> (s, s))

    let put (state: 's) = State (fun _ -> (state, ()))

    let stateful (func: System.Func<'s, struct('s * 'a)>) = State (func.Invoke)

    let inline modify modification : State< ^s, unit> =
        State (fun s -> (modification s, ()))

    let inline gets (proj: ^s -> ^a) = State (fun s -> (s, proj s))

    let runState initial (State f) : struct ('s * 'a) = f initial

    let evalState (initial: 's) (State f) : 'a =
        let struct (_, a) = f initial in a

    let execState initial (State (f: 's -> struct(^s * 'a))) : ^s =
        let struct (s, _) = f initial in s

    let inline mapState mapping (State f) : State< ^s, ^b> =
        State (fun s -> mapping ((f s): struct(^s * ^a)))

    let inline withState f (State st) : State< ^s, ^a> =
        State (fun s -> st (f s))


// Functor

    let inline map (f: ^a -> ^b) (State st) =
        State (fun (s: ^s) -> let struct (s, a) = st s in (s, f a))


// Applicative

    [<CompiledName("Unit")>]
    let unit (value: 'a) : State<'s, ^a> =
        State (fun s -> (s, value))

    let inline ap (State fv) (State ff) : State< ^s, ^b> =
        State (fun s -> let struct (s, f) = ff s
                        let struct (s, v: ^a) = fv s
                        (s, f v))

    let inline map2 (f: ^a -> ^b -> ^c) (State fa) (State fb) : State< ^s, ^c> =
        State (fun s -> let struct (s, a) = fa s
                        let struct (s, b) = fb s
                        (s, f a b))

    let sequence (source: #seq<State<'s, 'a>>) : State<'s, seq<'a>> =
        State (fun s ->
            let mutable s = s
            let xs = ResizeArray<_>()
            for (State f) in source do
                let struct(s',v) = f s
                s <- s'
                xs.Add(v)
            (s, xs :> seq<_>))

//    //let sequence_ (source: #seq<State<'s, 'a>>) : State<'s, unit> =
//    //    State (fun s ->
//    //        let mutable s = s
//    //        for (State f) in source do s <- (f s).State
//    //        { SVPair.State = s; Value = () })

    let inline traverse (f: ^a -> State< ^s, ^b>) (source: #seq< ^a>) : State< ^s, seq< ^b>> =
        State (fun s ->
            let mutable s = s
            let xs = ResizeArray<_>()
            for x in source do
                let (State st) = f x
                let struct(s',v) = st s
                s <- s'
                xs.Add(v)
            (s, xs :> seq<_>))

//    //let inline traverse_ (f: ^a -> State< ^s, ^b>) (source: #seq< ^a>) : State< ^s, unit> =
//    //    State (fun s ->
//    //        let mutable s = s
//    //        for a in source do let (State g) = f a in s <- (g s).State
//    //        { SVPair.State = s; Value = () })


// Monad

    let inline bind (f: ^a -> State< ^s, ^b>) (State m) =
        State (fun s -> let struct(s,a) = m s in let (State sf) = f a in sf s)

    let flatten (State mm) : State<'s, 'a> =
        State (fun s -> let struct(s,(State m)) = mm s in m s)

    let inline fixM loop (em: Choice< ^a, State< ^s, ^a>>) : State< ^s, ^b> =
        State (fun s ->
            let mutable s = s
            let rec go (State st) =
                let struct(s',v) = st s
                s <- s'
                k v
            and k a = loop k go a
            let (State st) = match em with
                             | Choice1Of2 a -> k a
                             | Choice2Of2 m -> go m in st s)

//    // foldlM

//    //let inline foldlM (f: ^z -> ^a -> State< ^s, ^z>) (seed: ^z) (source: #seq< ^a>) : State< ^s, ^z> =
//    //    State (fun s ->
//    //        let mutable s = s
//    //        let mutable z = seed
//    //        for x in source do
//    //            let (State p) = f z x
//    //            let sv = p s
//    //            s <- sv.State
//    //            z <- sv.Value
//    //        { State = s; Value = z })

//    //let inline foldlm (f: ^z -> ^a -> State< ^s, ^z>) (seed: ^z) (source: #seq< ^a>) : State< ^s, ^z> =
//    //    let (>>=) m f = bind f m
//    //    let x : State< ^s, ^z> = Seq.foldBack (fun a k z -> f z a >>= k) source unit seed
//    //    x
//    //let inline foldrm (f: ^a -> ^z -> State< ^s, ^z>) (seed: ^z) (source: #seq< ^a>) : State< ^s, ^z> =
//    //    let (>>=) m f = bind f m
//    //    let x : State< ^s, ^z> = Seq.fold (fun k a z -> f a z >>= k) unit source seed
//    //    x


    [<RequireQualifiedAccess>]
    module Workflow =

        type StateBuilder() =
            member _.Return(x: 'a) : State<'s, 'a> = unit x
            member _.ReturnFrom(m) : State<'s, 'a> = m
            member _.Zero() : State<'s, unit> = unit ()
            member inline _.Bind(m, f: ^a -> State< ^s, ^b>) = bind f m            

    let state = Workflow.StateBuilder()


// Semigroup

    let inline append (State b) (State a) : State< ^s, ^a> =
        State (fun (s: ^s) ->
            let struct(s,a: ^a) = a s
            let struct(s,b: ^a) = b s
            (s, a + b))