namespace PTR.Context.Type.Incomplete.Reducer

// With current implementation,
// the input-type is coupled to the reduction type,
// ex: summing () |> ... |> fold [] [1..10] throws an error
// MEANING, changes to the INPUT function is coupled to the OUTPUT function


[<Struct>]
type Reduced<'T> = Reduced of Final: ^T | Continue of ^T

[<RequireQualifiedAccess>]
module Reduced =

    let inline map f = function
    | Reduced a  -> Reduced (f a)
    | Continue a -> Continue (f a)

    let inline extract reducer =
        match reducer with
        | Reduced a  -> a
        | Continue a -> a


[<Struct>]
type StateRedux<'State, 'R> = { State : ^State ; Redux : Reduced< ^R> }

[<RequireQualifiedAccess>]
module StateRedux =
    
    let inline map (f: ^a -> ^b) (sr: StateRedux< ^s, ^a>) =
        { StateRedux.State = sr.State
        ; Redux = Reduced.map f sr.Redux }

    let inline extract { StateRedux.Redux = r } = Reduced.extract r

    let inline ofReduced state reduced = { StateRedux.State = state ; Redux = Reduced reduced }
    let inline ofContinued state continued = { StateRedux.State = state ; Redux = Continue continued }


[<Struct; NoComparison; NoEquality>]
type Reducer<'State, 'Input, 'Acc> =
    { Init : unit -> ^State
    ; Complete : ^State -> ^Acc -> ^Acc
    ; Step : ^State -> ^Acc -> ^Input -> StateRedux< ^State, ^Acc> }


type StatelessReducer<'Input, 'Acc> = Reducer<unit, 'Input, 'Acc>


module Reducer =

    let inline private const_id _ x = x


    let inline stateless reduction : StatelessReducer< ^i, ^r> =
        { Reducer.Init = id
        ; Complete = const_id
        ; Step = fun _ r a -> StateRedux.ofContinued () (reduction r a) }

    let inline transduce
        xinit xcomplete xstep (reducer: Reducer< ^s0, ^ia, ^a>) : Reducer< ^s, ^ib, ^b> =
        { Reducer.Init = xinit reducer.Init
        ; Complete = xcomplete reducer.Complete
        ; Step = xstep reducer.Step }


    [<RequireQualifiedAccess>]
    module Stateful =

        let inline summing (ra: Reducer< ^s, ^i, ^r>) =
            let step s r a =
                let add r = r + a in ra.Step s r a |> StateRedux.map add
            { ra with Reducer.Step = step }

        let inline summingBy f (ra: Reducer< ^s, ^i, ^r>) =
            let step s r a =
                let add r = r + f a in ra.Step s r a |> StateRedux.map add
            { ra with Reducer.Step = step }            

        let inline fold seed (source: #seq< ^i>) (reducer: Reducer< ^s, ^i, ^r>) =
            let mutable st = reducer.Init ()
            let mutable ac = seed
            let mutable fl = true
            use e = source.GetEnumerator()
            while fl && e.MoveNext() do
                let srx = reducer.Step st ac e.Current
                st <- srx.State
                match srx.Redux with
                | Continue r -> ac <- r
                | Reduced r  -> ac <- r ; fl <- false
            st, reducer.Complete st ac

        let inline reduce seed (source: ^i) (reducer: Reducer< ^s, ^i, ^a>) =
            let s0 = reducer.Init ()
            let sr = reducer.Step s0 seed source
            sr.State, reducer.Complete sr.State (StateRedux.extract sr)


        let inline mapping (f: ^ia -> ^ib) (rb: Reducer< ^s, ^ib, ^r>)
            : Reducer< ^s, ^ia, ^r> =
            { Reducer.Init = rb.Init
            ; Complete = rb.Complete
            ; Step = fun s r a -> rb.Step s r (f a) }

        let inline filtering (p: ^i -> bool) (ra: Reducer< ^s, ^i, ^a>) =
            let filterStep s acc x =
                if p x then ra.Step s acc x
                else { StateRedux.State = s ; Redux = Continue acc }
            { ra with Reducer.Step = filterStep }


        let inline modifying f (ra: Reducer< ^s, ^i, ^a>) : Reducer< ^s, ^i, ^a> =
            let step s r a = ra.Step (f s) r a
            { ra with Reducer.Step = step }


        let inline taking count (ra: Reducer< ^s, ^i, ^a>) : Reducer< ^s, ^i, ^a> =
            let mutable n = 1
            let step s r a =
                if n <= count then
                    n <- n + 1
                    ra.Step s r a
                else StateRedux.ofReduced s r
            { ra with Reducer.Step = step }

        let inline takingWhile p (ra: Reducer< ^s, ^i, ^a>) : Reducer< ^s, ^i, ^a> =
            let step s r a =
                if p a then ra.Step s r a
                else StateRedux.ofReduced s r
            { ra with Reducer.Step = step }

        let inline listing initf : Reducer< ^s, ^r, ^r list> =
            let cons s xs x = StateRedux.ofContinued s (x::xs)
            let rev _ xs = List.rev xs
            { Reducer.Init = initf
            ; Complete = rev
            ; Step = cons }


    [<RequireQualifiedAccess>]
    module Stateless =

        let inline summing () : StatelessReducer< ^i, ^r> = stateless (+)

        let inline summingBy f : StatelessReducer< ^i, ^r> =
            let sum r a = r + f a in stateless sum


        let inline fold seed (source: #seq< ^i>) (reducer: StatelessReducer< ^i, ^r>) =
            let mutable ac = seed
            let mutable fl = true
            use e = source.GetEnumerator()
            while fl && e.MoveNext() do
                let srx = reducer.Step () ac e.Current
                match srx.Redux with
                | Continue r -> ac <- r
                | Reduced r  -> ac <- r ; fl <- false
            reducer.Complete () ac

        let inline reduce seed (source: ^i) (reducer: StatelessReducer< ^i, ^r>) =
            reducer.Complete () (StateRedux.extract (reducer.Step () seed source))


        let inline mapping (f: ^ia -> ^ib) (rb: StatelessReducer< ^ib, ^r>)
            : StatelessReducer< ^ia, ^r> =
            { Reducer.Init = rb.Init
            ; Complete = rb.Complete
            ; Step = fun s r a -> rb.Step s r (f a) }

        let inline filtering (p: ^i -> bool) (ra: StatelessReducer< ^i, ^r>)
            : StatelessReducer< ^i, ^r> =
            let filterStep _ acc x =
                if p x then ra.Step () acc x
                else { StateRedux.State = () ; Redux = Continue acc }
            { ra with Reducer.Step = filterStep }

        let inline taking count (ra: StatelessReducer< ^i, ^r>)
            : StatelessReducer< ^i, ^r> =
            let mutable n = 1
            let step _ r a =
                if n <= count then
                    n <- n + 1
                    ra.Step () r a
                else StateRedux.ofReduced () r
            { ra with Reducer.Step = step }

        let inline takingWhile p (ra: StatelessReducer< ^i, ^r>)
            : StatelessReducer< ^i, ^r> =
            let step _ r a =
                if p a then ra.Step () r a
                else StateRedux.ofReduced () r
            { ra with Reducer.Step = step }

        let listing<'a> : StatelessReducer< ^a, ^a list> =
            let cons _ xs x = StateRedux.ofContinued () (x::xs)
            let rev _ xs = List.rev xs
            { Reducer.Init = id
            ; Complete = rev
            ; Step = cons }

    let inline ( ^& ) a f = f a //(a: Reducer< ^s, ^ib, ^r>) f : Reducer< ^s, ^ia, ^r> = f a


    //let inline overList (r: Reducer< ^s, ^r, ^a>) source =
    //    let mutable s = r.Init ()
    //    let ra = ResizeArray<_>()
    //    let rec go = function
    //    | [] -> ()
    //    | x::xs ->
    //        let sr = r.Step s a x
    //        match sr.Redux with
    //        | Reduced _ -> ()
    //        | Continue a' ->
    //            s <- sr.State
    //            a <- a'
    //            ra.Add(x)
    //            go xs
    //    go source
    //    Seq.toList ra
    //
    //let inline overSeq (r: Reducer< ^s, ^r, ^a>) (src: ^a seq) =
    //    seq {
    //        let mutable s = r.Init ()
    //        let mutable g = true
    //        use e = src.GetEnumerator()            
    //        while g && e.MoveNext() do
    //            let sr = r.Step s a e.Current
    //            match sr.Redux with
    //            | Reduced _ -> ()
    //            | Continue r ->
    //                s <- sr.State
    //                a <- r
    //                yield e.Current }


    
    //System.Threading.Thread.Sleep 1000
    
    async {
    
        let xs = seq { 1L..10_000_000L }
        let plus1 x = x + 1L
        let even x = x % 2L = 0L
        let divb6 x = x % 5L = 0L
        let intby2 x = int64 x * 2L
        let lt x = x < 7_000_000L
        let stateful : Reducer<int64, int64, int64> =
            { Reducer.Init = fun () -> 0L
            ; Complete = const_id
            ; Step = fun s r _ ->
                StateRedux.ofContinued s r }
        let n_to_take = 9_000_000
        let zero = 0L

        let cull () =        
            System.GC.Collect()
        let t = System.Diagnostics.Stopwatch()


        cull ()
        t.Restart()
        let with_state =
            Stateful.listing (fun () -> 0L)        
            |> Stateful.takingWhile lt
            |> Stateful.taking n_to_take
            |> Stateful.filtering divb6
            |> Stateful.modifying ((+) 1L)
            |> Stateful.mapping (string >> intby2)
            |> Stateful.filtering even
            |> Stateful.fold [] xs
        cull ()
        t.Stop()
        let t_w_state = t.ElapsedMilliseconds


        cull ()
        t.Restart()
        let no_state =
            Stateless.listing<int64>
            |> Stateless.takingWhile lt
            |> Stateless.taking n_to_take
            |> Stateless.filtering divb6
            |> Stateless.mapping (string >> intby2)
            |> Stateless.filtering even
            |> Stateless.fold [] xs
        cull ()
        t.Stop()
        let t_no_state = t.ElapsedMilliseconds
    
        cull ()
        t.Restart()
        let no_state_2 =
            Stateless.listing<int64>
            |> (Stateless.filtering even
                << Stateless.mapping (string >> intby2)
                << Stateless.filtering divb6
                << Stateless.taking n_to_take
                << Stateless.takingWhile lt)
            |> Stateless.fold [] xs
        cull ()
        t.Stop()
        let t_no_state_2 = t.ElapsedMilliseconds


        cull ()
        t.Restart()
        let normal_seq =
            xs
            |> Seq.filter even
            |> Seq.map (string >> intby2)
            |> Seq.filter divb6
            |> Seq.take n_to_take
            |> Seq.takeWhile lt
            |> fun s -> Seq.foldBack (fun x xs -> x::xs) s []
        cull ()
        t.Stop()
        let t_seq_no_state = t.ElapsedMilliseconds

        printfn "same = %b" (snd with_state = no_state && no_state = no_state_2 && no_state = normal_seq)

        let printr name time = printfn "%s = %i" name time

        printr "with_state" t_w_state
        printr "no_state" t_no_state
        printr "no_state_2" t_no_state_2
        printr "normal_seq" t_seq_no_state

    } |> Async.Start