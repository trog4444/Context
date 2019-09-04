namespace PTR.Context.Type.Incomplete


///
[<Struct; NoComparison; NoEquality>]
type SearchTree<'T> = Leaf of LeafValue: ^T | Nodes of (unit -> SearchTree< ^T>) seq


///
module SearchTree =

    ///
    let ofArray (source: 'a []) : SearchTree< ^a> =
        Nodes [| for x in source -> fun () -> Leaf x |]

    ///
    let ofList (source: 'a list) : SearchTree< ^a> =
        Nodes [| for x in source -> fun () -> Leaf x |]

    ///
    let ofSeq (source: 'a seq) : SearchTree< ^a> =
        Nodes (Seq.map (fun a () -> Leaf a) source)

    ///
    let toSeq (tree: 'a SearchTree) : ^a seq =
        let rec go st = seq {
            match st with
            | Leaf a -> yield a
            | Nodes ns -> yield! Seq.collect (fun st -> go (st ())) ns }
        go tree

    ///
    let inline unfold generator (seed: ^s) =
        let rec go nst = seq {
            match nst with
            | None -> ()
            | Some (t, z) ->
                yield fun () -> Leaf t
                yield! go (generator z) }
        Nodes (go (generator seed))

    ///
    let inline unfoldf generator (seed: ^s) =
        let rec go nst = seq {
            match nst with
            | None -> ()
            | Some (ft, z) ->
                yield ft
                yield! go (generator z) }
        Nodes (go (generator seed))

    ///
    let inline initWide count initializer =
        Nodes (Seq.init count (fun i () -> initializer i))

    ///
    let inline initDeep count initializer =
        let rec go i = seq {
            if i >= count then ()
            else yield fun () -> Leaf (initializer i)
                 yield! go (i + 1) }
        Nodes (go 0)

    ///
    let inline fold folder seed (st: SearchTree<_>) : ^s =
        let rec go z = function
        | Leaf a -> folder z a
        | Nodes fs -> Seq.fold (fun s t -> go s (t ())) z fs
        go seed st

    ///
    let inline iter action st =
        let rec go = function
        | Leaf t -> action t
        | Nodes ns -> Seq.fold (fun u t -> go (t u)) () ns
        go st


    ///
    module Compose =

        ///
        let wrap x = Leaf x

        ///
        let inline map f fa =
            let rec go = function
            | Leaf a -> Leaf (f a)
            | Nodes ns -> Nodes (Seq.map (fun st () -> go (st ())) ns)
            go fa

        ///
        let inline ap fv ff =
            let rec goF = function
            | Leaf f -> goV f fv
            | Nodes ns -> Nodes (Seq.map (fun st () -> goF (st ())) ns)
            and goV f = function
            | Leaf v -> Leaf (f v)
            | Nodes ns -> Nodes (Seq.map (fun st () -> goV f (st ())) ns)
            goF ff

        ///
        let inline bind k m =
            let rec go = function
            | Leaf a -> k a
            | Nodes ns -> Nodes (Seq.map (fun st () -> go (st ())) ns)
            go m

        ///
        let flatten mm =
            let rec go = function
            | Leaf st -> st
            | Nodes ns -> Nodes (Seq.map (fun st () -> go (st ())) ns)
            go mm