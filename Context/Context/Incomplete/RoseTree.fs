namespace Ptr.Context.Incomplete.Type.RoseTree


///
//[<Struct; NoComparison>]
//type RoseTree<'n, 'l> =
//    | Leaf of Value: ^l
//    | Node of Label: struct (^n * RoseTree< ^n, ^l> seq)
//with interface System.Collections.Generic.IEnumerable< ^l> with
//        override s.GetEnumerator() =
//            let rec go t = (seq {
//                match t with
//                | Leaf l -> yield l
//                | Node (_, ts) ->
//                    if isNull ts then () else
//                    yield! Seq.collect go ts }) in (go s).GetEnumerator()
//        override s.GetEnumerator() = (seq s).GetEnumerator() :> System.Collections.IEnumerator


///
//module Pattern =

//    /
//    let inline ( |Leaf|Node| ) (rt: RoseTree< ^node, ^leaf>) =
//        match rt with
//        | Leaf l -> Leaf (l: ^leaf)
//        | Node (struct ((n: ^node), null)) -> Node (struct (n, Seq.empty))
//        | Node (struct ((n: ^node), ts))   -> Node (struct (n, ts))


//open Pattern

///
//module Std =

//    /
//    let inline leaf< ^node, ^leaf> leaf : RoseTree< ^node, ^leaf> = Leaf leaf

//    /
//    let inline node< ^node, ^leaf> node : RoseTree< ^node, ^leaf> = Node (struct (node, Seq.empty))

//    /
//    let inline init (count: uint32) initNode initLeaf : RoseTree< ^node, ^leaf> =
//        let c = count - 1u
//        let rec go = function
//        | n when n >= c -> Leaf (initLeaf n)
//        | n -> Node (initNode n, seq { yield Leaf (initLeaf n)
//                                       yield go (n + 1u) })
//        go 0u

//    /
//    let inline initLong (count: uint64) initNode initLeaf : RoseTree< ^node, ^leaf> =
//        let c = count - 1UL
//        let rec go = function
//        | n when n >= c -> Leaf (initLeaf n)
//        | n -> Node (initNode n, seq { yield Leaf (initLeaf n)
//                                       yield go (n + 1UL) })
//        go 0UL

//    /
//    let inline unfold generator seed : RoseTree< ^node, ^leaf> =
//        let rec go n = seq {
//            match generator n with
//            | None -> ()
//            | Some (struct (n, l)) ->
//                yield Node (n, seq { yield Leaf l
//                                     yield! go n })}
//        Node (seed, go seed)

//    let a = initLong 1_000_000UL id id |> Seq.iter ignore



//    let inline fold fbranch fleaf (seedB: ^sB) (seedL: ^sL) (tree: RoseTree< ^b, ^l>) =
//        let rec go accB accL = function
//        | Pattern.Leaf a -> fleaf accB accL a
//        | Pattern.Branch (b, ts) -> Seq.fold (go (fbranch accB b)) accL ts
//        go seedB seedL tree



//    let inline scan fbranch fleaf (seedB: ^sB) (seedL: ^sL) (tree: RoseTree< ^b, ^l>) =
//        let rec go accB accL = function
//        | Pattern.Leaf a -> Leaf (fleaf accB accL a)
//        | Pattern.Branch (b, ts) -> let b = fbranch accB b
//                                    Branch (b, Seq.map (go b accL) ts)
//        go seedB seedL tree



//    /// Generates trees and subtrees until the generator stops.
//    let inline unfold generator seed =
//        let rec go s =
//            match generator s with
//            | Choice1Of2 (a: ^l) -> Leaf a
//            | Choice2Of2 (s: ^s, b: ^b, ts: RoseTree< ^b, ^l> seq) ->
//                if isNull ts then Branch (b, Seq.empty)
//                else Branch(b, Seq.map (mk s) ts)
//        and mk s = function
//        | Pattern.Leaf _ as l -> l
//        | Pattern.Branch(b, ts) -> Branch(b, seq { yield! ts; yield go s })
//        go seed

//    let inline g s = if s >= 10L then Choice1Of2 s else Choice2Of2 (s + 1L, s, seq { yield branch s })
//    let inline fb s b = b :: s
//    let inline fl sb sl l = (l, sb) :: sl
//    let a = unfold g 0L |> fold fb fl [] []


/////
//module Composition =

//    ///
//    let inline map f (RT ((l, rs) as q)) = q