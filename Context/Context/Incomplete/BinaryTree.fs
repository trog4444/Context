namespace PTR.Context.Type//.Incomplete.BinaryTree


//[<Struct; NoComparison; NoEquality>]
//type BinaryTree<'T> =
//    | Leaf   of Value: ^T
//    //| Branch of (unit -> Branches< ^T>)
//    | Branch of ^T Branches Lazy


//and [<Struct; NoComparison; NoEquality>] Branches<'T> =
//    { Left: BinaryTree< ^T> ; Right: BinaryTree< ^T> }


//module BinaryTree =

//    let inline init count initializer =
//        let rec go n =
//            if n >= count - 1 then Leaf (initializer n)
//            else let v = initializer n
//                 Branch (lazy { Branches.Left = Leaf v ; Right = go (n + 1) })
//        go 0

//    let inline iter action tree =
//        let rec go = function
//        | Leaf a -> action a
//        | Branch b ->
//            let t = b.Force()
//            go t.Left
//            go t.Right
//        go tree

//    let xs = ResizeArray<int64>(1_000)
//    let tree = init 100 ((+) 1 >> int64)
//    iter xs.Add tree
//    let rs = Seq.sum xs