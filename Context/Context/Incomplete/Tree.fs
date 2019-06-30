namespace Ptr.Context.Incomplete.Type.Tree


///
//[<Struct; NoComparison; NoEquality>]
//type TreeNode<'a> = { Leaf: ^a ; TLeft: unit -> ^a Tree ; TRight: unit -> ^a Tree }


/////
//and [<Struct; NoComparison; NoEquality>] Tree<'a> = TNil | Node of ^a TreeNode


/////
//module Compose =

//    ///
//    let inline wrap x = Node { Leaf = x ; TLeft = (fun () -> TNil) ; TRight = fun () -> TNil }

//    ///
//    let inline map f (m: ^a Tree) : ^b Tree =
//        let rec go = function
//        | TNil -> TNil
//        | Node t -> Node { Leaf = f t.Leaf ; TLeft = (t.TLeft >> go) ; TRight = t.TRight >> go }
//        go m

//    ///
//    let inline ap (mv: ^a Tree) (mf: (^a -> ^b) Tree) : ^b Tree =
//        let rec go = function
//        | TNil -> TNil
//        | Node t -> Node { Leaf = }