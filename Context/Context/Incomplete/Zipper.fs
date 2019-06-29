namespace Ptr.Context.Incomplete.Type.Zipper

// http://learnyouahaskell.com/zippers

///
////[<Struct>]
////type Zipper<'a> = { Left: ^a list ; Focus: ^a ; Right: ^a list }
////with interface System.Collections.Generic.IEnumerable< ^a> with
////      override s.GetEnumerator() =
////            (seq { yield! s.Left
////                   yield  s.Focus
////                   yield! s.Right }).GetEnumerator()
////      override s.GetEnumerator() = (s :> _ seq).GetEnumerator() :> System.Collections.IEnumerator

/////
//module Pattern =

//    ///
//    let inline ( |Left| ) { Left = l } = Left l

//    ///
//    let inline ( |Focus| ) { Focus = f } = Focus f

//    ///
//    let inline ( |Right| ) { Right = r } = Right r


/////
//module Std =

//    ///
//    let inline leftMaybe (lz: ^a Zipper) =
//        match lz.Left with
//        | [] -> None
//        | x::xs -> Some { Left = xs ; Focus = x ; Right = lz.Focus::lz.Right }

//    ///
//    let inline left z = Option.defaultValue z (leftMaybe z)

//    ///
//    let inline rightMaybe (lz: ^a Zipper) =
//        match lz.Right with
//        | [] -> None
//        | x::xs -> Some { Left = lz.Focus::lz.Left ; Focus = x ; Right = xs }

//    ///
//    let inline right z = Option.defaultValue z (rightMaybe z)


/////
//module Composition =

//    ///
//    let inline wrap x = { Left = [] ; Focus = x ; Right = [] }    

//    ///
//    let inline ap (mv: ^a Zipper) (mf: (^a -> ^b) Zipper) =
//        { Left = [ for f in mf.Left do for v in mv.Left -> f v ]
//        ; Focus = mf.Focus mv.Focus
//        ; Right = [ for f in mf.Right do for v in mv.Right -> f v ]}

//    ///
//    let inline map f (m: ^a Zipper) =
//        { Left = List.map f m.Left ; Focus = f m.Focus ; Right = List.map f m.Right }


//    ///
//    module Comonad =

//        ///
//        let inline extract { Focus = x } = x

//        ///
//        let inline duplicate (w: ^a Zipper) : Zipper<Zipper< ^a>> =
//            let rec iterate f x = seq {
//                yield x
//                yield! iterate f (f x) }
//            let inline gather f = Seq.tail << Seq.choose id << Seq.takeWhile Option.isSome << iterate (Option.bind f) << Some
//            let lefts = gather Std.leftMaybe w |> Seq.toList
//            let rights = gather Std.rightMaybe w |> Seq.toList
//            { Left = lefts ; Focus = w ; Right = rights }

//        ///
//        let inline extend j w = map j (duplicate w)