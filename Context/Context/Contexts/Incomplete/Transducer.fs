namespace Context.Type.Incomplete


/////
//[<Struct>]
//type Step<'s, 'a> = { State: ^s ; Value: ^a }

/////
//[<Struct>]
//type Reducer<'a> =
//    | Continue of C: ^a
//    | Reduced  of R: ^a

/////
//[<Struct; NoComparison; NoEquality>]
//type Transducer<'s0, 's1, 'a, 'b> =
//    Tranducer of (Step< ^s0, ^a> -> Step< ^s1, Reducer< ^b>>)


/////
//module Transducer =

//    let inline sumN (n: int) (source: int list) : Transducer<int * int list, int> =
//        let rec go () =
//            Transducer (fun { Step.State = (c, xs) as p ; Value = sum } ->
//                if c >= n then
//                    { Step.State = p ; Value = Reduced sum }
//                else
//                    match xs with
//                    | [] -> { Step.State = p ; Value = Reduced sum }
//                    | x::xs -> let c' = c + 1
//                               let sum' = sum + x
//                               { Step.State = (c', xs) ; Value = Continue sum' })
//        go (fun { Step.State = (0, source) ; Value = Continue 0 })
