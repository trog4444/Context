namespace Ptr.Context.Incomplete.Type.Tuple


///// 2-arity tuple.
//[<Struct>]
//type Tuple2<'a, 'b> = Tuple2 of _1st: ^a * _2nd: ^b
//with interface System.Collections.Generic.IEnumerable< ^b> with
//        override s.GetEnumerator() = match s with Tuple2 (_2nd = x) -> (Seq.singleton x).GetEnumerator()
//        override s.GetEnumerator() = (s :> ^b seq).GetEnumerator() :> System.Collections.IEnumerator

///// 3-arity tuple.
//[<Struct>]
//type Tuple3<'a, 'b, 'c> = Tuple3 of _1st: ^a * _2nd: ^b * _3rd: ^c
//with interface System.Collections.Generic.IEnumerable< ^c> with
//        override s.GetEnumerator() = match s with Tuple3 (_3rd = x) -> (Seq.singleton x).GetEnumerator()
//        override s.GetEnumerator() = (s :> ^c seq).GetEnumerator() :> System.Collections.IEnumerator


/////
//module Arity2 =

//    failwith "undefined"

//    ///
//    module Std =
        
//        let x : unit = failwith "undefined"
    

//    ///
//    module Composition =

//        ///
//        let inline map f (Tuple2 (a, b)) = Tuple2 (a, f b)
        
            

/////
//module Arity3 =

//    failwith "undefined"


//open Arity2.Primitive
//open Arity2.Composition

//type Tuple2<'a, 'b> with


//// @ Functor @

//    ///
//    static member inline (|>>) (fa, f) = map f fa