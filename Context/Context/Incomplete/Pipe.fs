namespace Ptr.Context.Incomplete.Type.Pipe



//// 'Port' of "Pipes" from Haskell by Gabriel Gonzalez.

///// Bidirectional streaming.
/////
///// a: Upstream interface - a0's go out and a's come in.
/////
///// b: Downstream interface - b's go out and b0's come in.
/////
///// r: The final return value.
//[<Struct; NoComparison; NoEquality>]
//type Pipe<'a0, 'a, 'b0, 'b, 'r> =
//    | Yield      of Yld: ^r
//    | Upstream   of Up : struct (^a0 * (^a  -> Stream< ^a0, ^a, ^b0, ^b, ^r>))
//    | DownStream of Dw : struct (^b  * (^b0 -> Stream< ^a0, ^a, ^b0, ^b, ^r>))


/////
//[<Struct>]
//type Close = internal Close of unit

/////
//type Effect<'r> = Stream<Close, unit, unit, Close, 'r>

/////
//type Producer<'b, 'r> = Stream<Close, unit, unit, 'b, 'r>

/////
//type Pipe<'a, 'b, 'r> = Stream<unit, 'a, unit, 'b, 'r>

/////
//type Consumer<'a, 'r> = Stream<unit, 'a, unit, Close, 'r>

/////
//type Client<'a0, 'a, 'r> = Stream<'a0, 'a, unit, Close, 'r>

/////
//type Server<'b0, 'b, 'r> = Stream<Close, unit, 'b0, 'b, 'r>


/////
//module Std =

//    ///
//    let inline unfold g z0 : Stream< ^a0, ^a, ^b0, ^b, ^b0> =
//        let rec go z = function
//        | None -> Yield z
//        | Some (t, z) -> DownStream (struct (t, fun a -> go z (g a)))
//        go z0 (g z0)

//    let (*inline*) go () = Close ()

//    let inline withStream () = ()
//    let inline readStream () = ()
//    let inline mapStream () = ()
//    let inline yld () = ()
//    let inline await () = ()



/////
//module Composition =

//    ///


//    ///


//    ///


//    ///
//    let inline map f t =
//        let rec go = function
//        | Yield r -> Yield (f r)
//        | Upstream (struct (a0, f)) -> Upstream (struct (a0, f >> go))
//        | DownStream (struct (b, g0)) -> DownStream (struct (b, g0 >> go))
//        go t