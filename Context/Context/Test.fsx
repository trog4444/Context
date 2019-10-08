let test (timeout_ms: int64) (name: string) (work: Async<'a>) =
    let err = async {
        let t = System.Diagnostics.Stopwatch.StartNew()
        while t.ElapsedMilliseconds < timeout_ms do ()
        return failwith "test timed out" }
    let work = async { let t = System.Diagnostics.Stopwatch.StartNew()
                       let! w = work
                       System.GC.Collect()
                       let time = t.ElapsedMilliseconds
                       return Some (name, w, time) }
    async {
        do! Async.SwitchToThreadPool()
        let! w = Async.Choice [work; err]
        printfn "test result = %A" w.Value }

// test result @ 50_000_000 => ((), 29493L)
let inline unlim1 s f =
    let rec st s () = go (f s)
    and go s = Seq.append (Seq.singleton s) (Seq.delay (st s))
    go s

// test result @ 50_000_000 => ((), 5122L)
let inline unlim1' s f =
    let rec go s =
        seq { yield s
              yield! go (f s) }
    go s

// test result @ 50_000_000 => ((), 1669L)
let inline unlim2 s f =
    seq { let mutable s = s
          yield s
          while true do
            s <- f s
            yield s }

// test result @ 50_000_000 => ((), 3183L)
let inline unlim3 s f =
    Seq.unfold (fun s -> Some (s, f s)) s


let inline taken f =
    async {
        let xs = System.Linq.Enumerable.AsEnumerable(f ())
        ignore <| System.Linq.Enumerable.ToArray(System.Linq.Enumerable.Take(xs, 50_000_000)) }
let prep () = let mutable arr = Array.init 1000000 id
              for _ in arr do ()
              arr <- null
              System.GC.Collect()
let inc x = x + 1
let time = 45_000L
let testing = test time
prep ()
Async.Sequential [
    testing "unlim1" (taken (fun () -> unlim1 1 inc))
    testing "unlim1'" (taken (fun () -> unlim1' 1 inc))
    testing "unlim2" (taken (fun () -> unlim2 1 inc))
    testing "unlim3" (taken (fun () -> unlim3 1 inc)) ]
|> fun w -> async { let! _ = w in () }
|> Async.Start


#load @"Contexts/Maybe.fs"
#load @"Builders/Maybe.fs"
//#load @"Contexts/Either.fs"
//#load @"Builders/Either.fs"

#load @"Operators/Operators.fs"


open PTR.Context.Type.Maybe
open PTR.Context.Type.Maybe.Maybe
//open PTR.Context.Builder.Maybe
//open PTR.Context.Builder.Maybe.Build.Extensions
//open PTR.Context.Builder.Maybe.Build.Linq
//open PTR.Context.Type.Either
//open PTR.Context.Type.Either.Either
open PTR.Context.Operators

let a : Maybe<int> = Just 1 >>= unit
let b : Maybe<int> = unit =<< Just 2

let c : Maybe<int> = Just id <*> Just 3
let d : Maybe<int> = Just 4 <**> Just id

let e : Maybe<int> = Just "-5" *> Just 5
let f : Maybe<int> = Just 6 <* Just "-6"

let g : Maybe<int> = Just 7 |%> id
let h : Maybe<int> = id <%| Just 8

let i : Maybe<int> = Just "-9" %> 9
let j : Maybe<int> = 10 <% Just "-10"


let r = ref 0
type T = T of int with
    member s.Get = let (T a) = s in a
    interface System.IDisposable with
        member _.Dispose() = incr r

let work = async {
    let a = maybe {
        let r0 = !r
        use! d1 = Just <| T 1
        let! ex = try Just "exn" with _ -> Nothing
        do! try Just () finally ()
        let r1 = !r
        use! d2 = Just <| T 2
        let r2 = !r
        use! d3 = Just <| T 3
        return ex, [r0; r1; r2; !r], [d1.Get; d2.Get; d3.Get] }
    return a, !r }
    
test 1000000L work |> Async.Start





////100. * (43555.0 - 41895.0) / ((43555.0 + 41895.0) / 2.)

////// outref<_> can be used as output but only in limited circumstances.
////// inref<_> is equivalent to 'in' keyword in C#, in that it passes 
////// the value by reference (in f# this requires passing a MUTABLE value by address (let mutable x = ... in &x)
////// this CAN improve performance but only in certain circumstances.
////// example:
//////  in Writer.recM, replacing the 'append' with:
//////      let inline private appendn (a: inref< ^w>) b = (^w : (static member Append: ^w -> ^w -> ^w) (a, b))
////// and looping 3 billion times (with append operation simply doing addition), time only improved 3.8%
////// which was NOTHING => 43555 ms VS 41895 ms
////let inline ( /> ) (x: inref<_>) f = f x
////let inline ( </ ) f (x: inref<_>) = f x

////let mutable x = 1
////let f x = x, 'a'
////let aref = &x /> f
////let bref = f </ &x