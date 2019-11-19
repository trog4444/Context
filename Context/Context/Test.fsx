
let inline fixs loop xs =
    let rec go xs = Seq.collect k xs
    and k a = loop k go a
    match xs with Choice1Of2 a -> k a | Choice2Of2 xs -> go xs


let loop f g a =
    if a >= 100 then Seq.singleton (string a)
    elif a % 2 = 0 then f (a + 1)
    else g (Seq.replicate 5 a |> Seq.map ((+) 1))


let r = fixs loop <| Choice1Of2 1 |> Seq.toList
let l = List.length r


let inline y2 f g x =
    let rec go1 a = f go2 a
    and go2 b = g go1 b
    go1 x

let inline ``y2?`` f x =
    let rec go a = k a //--this is wrong, monad version is go m = bind k m
    and k a = f k go a
    go x
// ^^ see below and compare
type Id<'a> = Id of 'a
let inline runId (Id a) = a
let inline bind f (Id a) : Id< ^b> = f a
let inline loopm f x =
    let rec go m = bind k m
    and k a = f k go a
    k x

let inline f k x =
    if x >= 100 then
        string x
    else
        printfn "k: %i" x
        k (x + 1)
let inline g j x =
    printfn "j: %i" x
    if x % 2 = 0 then
        j (x + 3)
    else j (x + 1)

let inline h f g x =
    if x >= 100 then x
    elif x % 2 = 0 then
        printfn "h-f: %i" x
        f (x + 1)
    else
        printfn "h-g: %i" x
        g (x + 3)

let a = y2 f g 1
let b = ``y2?`` h 1

let inline force (value : Lazy<_>) = value.Force()

let inline fix f = let rec x = lazy (f x) in x

let inline recM f = let rec x = f (lazy (Option.get x)) in lazy x

// Examples
let fac = fix (fun f x -> if x = 0 then 1 else x * force f (x - 1))
let sum = fix (fun f x -> if x >= 1_000_000_000 then x else force f (x + 1))
let nums = fix (fun v -> seq { yield 0; yield! Seq.map ((+) 1) (force v) }) 
//let some = recM (fun a -> let x = force a in Some x)


let rsum = force sum 1
//let s : int option = force some
let a = force fac 10 // 10! = 3628800
let b = Seq.take 10 (force nums) |> Seq.toList // seq [0; 1; 2; 3; ...]




#load "Contexts\Maybe.fs"
#load "Operators.fs"
open Rogz.Context.Extra.Operators
open Rogz.Context.Data.Maybe
module M = Maybe


let j1 = Just 1
let j2 = Just 2
let jadd = Just ((+) 1)
let n = M.empty<int>
let even n = n % 2 = 0

let j1_map_string = j1 |%> string
let n_map_string = n |%> string

let j1_replaceby_3 = j1 %> 3

let j2_from_j1 = j1 *> j2
let j1_over_n = n <|> j1
let j1_plus1_ap = jadd <*> j1

let j1_filtered_to_nothing = j1 ?> even
let j2_unchanged_by_filter = j2 ?> even

let j1_bind_string = j1 >>= (string >> M.unit)




















#load "Contexts\Cont.fs"
open Rogz.Context.Data.Cont

let a : Cont<int, string> = Cont.unit "1"



[<Interface>]
type IConj<'a> =
    abstract member Conj: a: ^a -> IConj< ^a>
    abstract member ToList: unit -> ^a list
    abstract member ToSeq: unit -> ^a seq
    inherit System.Collections.Generic.IEnumerable<'a>


[<Struct>]
type LL<'a> = NilLL | NodeLL of struct ('a * (unit -> LL<'a>)) with
    interface IConj<'a> with
         override s.Conj x =
             let rec go = function
             | NilLL -> NodeLL (x, fun () -> NilLL)
             | NodeLL _ as n -> NodeLL (x, fun () -> n)
             go s :> _ IConj

         override s.ToList() =
             let r = ResizeArray<_>()
             let rec go = function
             | NilLL -> ()
             | NodeLL (a, f) -> go (f (r.Add(a)))
             go s
             Seq.toList r

         override s.ToSeq() =
             let rec go m = seq {
                 match m with
                 | NilLL -> ()
                 | NodeLL (a, f) -> yield a
                                    yield! go (f ()) }
             go s

         override s.GetEnumerator() = (s :> _ IConj).ToSeq().GetEnumerator()
         override s.GetEnumerator() = (s :> _ IConj).GetEnumerator() :> System.Collections.IEnumerator


[<Struct>]
type Vector<'a> private (?x: ^a, ?v: Vector< ^a>) =
    member private s.Map =
        match x, v with
        | Some x, Some v -> v.Map 
    member private s.X = x
    member private s.V = v
    member private s.I = match v with Some v -> v.I + 1 | None -> 0
    //static member inline Empty() : ^T Vector = Unchecked.defaultof<Vector< ^T>> //Vector(?x = None, ?v = None)    
    //new () = Vector(?x = None, ?v = None)
    new (x: ^a) = Vector(?x = Some x, ?v = None)
    interface IConj<'a> with

        override s.Conj x = Vector(?x = Some x, ?v = Some s) :> _ IConj

        override s.ToList() =
            let rec go acc (vec: _ Vector) =
                match vec.V, vec.X with
                | Some v, Some x -> go (x::acc) v
                | None, Some x -> x::acc
                | _ -> acc
            go [] s

        override s.ToSeq() =
            //let rec go (vec: _ Vector) = seq {
            //    match vec.V, vec.X with
            //    | Some v, Some x -> yield x; yield! go v
            //    | None, Some x -> yield x
            //    | _ -> () }
            //go s
            let s = s :> _ IConj in seq { yield! s.ToList() }

        override s.GetEnumerator() = ((s :> _ IConj).ToList() :> _ seq).GetEnumerator()
        override s.GetEnumerator() = (s :> _ IConj).GetEnumerator() :> System.Collections.IEnumerator



let emptyv<'T> : Vector< ^T> = Unchecked.defaultof<Vector< ^T>>




type LIST<'T> = MT | LST of struct ('T * LIST<'T>) with
    interface IConj<'T> with
        
        override s.Conj x = (match s with MT -> LST (x, s) | LST _ as t -> LST (x, t)) :> _ IConj
        
        override s.ToList() =
            let r = ResizeArray<_>()
            let rec go = function
            | MT -> ()
            | LST (h, t) -> r.Add(h); go t
            go s
            Seq.toList r

        override s.ToSeq() =
            let rec go lst = seq {
                match lst with
                | MT -> ()
                | LST (h, t) -> yield h; yield! go t }
            go s

        override s.GetEnumerator() = (s :> _ IConj).ToSeq().GetEnumerator()
        override s.GetEnumerator() = (s :> _ IConj).GetEnumerator() :> System.Collections.IEnumerator


// doesnt work cause go (f ()) calls all funcs within f therefore stack overflow
//[<Struct>]
//type Vect<'a> = NilV | NodeV of struct ('a * (unit -> Vect<'a>))
//with interface IConj<'a> with
//         override s.Conj x =
//             let rec go = function
//             | NilV -> NodeV (x, fun () -> NilV)
//             | NodeV (a, f) -> NodeV (a, fun () -> go (f ()))
//             go s :> _ IConj
//         override s.ToList() =
//             let r = ResizeArray<_>()
//             let rec go = function
//             | NilV -> ()
//             | NodeV (a, f) -> go (f (r.Add(a)))
//             go s
//             Seq.toList r
//         override s.ToSeq() =
//             let rec go m = seq {
//                 match m with
//                 | NilV -> ()
//                 | NodeV (a, f) -> yield a
//                                   yield! go (f ()) }
//             go s



let inline into xs (c: IConj<_>) =
    let mutable c = c
    for x in xs do c <- c.Conj x
    c

let xs = seq { 1L..100_000L }


#time "on"
printfn "Vector:  %A" ((into xs emptyv).ToSeq() |> Seq.toList)
#time "off"

#time "on"
printfn "LL:  %A" ((into xs NilLL).ToSeq() |> Seq.toList)
#time "off"

#time "on"
printfn "LIST:  %A" ((into xs MT).ToSeq() |> Seq.toList)
#time "off"



//#time "on"
//printfn "Vect:  %A" ((into xs (NilV)).ToList())
//#time "off"