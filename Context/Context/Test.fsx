#load "Data\Attr.fs"
#load "Contexts\Attr.fs"
#load "Data\Attr.fs"
#load "Contexts\Attr.fs"

open Rogz.Context.Data.Attr.Attr

let a, b = proxy<int>, proxy<string>

let c = typeOf a |> fun x -> x.Assembly.ToString()


// #load "FolderName\FileName.fs"



#load "Data\Cont.fs"
#load "Contexts\Cont.fs"
open Rogz.Context.Data.Cont
open System.Linq
let xxx =
    [| for i = 1 to 10 do
        do printfn "%i" i
        yield Cont.unit i |]
    |> Cont.sequence
    |> Cont.map (fun x -> printfn "%i" (x.Count()))
    |> Cont.evalCont


//#load "Data\RWS.fs"
//#load "Contexts\RWS.fs"
//open Rogz.Context.Data.RWS
//module R = Rogz.Context.Data.RWS.RWS

//type T = T of int with
//    static member Empty() = T 0
//    static member Append((T a), T b) = T <| a + b

//let inline f j k a =
//    if a >= 100_000_000 then R.unit a
//    elif a % 2 = 0 then j (a + 1)
//    else k <| RWS (fun e s -> { RWSResult.State = s + 1; Log = T s; Value = a + e })
//let a = 1
//let rj = R.runRWS 1 1 <| R.fixM f (Rogz.Context.Data.Either.Left a)
//let rk = R.runRWS 1 1 <| R.fixM f (Rogz.Context.Data.Either.Right (RWS (fun _ _ -> { RWSResult.State = 10; Log = T -999; Value = a })))

//let j1 = Just 1
//let j2 = Just 2
//let jadd = Just ((+) 1)
//let n = M.empty<int>
//let even n = n % 2 = 0

//let ``j1_map_string: Just "1"`` = j1 |%> string
//let ``n_map_string: Nothing`` = n |%> string

//let ``j1_replaceby_3: Just 3`` = j1 %> 3

//let ``j2_from_j1: Just 2`` = j1 *> j2
//let ``j1_over_n: Just 1`` = n <|> j1
//let ``j1_plus1_ap: Just 2`` = jadd <*> j1

//let ``j1_filtered_to_nothing: Nothing`` = j1 ?> even
//let ``j2_unchanged_by_filter: Just 2`` = j2 ?> even

//let ``j1_bind_string: Just "1"`` = j1 >>= (string >> M.unit)