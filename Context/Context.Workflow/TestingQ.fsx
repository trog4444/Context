//#r @"bin\Release\netcoreapp3.1\Context.Base.dll"
//open Rogz.Context.Base
//module M = Rogz.Context.Base.Maybe
//#load @"MaybeQ.fs"
//module Q = Rogz.Context.Workflow.Maybe


//let inline query_monad () =
//    Maybe.maybe {
//        let! a = Just 1
//        let! b = Just 2
//        let! a0 = Just 1
//        let! a1 = Just 1
//        let! a2 = Just 1
//        let! a3 = Just 1
//        let! a4 = Just 1
//        let! a5 = Just 1
//        let! a6 = Just 1
//        let! a7 = Just 1
//        let! a8 = Just 1
//        let! a9 = Just 1
//        let! c = Just 5
//        let! a0w = Just 1
//        let! a1w = Just 1
//        let! a2w = Just 1
//        let! a3w = Just 1
//        let! a4w = Just 1
//        let! a5w = Just 1
//        let! a6w = Just 1
//        let! a7w = Just 1
//        let! a8w = Just 1
//        let! a9w = Just 1
//        return a, b, c
//        }

//let inline query_applicative () =
//    M.maybe {
//        zip a in Just 1
//        zip b in Just 2
//        zip a0 in Just 1
//        zip a1 in Just 1
//        zip a2 in Just 1
//        zip a3 in Just 1
//        zip a4 in Just 1
//        zip a5 in Just 1
//        zip a6 in Just 1
//        zip a7 in Just 1
//        zip a8 in Just 1
//        zip a9 in Just 1
//        zip c in Just 5
//        zip a0w in Just 1
//        zip a1w in Just 1
//        zip a2w in Just 1
//        zip a3w in Just 1
//        zip a4w in Just 1
//        zip a5w in Just 1
//        zip a6w in Just 1
//        zip a7w in Just 1
//        zip a8w in Just 1
//        zip a9w in Just 1
//        yield (a, b, c)
//        } //23, 13

//let inline query_applicative_direct () =
//    let f a b a03 a04 a05 a06 a07 a08 a09 a10 a11 a12 c a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 =
//        match a, b, a03, a04, a05, a06, a07, a08, a09, a10, a11, a12, c, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23 with
//        | Just a, Just b, _, _, _, _, _, _, _, _, _, _, Just c, _, _, _, _, _, _, _, _, _, _ -> Just (a, b, c)
//        | _ -> Nothing
//    f (Just 1) (Just 2) (Just 1) (Just 1) (Just 1) (Just 1) (Just 1) (Just 1) (Just 1) (Just 1) (Just 1) (Just 1) (Just 5) (Just 1) (Just 1) (Just 1) (Just 1) (Just 1) (Just 1) (Just 1) (Just 1) (Just 1) (Just 1)


//let same = query_applicative () = query_monad ()

//let range = Array.init 2_000 id
//let iters = Array.zeroCreate<int> 2_000

//let startup_delay delay =
//    if delay then System.Threading.Thread.Sleep 1000
//    System.GC.Collect()
//    System.GC.GetTotalMemory true |> ignore

//let inline test (name: string) (f: unit -> ^a) : string =
//    let t = System.Diagnostics.Stopwatch.StartNew()
//    (for _ = 1 to 3 do
//     for _ in iters do
//     for _ in range do
//         ignore (f ()))
//    |> ignore
//    startup_delay false
//    let time = t.ElapsedMilliseconds
//    sprintf "Test: %s    |    %i" name time



//startup_delay true

//let monad_workflow = test "monad" query_monad

//startup_delay true
//startup_delay true

//let apply_workflow = test "apply" query_applicative

//startup_delay true

//let apply_direct = test "apply-direct" query_applicative_direct