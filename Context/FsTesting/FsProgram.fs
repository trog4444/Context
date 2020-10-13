namespace FsTesting

open Rogz.Context.Base
open Rogz.Context.Linq
open Rogz.Context.Base.Cont


module Util =

    #if DEBUG
    let inRelease = false
    #else
    let inRelease = true
    #endif



    //let x = Just 1
    //let y = x.Select(fun x -> x + 1)
    //let z = LinqMaybe.Zip(x, y);

    ///// LOL
    //let inline (<|>) a b = a <|> b
    //let inline (<+>) a b = a + b
    //type Sum = Sum of int with
    //    static member ( + ) ((Sum a), Sum b) = Sum (a + b)

    //let inline asum xs = Array.reduceBack (<|>) xs
    //let inline mconcat (xs: Maybe<Sum> []) = Array.foldBack (<+>) xs Nothing

    //let e = Left 1 <|> Left 2 <|> Left 3 <|> Right 4 <|> Right 5 <|> Left 6
    //let m = Nothing <|> Just 2 <|> Nothing <|> Nothing <|> Just 5 <|> Just 6
    //let es = asum [|Left 1; Left 2; Right 3; Right 4; Right 5; Left 6|]
    //let maybes = [|Nothing; Nothing; Just 3; Just 4; Just 5; Nothing|]
    //let ms  = asum maybes
    //let inline ( ^@ ) f x = f x
    //let inline ( ^. ) g f = f >> g
    //let ms' = mconcat (Array.map (Maybe.map Sum) maybes)
    //let ms'' = maybes |> Array.map (Maybe.map Sum) |> mconcat

module Program =

    [<EntryPoint>]
    let main _ =

        printfn "Starting...\n"

        //printfn "%A" (Util.x, Util.y, Util.z)
        //printfn "%A" (Util.e, Util.m)
        //printfn "%A\n%A\n%A" Util.es Util.ms Util.ms'
        
        let inline ( >>= ) m f = bind f m

        System.Threading.Tasks.Task.Run(fun () ->

            try

                let clear sleep =
                    if sleep then System.Threading.Thread.Sleep 1000
                    System.GC.Collect()
                    if sleep then System.Threading.Thread.Sleep 1000
                    System.GC.Collect()
                    System.GC.GetTotalMemory true |> ignore

                clear false
                clear true

                let loops = 50_000_000
                let time  = System.Diagnostics.Stopwatch()

                clear false
                clear true

                let mutable memory = System.GC.GetAllocatedBytesForCurrentThread () / 1024L / 1024L
                let countMem () =
                    let newmem = System.GC.GetAllocatedBytesForCurrentThread () / 1024L / 1024L
                    let dif = newmem - memory
                    memory <- newmem
                    dif

                let runTest order =

                    printfn "\n-- Beginning test #%i --\n" order

                    let runtest f =                    
                        clear false
                        ignore <| countMem ()
                        clear true
                        time.Restart()
                        let name, x = f ()
                        clear false
                        time.Stop()
                        printfn "%s:\tresult:  %i\ttime:  %i\tmem:  %i" name x time.ElapsedMilliseconds (countMem ())


                    let notInlined () =
                        "not inlined", //callCC' (fun exit ->
                        //    getCC' 0
                        //    >>= (fun struct (x, f) ->
                        //        callCC' (fun next ->
                        //            unit (x + 1)
                        //            >>= (fun y ->
                        //                if y >= loops
                        //                then exit y
                        //                else f y >>= next))))
                        //    |> evalCont
                            cont {
                                let! (x, f) = getCC 0
                                let! y      = unit (x + 1)
                                let! (z, g) = getCC y
                                if z % 2 = 0 then
                                    if y >= loops then
                                        return y
                                    else
                                        return! f z
                                else
                                    return! g (z + 1)
                            } |> evalCont


                    let yesInlined () =
                        "yes inlined", //callCC (fun exit ->
                            //getCC 0
                            //>>= (fun struct (x, f) ->
                            //    callCC (fun next ->
                            //        unit (x + 1)
                            //        >>= (fun y ->
                            //            if y >= loops
                            //            then exit y
                            //            else f y >>= next))))
                            //|> evalCont
                            cont {
                                let! (x, f) = getCC 0
                                let! y      = unit (x + 1)
                                let! (z, g) = getCC y
                                if z % 2 = 0 then
                                    if y >= loops then
                                        return y
                                    else
                                        return! f z
                                else
                                    return! g (z + 1)
                            } |> evalCont

                    //let call_inline () =
                    //    "call inlined", callCC (fun exit ->
                    //            getCC' 0
                    //            >>= (fun struct (x, f) ->
                    //                callCC (fun next ->
                    //                    unit (x + 1)
                    //                    >>= (fun y ->
                    //                        if y >= loops
                    //                        then exit y
                    //                        else f y >>= next))))
                    //        |> evalCont


                    match order % 2 with
                    | 1 -> runtest notInlined;   runtest yesInlined;
                    | 0 -> runtest yesInlined;   runtest notInlined;
                    | _ -> printfn "failed match on test order"


                for n = 1 to 6 do runTest n

            with e -> printfn "%A" e

        ).Wait()
        
        
        System.Console.WriteLine("\n\nDone. Press any key to quit.\n\n")
        if Util.inRelease then System.Console.ReadKey(true) |> ignore

        0