type IO<'T> = IO of Async<'T>
with
    static member inline ( <+> ) ((IO a), IO b) =
        IO (async {
            let! a = a
            let! b = b
            return a <+> b })

type XS = XS of int list
with
    static member ( <+> ) ((XS a), XS b) = XS (a @ b)

let make f = IO (async { return f () })
let run (IO a) = Async.RunSynchronously a

type IOb() =
    member _.Return x = IO (async.Return x)
    member _.ReturnFrom m : IO<_> = m
    member inline _.Bind((IO a), f) =
        IO (async {
            let! a = a
            let (IO b) = f a
            return! b })

let doIO = IOb()

let x =
    doIO {
        let xs = make (fun () -> XS [1..10])
        let ys = make (fun () -> XS [11..20])
        return! xs <+> ys
    } |> run



#load "Contexts\Maybe.fs"
open Rogz.Context.Base
open Rogz.Context.Base.Maybe

type Monad() =
    member _.Return x : Maybe<_> = Just x // Required for 'where'.
    member _.Yield x : Maybe<_> = Just x // Required for 'zip'
    //member _.YieldFrom m : Maybe<_> = m
    //member _.Zero() = Just ()
    //member inline _.Bind(m, f) = Maybe.bind f m
    member inline _.For(m, f) = Maybe.bind f m // Required for 'zip'. 'bind' required over 'map' unless the 'unit' was removed from 'Yield.'
    


    [<CustomOperation("map2", IsLikeZip=true)>]
    member inline _.Map2(m, n, f) = Maybe.map2 f m n
    
    [<CustomOperation("where", MaintainsVariableSpace=true)>]
    member inline _.Where(m, [<ProjectionParameter>] p) = Maybe.filter p m
    
    [<CustomOperation("join", AllowIntoPattern=true)>]
    member inline _.GroupJoin(xs: Maybe< ^a>, ys: Maybe< ^b>, [<ProjectionParameter>] p, [<ProjectionParameter>] f) = Maybe.join p f xs ys


let doMonad = Monad()

let xxx =
    doMonad {
        //for _ in Just () do
        map2 a in Just 1
        map2 b in Just 2
        where (b > a)
        map2 c in Just 3
        map2 d in Just 4
        join (Just 5) (fun it -> it > d) (fun e -> a, b, c, d, e) into (a, b, c, d, e)
        map2 f in Just 6
        map2 h in Just 7
        for aa in Just 1 do
        for bb in Just 2 do
        map2 cc in Just 3
        map2 dd in Just 4
        yield (a, b, c, d, e, f, h, aa, bb, cc, dd)
    }










#load "Contexts\Maybe.fs"
open Rogz.Context.Base
open Rogz.Context.Base.Maybe
#load "Contexts\Either.fs"
open Rogz.Context.Base.Either


//type Work = Work with
//    member _.ReturnFrom x : Maybe<_>    = x
//    member _.ReturnFrom x : Either<_,_> = x
//    member inline _.Bind(m: Maybe<_>,    f: _ -> Maybe<_>)    : Maybe<_>    = Maybe.bind f m    
//    member inline _.Bind(m: Either<_,_>, f: _ -> Either<_,_>) : Either<_,_> = Either.bind f m
//    member inline _.Bind(m: )

//let doWork = Work

//let justs : Maybe<int * int * int> =
//    doWork {
//        let! a = Just 1
//        let! b = Just 2
//        let! c = Just 3
//        return! Just (a, b, c)
//    }


//let rights =
//    doWork {
//        let! a = Right 1
//        let! b = Right 2
//        let! c = Right 3
//        return! Right (a, b, c)
//    }



////let r = ref 0
////let rs = ref []
////type ListBuilder () =
////    member _.Yield x = [x]
////    member inline _.For(m, f) = List.collect f m
////    member inline _.Combine(x, xs) = x::xs
////    [<CustomOperation("zip",IsLikeZip=true)>]
////    member inline _.Zip(xs: list<_>, ys:list<_>, f:(_ -> _ -> _)) =
////        rs := !r::!rs
////        r := 0
////        let rec go xs ys k =
////            incr r
////            match xs, ys with
////            | [], _ | _, []    -> k []
////            | (x::xs), (y::ys) -> go xs ys (fun zs -> k (f x y :: zs))
////        go xs ys id
////let doList = ListBuilder()


////type OptionBuilder() =
////    member _.Return x = Some x
////    member _.Yield x  = Some x
////    member _.ReturnFrom(m) : _ option = m
////    member _.Zero() = Some ()
////    member inline _.Bind(m, f) = Option.bind f m
////    member inline _.For(m, f)  = Option.bind f m
////    [<CustomOperation("join",MaintainsVariableSpaceUsingBind=true,AllowIntoPattern=true)>]//,IsLikeZip=true)>]
////    member inline _.Join(xs: option<_>, ys:option<_>, (*[<ProjectionParameter>]*) p, f) =
////        match xs, ys with
////        | None, _ | _, None -> None
////        | Some a, Some b    -> if p a b then Some (f a b) else None
////    [<CustomOperation("zip",IsLikeZip=true)>]
////    member inline _.Zip(a, b, f) = Option.map2 f a b
////let doOption = OptionBuilder()

////let opts_ref = ref 0
////let opts =
////    doOption {
////        incr opts_ref
////        let! a = Some 1
////        incr opts_ref
////        zip x in None
////        join (Some 2) (fun (_, a) b -> a <> b) (fun a b -> a, b) into c
////        incr opts_ref
////        let! b = Some 2
////        incr opts_ref
////        return c, b
////    }

////let opts_zip =
////    doOption {
////        zip a in Some 1
////        zip b in Some 2
////        zip c in Some 3
////        if a = b then return a, b
////        else return c, a
////    }


////let xs =
////    let xs =
////        doList {
////            for x in [1..3] do
////            zip y in [4..6]
////            //zip u in List.init 20 id
////            //for z in [7..9] do
////            yield x, y//, z
////        }
////    in xs
////    //let ys =
////    //    doList {
////    //        for x in [1..3] do
////    //        for y in [4..6] do
////    //        for z in [7..9] do
////    //        yield x, y, z
////    //}
////    //{|Zip = xs; For = ys|}



////type Select<'r, 'a> = Select of (('a -> 'r) -> 'a)
////let inline runSelect selection (Select m) = m selection


////let inline map (f: ^a -> ^b) (m: Select< ^r, ^a>) : Select< ^r, ^b> = Select (fun k -> runSelect (k << f) m |> f)

////let inline f x = float x + 1.
////let inline g x = string x
////let m = Select (fun k -> k 1)
////let inline k x = int x + 1

////let functor =
////    let c = m |> map g |> runSelect k
////    let a =
////        map f m |> map g |> runSelect k
////    let b = map (f >> g) m |> runSelect k
////    {| A = a; B = b; Same = a = b; C = c |}




////let inline from x = x


////let x =
////    maybe {
////        let! a = Just 1
////        select (a)
////        let! b = Just 2
////        select (a, b)
////    }

////let y =
////    maybe {
////        let  x = 5
////        let! a = Just x
////        where (x <> a)
////        return a
////    }



//////[<Interface>]
//////type ICastable<'To> =
//////    abstract member Cast: unit -> 'To

//////let inline cast<'To> : (ICastable<'To> -> 'To) =
//////    fun (from) -> from.Cast()


////// defining op_Explicit is doable for type coercions, but it must be done at the definition site of the type.
////type Maybex<'T> = Nothingx | Justx of 'T //with
    
////    // -- Below works (allows: int (Just 2.1) => 2), but not for all types (eg cant do: option<int> (Just 1) => Some 1)
////    // ^- and may be against the spirit of explicitness....?
////    //static member inline op_Explicit(m) = match m with Nothingx -> 0  | Justx x -> int x
////    //static member inline op_Explicit(m) = match m with Nothingx -> 0L | Justx x -> int64 x
////    //static member inline op_Explicit(m) = match m with Nothingx -> 0. | Justx x -> float x
    
////    // -- Below doesnt compile.
////    //interface ICastable<Option<'T>> with
////    //    override this.Cast() = match this with Nothingx -> None | Justx a -> Some a
////    //interface ICastable<System.Nullable<'T>> with
////    //    override this.Cast() = match this with Nothingx -> System.Nullable() | Justx a -> System.Nullable a


////[<AbstractClass; Sealed>]
////type Cast =
////    // -- Be judicious with inlining. These won't generally be called often and won't benenfit from it much anyway.
////    //[<AggressiveInlining>]
////    static member Fw(it: Maybe<_>) : Option<_> =
////        match it with Nothing -> None | Just a -> Some a
////    static member Bw(ot: Option<_>) : Maybe<_> =
////        match ot with None -> Nothing | Some a -> Just a

//////let idint =
//////    let x = Justx 1
//////    let y : Maybex<int> = Nothingx
//////    {| Int = int x, int y
//////     ; Int64 = int64 x, int64 y
//////     ; Float = float x, int64 y
//////     ; Option = cast<int option> x |}



////// fix :: (a -> a) -> a
////let inline fix (f: (^a -> ^b) -> ^a -> ^b) : (^a -> ^b) =
////    let rec go x = f go x in go

////// mfix :: (a -> m a) -> m a
////// fixM :: ((a -> m b) -> a -> m b) -> a -> m b
////let inline mfix (f: (^a -> Maybe< ^b>) -> ^a -> Maybe< ^b>) : (^a -> Maybe< ^b>) =
////    let rec go x = bind (f go) (Just x) in go

////let f k x =
////    if x >= 1_000_000 then Just (float x) else k (x + 1)
////let a = mfix f 1





////// #load "FolderName\FileName.fs"



////#load "Data\Cont.fs"
////#load "Contexts\Cont.fs"
////open Rogz.Context.Data.Cont
////open System.Linq
////let xxx =
////    [| for i = 1 to 10 do
////        do printfn "%i" i
////        yield Cont.unit i |]
////    |> Cont.sequence
////    |> Cont.map (fun x -> printfn "%i" (x.Count()))
////    |> Cont.evalCont


//////#load "Data\RWS.fs"
//////#load "Contexts\RWS.fs"
//////open Rogz.Context.Data.RWS
//////module R = Rogz.Context.Data.RWS.RWS

//////type T = T of int with
//////    static member Empty() = T 0
//////    static member Append((T a), T b) = T <| a + b

//////let inline f j k a =
//////    if a >= 100_000_000 then R.unit a
//////    elif a % 2 = 0 then j (a + 1)
//////    else k <| RWS (fun e s -> { RWSResult.State = s + 1; Log = T s; Value = a + e })
//////let a = 1
//////let rj = R.runRWS 1 1 <| R.fixM f (Rogz.Context.Data.Either.Left a)
//////let rk = R.runRWS 1 1 <| R.fixM f (Rogz.Context.Data.Either.Right (RWS (fun _ _ -> { RWSResult.State = 10; Log = T -999; Value = a })))

//////let j1 = Just 1
//////let j2 = Just 2
//////let jadd = Just ((+) 1)
//////let n = M.empty<int>
//////let even n = n % 2 = 0

//////let ``j1_map_string: Just "1"`` = j1 |%> string
//////let ``n_map_string: Nothing`` = n |%> string

//////let ``j1_replaceby_3: Just 3`` = j1 %> 3

//////let ``j2_from_j1: Just 2`` = j1 *> j2
//////let ``j1_over_n: Just 1`` = n <|> j1
//////let ``j1_plus1_ap: Just 2`` = jadd <*> j1

//////let ``j1_filtered_to_nothing: Nothing`` = j1 ?> even
//////let ``j2_unchanged_by_filter: Just 2`` = j2 ?> even

//////let ``j1_bind_string: Just "1"`` = j1 >>= (string >> M.unit)