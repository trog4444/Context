namespace PTR.Context.Type.Incomplete.Stream2

#if COMPILED
type Stream<'T> = ((^T -> bool) -> unit)
[<AutoOpen>]
module StreamPrimitives =
    val inline ( |Stream| ) : ^T Stream -> ^T Stream
    val inline Stream : ((^T -> bool) -> unit) -> ^T Stream
#else
[<Struct; NoComparison; NoEquality>]
type Stream<'T> = Stream of ((^T -> bool) -> unit)
#endif


module Stream =

    [<CompiledName("OfArray")>]
    val inline ofArray : source: ^a [] -> ^a Stream

    [<CompiledName("OfList")>]
    val inline ofList : source: ^a list -> ^a Stream

    [<CompiledName("OfSeq")>]
    val inline ofSeq : source: ^a seq -> ^a Stream

    [<CompiledName("Map")>]
    val inline map : mapping: (^a -> ^b) -> ^a Stream -> ^b Stream

    [<CompiledName("Collect")>]
    val inline collect : projection: (^a -> ^b Stream) -> ^a Stream -> ^b Stream

    [<CompiledName("CollectMany")>]
    val inline collectMany : projection: (^a -> ^b Stream) -> mapping: (^a -> ^b -> ^c) -> ^a Stream -> ^c Stream

    [<CompiledName("Filter")>]
    val inline filter : predicate: (^a -> bool) -> ^a Stream -> ^a Stream

    [<CompiledName("Choose")>]
    val inline choose : chooser: (^a -> ^b PTR.Context.Type.Maybe.Maybe) -> ^a Stream -> ^b Stream

    [<CompiledName("Fold")>]
    val inline fold : folder: (^s -> ^a -> ^s) -> seed: ^s -> ^a Stream -> ^s

    [<CompiledName("FoldWhile")>]
    val inline foldWhile : folder: (^s -> ^a -> ^s * bool) -> seed: ^s -> ^a Stream -> ^s

    [<CompiledName("Take")>]
    val inline take : count: int -> ^a Stream -> ^a Stream

    [<CompiledName("TakeWhile")>]
    val inline takeWhile : predicate: (^a -> bool) -> ^a Stream -> ^a Stream

    [<CompiledName("CacheStream")>]
    val inline cacheStream : ^a Stream -> ^a Stream when ^a : equality