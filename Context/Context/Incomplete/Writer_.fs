namespace PTR.Context.Type//.Writer


//type Writer<'Log, 'Value>(log: 'Log, value: 'Value) =

//    member inline _.Log = log
//    member inline _.Value = value

//    override _.ToString() = sprintf "Writer { Log = %A ; Value = %A }" log value

//    override s.Equals(other: obj) =
//        match other with
//        | null -> false
//        | s' when obj.ReferenceEquals(s, s') -> true
//        | :? Writer<'Log, 'Value> as o -> (box s.Log).Equals(o.Log) && (box s.Value).Equals(o.Value)
//        | _ -> false

//    override _.GetHashCode() = (box (struct (log, value))).GetHashCode()


//    member inline _.With(onLog: System.Func< ^Log, ^NewLog>) =
//        Writer(log = onLog.Invoke(log), value = value)

//    member inline _.With(onValue: System.Func< ^Value, ^NewValue>) =
//        Writer(value = onValue.Invoke(value), log = log)

//    member inline _.With(onLog: System.Func< ^Log, ^NewLog>, onValue: System.Func< ^Value, ^NewValue>) =
//        Writer(log = onLog.Invoke(log), value = onValue.Invoke(value))

//    member inline _.Apply(f: System.Func< ^Log, ^Value, ^Result>) = f.Invoke(log, value)


//    static member inline Unit(x: ^a) : Writer< ^w, ^a> =
//        Writer(value = x, log = (^w : (static member Empty: unit -> ^w) ()))

//    member inline _.Map(f: System.Func< ^Value, ^NewValue>) =
//        Writer(log = log, value = f.Invoke(value))

//    member inline s.Extend(f: System.Func<Writer< ^Log, ^Value>, ^NextValue>) =
//        Writer(log = log, value = f.Invoke(s))

//    member inline _.Extract = value

//    static member inline Append(first: #Writer< ^w, ^a>, second: #Writer< ^w, ^a>) =
//        Writer(log = (^w : (static member Append: ^w -> ^w -> ^w) (first.Log, second.Log))
//              ,value = (^a : (static member Append: ^a -> ^a -> ^a) (first.Value, second.Value)))


//type WriterAppend<'Log, 'Value
//        when 'Log : (static member Append: 'Log -> 'Log -> 'Log)>
//    (log: 'Log, value: 'Value) =
//    inherit Writer<'Log, 'Value>(log = log, value = value)

//    member inline _.With(onLog: System.Func< ^Log, ^NewLog>) =
//        WriterAppend(log = onLog.Invoke(log), value = value)

//    member inline _.With(onValue: System.Func< ^Value, ^NewValue>) =
//        WriterAppend(value = onValue.Invoke(value), log = log)

//    member inline _.With(onLog: System.Func< ^Log, ^NewLog>, onValue: System.Func< ^Value, ^NewValue>) =
//        WriterAppend(log = onLog.Invoke(log), value = onValue.Invoke(value))

//    //member inline _.Apply(f: System.Func< ^Log, ^Value, ^Result>) = f.Invoke(log, value)
    

//    static member inline Unit(x: ^a) : WriterAppend< ^w, ^a> =
//        WriterAppend(value = x, log = (^w : (static member Empty: unit -> ^w) ()))

//    member inline _.Bind(f: System.Func< ^Value, #Writer< ^Log, ^NewValue>>) =
//        let w = f.Invoke(value)
//        WriterAppend
//            (log = (^Log : (static member Append: ^Log -> ^Log -> ^Log) (log, w.Log))
//            ,value = w.Value)

//    member inline _.Map2(second: #Writer< ^Log, ^NextValue>, f: System.Func< ^Value, ^NextValue, ^FinalValue>) =
//        WriterAppend(log = (^Log : (static member Append: ^Log -> ^Log -> ^Log) (log, second.Log))
//                    ,value = f.Invoke(value, second.Value))

//    member inline _.Map(f: System.Func< ^Value, ^NewValue>) =
//        WriterAppend(log = log, value = f.Invoke(value))

//    member inline s.Extend(f: System.Func<Writer< ^Log, ^Value>, ^NextValue>) =
//        WriterAppend(log = log, value = f.Invoke(s))

//    static member inline Append(first: #Writer< ^w, ^a>, second: #Writer< ^w, ^a>) =
//        WriterAppend(log = (^w : (static member Append: ^w -> ^w -> ^w) (first.Log, second.Log))
//                    ,value = (^a : (static member Append: ^a -> ^a -> ^a) (first.Value, second.Value)))


//type WriterMonoid<'Log, 'Value
//        when 'Log : (static member Empty: unit -> 'Log)
//        and  'Log : (static member Append: 'Log -> 'Log -> 'Log)>
//    (log: 'Log, value: 'Value) =
//    inherit WriterAppend<'Log, 'Value>(log = log, value = value)

//    member inline _.With(onLog: System.Func< ^Log, ^NewLog>) =
//        WriterMonoid(log = onLog.Invoke(log), value = value)

//    member inline _.With(onValue: System.Func< ^Value, ^NewValue>) =
//        WriterMonoid(value = onValue.Invoke(value), log = log)

//    member inline _.With(onLog: System.Func< ^Log, ^NewLog>, onValue: System.Func< ^Value, ^NewValue>) =
//        WriterMonoid(log = onLog.Invoke(log), value = onValue.Invoke(value))

//    //member inline _.Apply(f: System.Func< ^Log, ^Value, ^Result>) = f.Invoke(log, value)


//    static member inline Unit(x: ^a) : WriterMonoid< ^w, ^a> =
//        WriterMonoid(value = x, log = (^w : (static member Empty: unit -> ^w) ()))

//    member inline _.Bind(f: System.Func< ^Value, #Writer< ^Log, ^NewValue>>) =
//        let w = f.Invoke(value)
//        WriterMonoid
//            (log = (^Log : (static member Append: ^Log -> ^Log -> ^Log) (log, w.Log))
//            ,value = w.Value)

//    member inline _.Map2(second: #Writer< ^Log, ^NextValue>, f: System.Func< ^Value, ^NextValue, ^FinalValue>) =
//        WriterMonoid(log = (^Log : (static member Append: ^Log -> ^Log -> ^Log) (log, second.Log))
//                    ,value = f.Invoke(value, second.Value))

//    member inline _.Map(f: System.Func< ^Value, ^NewValue>) =
//        WriterMonoid(log = log, value = f.Invoke(value))


//    member inline s.Extend(f: System.Func<Writer< ^Log, ^Value>, ^NextValue>) =
//        WriterMonoid(log = log, value = f.Invoke(s))

//    static member inline Append(first: #Writer< ^w, ^a>, second: #Writer< ^w, ^a>) =
//        WriterMonoid(log = (^w : (static member Append: ^w -> ^w -> ^w) (first.Log, second.Log))
//                    ,value = (^a : (static member Append: ^a -> ^a -> ^a) (first.Value, second.Value)))


//module Writer =

//    [<Struct>]
//    type T = T of string with
//        static member Append ((T a), T b) = T (a + b)
//        static member Empty () = T ""

//    let a = Writer(log = 1, value = 2)
//    let b = a.Map(fun a -> a)
//    let a_b = a = b
//    let a_extract = a.Extract
//    let a_extend = a.Extend(fun w -> w.Log, w.Value)
//    let c = Writer((T "1", 3))
//    let c' = WriterAppend((T "1"), 3)
//    let c'_extract_3 = c'.Extract
//    let c'' = WriterAppend((T "1"), 4)
//    let c_c'_true = c = (c' :> Writer<_,_>)
//    let c_c''_false = c = (c'' :> Writer<_,_>)
//    let d = c'.Map2(c, fun a b -> string (a + b))
//    let e = WriterMonoid((T "2"), 5).Map2((WriterMonoid<T, T>.Unit(T "10")), fun a b -> a, b)
//    let e_extend = e.Extend(fun w -> w.Log, w.Value)
//    let e_dupl = e.Extend(fun a -> a)
//    let e_map_viacomonad = e.Extend(fun a -> a).Map(fun a -> a.Extract)
//    let e_e_viadup = e = e_map_viacomonad
    
//    let inline private empty' () = (^w : (static member Empty: unit -> ^w) ())
//    let inline private append' a b = (^w : (static member Append: ^w -> ^w -> ^w) (a, b))


//    module Pattern =
  
//        let inline ( |Writer| ) (writer: #Writer< ^w, ^v>) = Writer (struct (writer.Log, writer.Value))
  
//        let inline ( |WriterLog| ) (w: Writer< ^w, ^v>) = WriterLog w.Log
  
//        let inline ( |WriterValue| ) (w: Writer< ^w, ^v>) = WriterValue w.Value


//// Primitives

//    [<CompiledName("RunWriter")>]
//    let inline runWriter f (w: Writer< ^w, ^a>) : ^b = f w.Log w.Value

//    [<CompiledName("Tell")>]
//    let tell (entry: 'w) : Writer< ^w, unit> = { Writer.Log = entry ; Value = () }

//    [<CompiledName("Listen")>]
//    let listen (w: Writer<'w, 'v>) : Writer< ^w, (^w * ^v)> =
//        { Writer.Log = w.Log ; Value = (w.Log, w.Value) }

//    [<CompiledName("Listens")>]
//    let inline listens f (w: Writer< ^w, ^a>) : Writer< ^w, (^a * ^b)> =
//        { Writer.Log = w.Log ; Value = (w.Value, f w.Log) }


//// Isomorphisms

//    [<CompiledName("Write")>]
//    let write (log: 'w) (value: 'a) : Writer< ^w, ^a> = { Writer.Log = log ; Value = value }

//    [<CompiledName("ToPair")>]
//    let toPair { Writer.Log = l: 'w ; Value = v: 'a } : ^w * ^a = l, v

//    [<CompiledName("ToPair1")>]
//    let toPair1 { Writer.Log = l: 'w ; Value = v: 'a } : struct (^w * ^a) = struct (l, v)


//// Monad

//      get rid of unit  
//    let inline unit (x: ^a) : Writer< ^w, ^a> when ^w : (static member Empty: unit -> ^w) =
//        { Writer.Log = empty' () ; Value = x }

//    [<CompiledName("Bind")>]
//    let inline bind (f: ^a -> Writer< ^w, ^b>) (m: Writer< ^w, ^a>) : Writer< ^w, ^b>
//        when ^w : (static member Append: ^w -> ^w -> ^w) =
//        let w = f m.Value in { w with Writer.Log = append' m.Log w.Log }

//    [<CompiledName("Flatten")>]
//    let inline flatten (mm: Writer< ^w, Writer< ^w, ^a>>) : Writer< ^w, ^a>
//        when ^w : (static member Append: ^w -> ^w -> ^w) =
//        { Writer.Log = append' mm.Log mm.Value.Log
//        ; Value = mm.Value.Value }

//    [<CompiledName("RecM")>]
//    let inline recM f (x: ^a) : Writer< ^w, ^b>
//        when ^w : (static member Append: ^w -> ^w -> ^w)
//        and  ^w : (static member Empty : unit -> ^w) =                
//        let mt : ^w = empty' ()
//        let mutable mt' = mt
//        let unit' a = mt' <- append' mt' mt ; { Writer.Log = mt' ; Value = a }
//        let bind' k m = k m.Value
//        let rec go m = bind' j m
//        and k a = go (unit' a)
//        and j a = f k a
//        j x

//    [<CompiledName("RecM1")>]
//    let inline recM1 f (x: ^a) : Writer< ^w, ^b>
//        when ^w : (static member Append: ^w -> ^w -> ^w)
//        and  ^w : (static member Empty: unit -> ^w) =
//        let mutable l : ^w = empty' ()
//        let bind' k (m: Writer< ^w, ^a>) = l <- append' l m.Log ; k m.Value
//        let rec go m = bind' k m
//        and k a = f go a
//        { k x with Writer.Log = l }

//    //[<CompiledName("FoldrM")>]
//    //let inline foldrM (f: ^a -> ^s -> Writer< ^w, ^s>) (s0: ^s) (source: ^a seq) : Writer< ^w, ^s> =
//    //    //let g k x s = bind k (f x s)
//    //    //match source with
//    //    //| :? array< ^a> as s -> Array.fold g wrap s s0
//    //    //| :? list<  ^a> as s -> List.fold  g wrap s s0
//    //    //| _ -> Seq.fold g wrap source s0
//    //    let mutable lg = empty' ()
//    //    let mutable st = s0
//    //    let act a () =
//    //        let w = f a st
//    //        lg <- append' lg w.Log
//    //        st <- w.Value
//    //    match source with
//    //    | :? array< ^a> as s -> Array.foldBack act s ()
//    //    | :? list<  ^a> as s -> List.foldBack act s ()
//    //    | _ -> Seq.foldBack act source ()
//    //    { Writer.Log = lg ; Value = st }
//    //
//    //[<CompiledName("FoldlM")>]
//    //let inline foldlM (f: ^s -> ^a -> Writer< ^w, ^s>) (s0: ^s) (source: ^a seq) : Writer< ^w, ^s> =
//    //    //let g x k s = bind k (f s x)
//    //    //match source with
//    //    //| :? array< ^a> as s -> Array.foldBack g s wrap s0
//    //    //| :? list<  ^a> as s -> List.foldBack  g s wrap s0
//    //    //| _ -> Seq.foldBack g source wrap s0
//    //    let mutable lg = empty' ()
//    //    let mutable st = s0
//    //    for a in source do
//    //        let w = f st a
//    //        lg <- append' lg w.Log
//    //        st <- w.Value
//    //    { Writer.Log = lg ; Value = st }


//    module Workflow =

//        type WriterBuilder () =
//            member inline _.Bind(m: Writer< ^w, ^a>, f) : Writer< ^w, ^b> when ^w : (static member Append: ^w -> ^w -> ^w) = bind f m
//            member inline _.Return x : Writer< ^w, ^a> when ^w : (static member Empty: unit -> ^w) = unit x
//            member inline _.ReturnFrom m : Writer< ^w, ^a> = m
//            member inline _.Zero() : Writer< ^w, unit> when ^w : (static member Empty: unit -> ^w) = unit ()
 
//            member inline _.TryWith(body, handler) : Writer< ^w, ^a> = try body with e -> handler e
//            member inline _.TryFinally(body, finalizer) : Writer< ^w, ^a> = try body finally finalizer ()
 
//            member inline _.Using(disp: ^d, body) : Writer< ^w, ^a> when ^d :> System.IDisposable =
//                using disp body

//            member inline _.While(guard, body) : Writer< ^w, unit> =                    
//                let rec go = function
//                | false -> unit ()
//                | true  -> bind k (body ())
//                and k () = go (guard ()) in k ()

//            member inline _.For(seq: #seq< ^a>, body) : Writer< ^w, unit> =
//                use e = seq.GetEnumerator()
//                let rec go = function
//                | false -> unit ()
//                | true  -> b e.Current
//                and b x = bind k (body x)
//                and k () = go (e.MoveNext()) in k ()


//    let writer = Workflow.WriterBuilder ()


//// Applicative

//    [<CompiledName("Ap")>]
//    let inline ap (fv: Writer< ^w, ^a>) (ff: Writer< ^w, ^a -> ^b>) : Writer< ^w, ^b>
//        when ^w : (static member Append: ^w -> ^w -> ^w) =
//        { Writer.Log = append' ff.Log fv.Log
//        ; Value = ff.Value fv.Value }

//    [<CompiledName("Map2")>]
//    let inline map2 f (fa: Writer< ^w, ^a>) (fb: Writer< ^w, ^b>) : Writer< ^w, ^c> =
//        { Writer.Log = append' fa.Log fb.Log
//        ; Value = f fa.Value fb.Value }

//    //[<CompiledName("Map3")>]
//    //let inline map3 f (fa: Writer< ^w, ^a>) (fb: Writer< ^w, ^b>) (fc: Writer< ^w, ^c>) : Writer< ^w, ^d> =
//    //    { Writer.Log = append' fa.Log (append' fb.Log fc.Log)
//    //    ; Value = f fa.Value fb.Value fc.Value }

//    [<CompiledName("AndThen")>]
//    let inline andThen fb (fa: Writer< ^w, ^a>) : Writer< ^w, ^b> =
//        { fb with Log = append' fa.Log fb.Log }

//    [<CompiledName("When")>]
//    let inline when_ condition f : Writer< ^w, unit> =
//        if condition then f () else unit ()

//    //[<CompiledName("FilterA")>]
//    //let inline filterA (p: ^a -> Writer< ^w, bool>) (source: ^a seq) =
//    //    let cons x b xs = if b then x::xs else xs
//    //    let g x xs = map2 (cons x) (p x) xs
//    //    let z = wrap []
//    //    match source with
//    //    | :? array< ^a> as s -> Array.foldBack g s z
//    //    | :? list<  ^a> as s -> List.foldBack  g s z
//    //    | _ -> Seq.foldBack g source z
//    //
//    //[<CompiledName("ZipWithA")>]
//    //let inline zipWithA f (source1: #seq< ^a>) (source2: #seq< ^b>) : Writer< ^w, ^c list> =
//    //    sequenceA (System.Linq.Enumerable.Zip(source1, source2, System.Func<_,_,_>f))
//    //
//    //[<CompiledName("ReplicateA")>]
//    //let inline replicateA (count: int) (fa: Writer< ^w, ^a>) : Writer< ^w, ^a seq> =
//    //    let mutable w = empty' ()
//    //    let xs = ResizeArray< ^a>(count)
//    //    for i = 0 to count - 1 do
//    //        w <- append' fa.Log w
//    //        xs.Add(fa.Value)
//    //    { Writer.Log = w ; Value = xs :> _ seq }


//// Functor

//    [<CompiledName("Map")>]
//    let inline map f (fa: Writer< ^w, ^a>) : Writer< ^w, ^b> =
//        { Writer.Log = fa.Log ; Value = f fa.Value }


//// Bifunctor

//    [<CompiledName("Bimap")>]
//    let inline bimap (f: ^w1 -> ^w2) (g: ^a -> ^b) (bf: Writer< ^w1, ^a>) : Writer< ^w2, ^b> =
//        { Writer.Log = f bf.Log ; Value = g bf.Value }

//    [<CompiledName("MapFst")>]
//    let inline mapFst (f: ^w1 -> ^w2) (bf: Writer< ^w1, ^a>) : Writer< ^w2, ^a> =
//        { Writer.Log = f bf.Log ; Value = bf.Value }

//    [<CompiledName("MapSnd")>]
//    let inline mapSnd (g: ^a -> ^b) (bf: Writer< ^w, ^a>) : Writer< ^w, ^b> =
//        { Writer.Log = bf.Log ; Value = g bf.Value }


//// Comonad

//    [<CompiledName("Extract")>]
//    let extract { Writer.Value = a: 'a } : ^a = a

//    [<CompiledName("Extend")>]
//    let inline extend j (w: Writer< ^w, ^a>) : Writer< ^w, ^b> =
//        { Writer.Log = w.Log ; Value = j w }    

//    [<CompiledName("Duplicate")>]
//    let duplicate (w: Writer<'w, 'a>) : Writer< ^w, Writer< ^w, ^a>> =
//        { Writer.Log = w.Log ; Value = w }

//    [<CompiledName("RecW")>]
//    let inline recW f (w: Writer< ^w, ^a>) =
//        let rec go w = f j w
//        and k w = f extract w
//        and j w = go (extend k w)
//        j w


//// Semigroup

//      get rid of append  
//    let inline append (e1: Writer< ^w, ^a>) (e2: Writer< ^w, ^a>) : Writer< ^w, ^a> =
//        { Writer.Log = append' e1.Log e2.Log
//        ; Value = append' e1.Value e2.Value }


//// Foldable

//    [<CompiledName("Fold")>]
//    let inline fold folder (seed: ^s) (source: Writer< ^w, ^a>) : ^s = folder seed source.Value

//    [<CompiledName("FoldBack")>]
//    let inline foldBack folder (seed: ^s) (source: Writer< ^w, ^a>) : ^s = folder source.Value seed

//    [<CompiledName("Foldl")>]
//    let inline foldl folder (seed: unit -> ^s) (source: Writer< ^w, ^a>) : ^s = folder seed source.Value
    
//    [<CompiledName("Foldr")>]
//    let inline foldr folder (seed: unit -> ^s) (source: Writer< ^w, ^a>) : ^s = folder source.Value seed

//    [<CompiledName("Foldm")>]
//    let inline foldm f (source: Writer< ^w, ^a>)
//        : ^m when ^m : (static member Append: ^m -> ^m -> ^m) = f source.Value

//    [<CompiledName("MapFold")>]
//    let inline mapFold mapping (seed: ^s) (source: Writer< ^w, ^a>) : Writer< ^w, ^b> * ^s =
//        let r, s = mapping seed source.Value
//        { Writer.Log = source.Log ; Value = r }, s

//    [<CompiledName("MapFoldBack")>]
//    let inline mapFoldBack mapping (seed: ^s) (source: Writer< ^w, ^a>) : Writer< ^w, ^b> * ^s =
//        let r, s = mapping source.Value seed
//        { Writer.Log = source.Log ; Value = r }, s


//// Bifoldable

//    [<CompiledName("Bifold")>]
//    let inline bifold (fold1: ^s -> ^a -> ^s) (fold2: ^s -> ^b -> ^s) (seed: ^s) (source: Writer< ^a, ^b>) : ^s =
//        fold2 (fold1 seed source.Log) source.Value

//    [<CompiledName("BifoldBack")>]
//    let inline bifoldBack (fold1: ^a -> ^s -> ^s) (fold2: ^b -> ^s -> ^s) (seed: ^s) (source: Writer< ^a, ^b>) : ^s =
//        fold2 source.Value (fold1 source.Log seed)

//    [<CompiledName("Bifoldl")>]
//    let inline bifoldl (fold1: (unit -> ^s) -> ^a -> ^s) (fold2: (unit -> ^s) -> ^b -> ^s) (seed: unit -> ^s) (source: Writer< ^a, ^b>) : ^s =
//        fold2 (fun () -> fold1 seed source.Log) source.Value

//    [<CompiledName("Bifoldr")>]
//    let inline bifoldr (fold1: ^a -> (unit -> ^s) -> ^s) (fold2: ^b -> (unit -> ^s) -> ^s) (seed: unit -> ^s) (source: Writer< ^a, ^b>) : ^s =
//        fold2 source.Value (fun () -> fold1 source.Log seed)

//    [<CompiledName("Bifoldm")>]
//    let inline bifoldm (f1: ^a -> ^m) (f2: ^b -> ^m) (source: Writer< ^a, ^b>) =
//        (^m : (static member Append: ^m -> ^m -> ^m) (f1 source.Log, f2 source.Value))

//    [<CompiledName("BimapFold")>]
//    let inline bimapFold (mapping1: ^s -> ^a -> ^b * ^s) (mapping2: ^s -> ^c -> ^d * ^s) (seed: ^s) (source: Writer< ^a, ^c>) : Writer< ^b, ^d> * ^s =
//        let w, s = mapping1 seed source.Log
//        let v, s = mapping2 s source.Value
//        { Writer.Log = w ; Value = v }, s

//    [<CompiledName("BimapFoldBack")>]
//    let inline bimapFoldBack (mapping1: ^a -> ^s -> ^b * ^s) (mapping2: ^c -> ^s -> ^d * ^s) (seed: ^s) (source: Writer< ^a, ^c>) : Writer< ^b, ^d> * ^s =
//        let w, s = mapping1 source.Log seed
//        let v, s = mapping2 source.Value s
//        { Writer.Log = w ; Value = v }, s


//// Traversable

//    [<CompiledName("Sequence")>]
//    let inline sequence (source: #seq<Writer< ^w, ^a>>) : Writer< ^w, ^a seq> =
//        let mutable w : ^w = empty' ()
//        let ra = ResizeArray<_>()
//        for x in source do
//            w <- append' w x.Log
//            ra.Add(x.Value)
//        { Writer.Log = w ; Value = System.Linq.Enumerable.AsEnumerable(ra) }

//    [<CompiledName("Traverse")>]
//    let inline traverse (f: ^a -> Writer< ^w, ^b>) (source: #seq< ^a>) : Writer< ^w, ^b seq> =
//        sequence (System.Linq.Enumerable.Select(source, f))