namespace Ptr.Context.Type.Lens

//
// Based on the Haskell lens package by Edward Kmett.
//
// https://www.youtube.com/watch?v=XVmhK8WbRLY&list=PLFDB7DEC7F7F53DFD&index=2
//
//  Yep. Like the monad laws, these are expectations you should have about lenses. Lenses that violate them are weird. Here they are
//
//    Get-Put: If you modify something by changing its subpart to exactly what it was before... then nothing happens
//    Put-Get: If you modify something by inserting a particular subpart and then viewing the result... you'll get back exactly that subpart
//    Put-Put: If you modify something by inserting a particular subpart a, and then modify it again inserting a different subpart b... it's exactly as if you only did the second step.



/// Acts like a property of an object that can be accessed through a `getter` and/or `setter`.
[<Struct; NoComparison; NoEquality>]
type Lens<'a, 'b> = { Get: (^a -> ^b) ; Set: (^a -> ^b -> ^a) } // change Set to a -> b -> a??


/// Active patterns on `Lens` values.
module Pattern =

    /// Active pattern that can be used to extract both the "getter" and "setter" within a 'Lens'.
    let inline ( |Lens| ) (lens: Lens< ^a, ^b>) = Lens lens

    /// Active pattern that can be used to extract the "getter" within a 'Lens'.
    let inline ( |LensGet| ) (lens: Lens< ^a, ^b>) = LensGet lens.Get

    /// Active pattern that can be used to extract the "setter" within a 'Lens'.
    let inline ( |LensSet| ) (lens: Lens< ^a, ^b>) = LensSet lens.Set    


/// Functions on Lenses to other types.
module Lenses =

    /// Abstractions to expand on Lens' expressiveness.
    module Abstraction =

        /// Abstraction for any 2-arity product-type.
        [<Interface>]
        type IPair<'a, 'b> =
            abstract member Fst: unit -> ^a
            abstract member Snd: unit -> ^b

        /// Convert two items into a Pair.
        let inline toPair a b =
            { new IPair< ^a, ^b> with
                override s.Fst () = a
                override s.Snd () = b }

        /// Convert two items into a Pair.
        let inline toPair' a b =
            { new IPair< ^a, ^b> with
                override s.Fst () = a ()
                override s.Snd () = b () }

        /// Convert two items into a Pair.
        let inline ofTuple (a, b) =
            { new IPair< ^a, ^b> with
                override s.Fst () = a
                override s.Snd () = b }


    /// Stateful `Lens` functions.
    module StateL =

        /// 'Stateful' version of setL -- when the 'state' value is left out,
        /// the function takes the form of 'state -> state * content', which
        /// can be put inside the 'State' type for stateful computations, where it
        /// behaves like the 'modify' function.
        let inline modifyL (lens: Lens< ^s, ^a>) property =
            fun state -> struct (lens.Set state property, ())

        /// Use a 'Lens' as a stateful function.
        let inline putL (lens: Lens< ^s, ^p>) =
            fun state -> struct (state, lens.Get state)

        /// Convert a stateful function of one state-type to another state-type.
        let inline focus f (lens: Lens< ^p, ^s>) =
            fun p -> match f (lens.Get p) with struct (s, a) -> struct (lens.Set p s, a)


    /// Functions between Lenses and collections.
    module CollectionL =

        /// Functions between Lenses and Maps.
        module MapL =

            /// 'GetL' attempts to retrieve a value from a Map given a key.
            ///
            /// 'SetL' attempts to add a value to a Map.
            let inline memberL key : Lens<Map< ^k, ^v>, ^v option> =
                { Get = Map.tryFind key
                ; Set = fun m -> (function None -> Map.remove key m | Some v -> Map.add key v m) }


/// Standard operations on `Lens` values.
module Std =

    /// Create a 'Lens' given an accessor function and a setter function.
    let inline newLens getter setter = { Get = getter; Set = setter }
    
    /// Run a `Lens` by applying a `getter` to the given value,
    /// then apply both to the `setter` to retrieve a final value.
    let inline runLens obj (lens: Lens< ^a, ^b>) = lens.Set obj (lens.Get obj)

    /// Retrieve the "getter" from a 'Lens'.
    let inline lensGet (lens: Lens< ^a, ^b>) = lens.Get

    /// Retrieve the "setter" from a 'Lens'.
    let inline lensSet (lens: Lens< ^a, ^b>) = lens.Set

    /// Retrieve a value with a 'getter', apply a function to it,
    /// then apply a 'setter' to the result and the initial value.
    let inline modLens f value (lens: Lens< ^a, ^b>) =
        lens.Set value (f (lens.Get value))

    /// Lens' equivalent of the standard 'fst' function on 2-arity product-types.
    let inline _1<'a, 'b> : Lens<Lenses.Abstraction.IPair< ^a, ^b>, ^a> =
        { Get = fun p -> p.Fst ()
        ; Set = fun p a -> { new Lenses.Abstraction.IPair< ^a, ^b> with
                                override s.Fst () = a
                                override s.Snd () = p.Snd () } }

    /// Lens' equivalent of the standard 'fst' function on 2-arity product-types.
    let inline _2<'a, 'b> : Lens<Lenses.Abstraction.IPair< ^a,  ^b>, ^b> =
        { Get = fun p -> p.Snd ()
        ; Set = fun p b -> { new Lenses.Abstraction.IPair< ^a, ^b> with
                                override s.Fst () = p.Fst ()
                                override s.Snd () = b } }

    /// Lens' equivalent of the standard 'fst' function on tuples.
    let inline fstL<'a, 'b> : Lens< ^a * ^b, ^a> =
        { Get = fst; Set = fun (_, b) a -> a, b }

    /// Lens' equivalent of the standard 'snd' function on tuples.
    let inline sndL<'a, 'b> : Lens< ^a * ^b, ^b> =
        { Get = snd; Set = fun (a, _) b -> a, b }    

    /// Caches both the 'getter' and 'setter' functions inside a Lens.
    let inline cacheLens (lens: Lens< ^a, ^b>) =
        let mg = System.Collections.Concurrent.ConcurrentDictionary< ^a, ^b>(HashIdentity.Structural)
        let ms = System.Collections.Concurrent.ConcurrentDictionary<(struct (^a * ^b)), ^a>(HashIdentity.Structural)
        let b0 = ref Unchecked.defaultof< ^b>
        let a0 = ref Unchecked.defaultof< ^a>
        { Get = fun a ->
            if mg.TryGetValue(a, b0) then !b0
            else mg.GetOrAdd(key = a, value = lens.Get a)
        ; Set = fun a b ->
            let p = struct (a, b)
            if ms.TryGetValue(p, a0) then !a0
            else ms.GetOrAdd(key = p, value = lens.Set a b) }


/// Compositional operations on `Lens` values.
module Composition =

    /// Lift a value onto an effectful context.
    let inline wrap x : Lens< ^a, ^b> =
        { Get = fun _   -> x
        ; Set = fun y _ -> y }

    /// Sequentially compose two effects, passing any value produced by the first
    /// as an argument to the second.
    let inline bind (f: ^b -> Lens< ^a, ^c>) (lens: Lens< ^a, ^b>) : Lens< ^a, ^c> =
        { Get = fun a   -> (f (lens.Get a)).Get a
        ; Set = fun a c -> lens.Set a (lens.Get ((f (lens.Get a)).Set a c)) }

    /// Removes one layer of monadic context from a nested monad.
    let inline flatten mm : Lens< ^a, ^b> = bind id mm

    /// Lift a function onto effects.
    let inline map (f: ^b -> ^c) (lens: Lens< ^a, ^b>) : Lens< ^a, ^c> =
        { Get = lens.Get >> f
        ; Set = fun a _ -> lens.Set a (lens.Get a) }

    /// Sequential application on effects.
    let inline ap mv (mf: Lens< ^a, ^b -> ^c>) : Lens< ^a, ^c> =
        bind (fun f -> map f mv) mf


    /// Supplementary Monad operations on the given type.
    module Monad =

        /// Monadic computation builder specialised to the given monad.
        type LensBuilder () =
            member inline s.Bind(m, k) = bind k m
            member inline s.Return x = wrap x
            member inline s.ReturnFrom m : Lens< ^a, ^b> = m
            member inline s.Zero () = s.Return ()

            member inline s.Delay f = f ()
            member inline s.Run f = f

            member inline s.TryWith (body, handler) = try s.ReturnFrom(body ()) with e -> handler e
            member inline s.TryFinally (body, finalizer) = try s.ReturnFrom(body ()) finally finalizer ()

            member inline s.Using(disp: #System.IDisposable, body) =
                s.TryFinally((fun () -> body disp),
                    fun () -> match box disp with null -> () | _ -> disp.Dispose ())

            member inline s.While(guard, body) =
                let rec loop = function
                | false -> s.Zero ()
                | true -> s.Bind(body (), guard >> loop)
                loop (guard ())

            member inline s.For(seq: _ seq, body) =
                s.Using(seq.GetEnumerator(),
                    fun enum -> s.While(enum.MoveNext,
                                    s.Delay(fun () -> body enum.Current)))


        /// Composes two monadic functions together.
        /// Acts as the composition function in the Kleisli category.
        let inline composeM k2 k1 = k1 >> bind k2
  
        /// Sequentially compose three actions, passing any value produced by the first
        /// two as arguments to the third.
        let inline bind2 k ma mb =
            bind (fun a -> bind (k a) mb) ma

        /// Sequentially compose four actions, passing any value produced by the
        /// first two as arguments to the third.
        let inline bind3 k ma mb mc =
            bind2 (fun a b -> bind (k a b) mc) ma mb

        /// Sequentially compose two actions, creating a third from the result and
        /// lifting a binary function on its effects.
        let inline bindMap k f m =
            bind (fun a -> map (f a) (k a)) m

        /// Build a monad through recursive (effectful) computations.
        /// Computation proceeds through the use of a continuation function applied to the intermediate result.
        /// The default monadic 'identity' function is used in each iteration where the continuation is applied.
        let inline recM f x =
            let rec go m = bind (f (wrap >> go)) m in go (f wrap x)

        /// Build a monad through recursive (effectful) computations.
        /// Computation proceeds through the use of a continuation function applied to an 'effect' applied over the intermediate result.
        /// Any constructor can be used in each iteration, in the case of union-types.
        let inline recMp f x =
            let rec go m = bind (f go) m in go (f id x)                  

        /// <summary>Monadic fold over a structure associating to the right.</summary>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        let inline foldrM f (s0: ^s) (source: ^a seq) : Lens< ^b, ^s> =
            let g k x s = bind k (f x s)
            Seq.fold g wrap source s0

        /// <summary>Monadic fold over a structure associating to the left.</summary>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        let inline foldlM f (s0: ^s) (source: ^a seq) : Lens< ^b, ^s> =
            let g x k s = bind k (f s x)
            Seq.foldBack g source wrap s0        


        /// Monadic zipping (combining or decomposing corresponding monadic elements).
        module Zip =
            
            /// Combine the corresponding contents of two monads into a single monad.
            let inline mzipWith f (mb: Lens< ^a, ^b>) (mc: Lens< ^a, ^c>) : Lens< ^a, ^d> =
                { Get = fun a   -> f (mb.Get a) (mc.Get a)
                ; Set = fun a _ -> mc.Set (mb.Set a (mb.Get a)) (mc.Get a) }

            /// Merge the contents (of corresponding pairs) of two monads into a monad of pairs.
            let inline mzip (mb: Lens< ^a, ^b>) (mc: Lens< ^a, ^c>) : Lens< ^a, ^b * ^c> =
                { Get = fun a -> mb.Get a, mc.Get a
                ; Set = fun a (b, c) -> mc.Set (mb.Set a b) c }
                    
            /// Decompose a monad comprised of corresponding pairs of values.
            let inline unzip (lens: Lens< ^a, (^b * ^c)>) : Lens< ^a, ^b> * Lens< ^a, ^c> =
                { Get = lens.Get >> fst
                ; Set = fun a b -> match lens.Get a with (_, c) -> lens.Set a (b, c) },
                { Get = lens.Get >> snd
                ; Set = fun a c -> match lens.Get a with (b, _) -> lens.Set a (b, c) }


    /// Creates a monadic workflow for the given type.
    let lens = Monad.LensBuilder ()


    /// Supplementary Applicative operations on the given type.
    module Applicative =

        /// Lift a binary function on effects.
        let inline map2 f fa fb =
            bind (fun a -> map (f a) fb) fa

        /// Lift a ternary function on effects.
        let inline map3 (f: ^b -> ^c -> ^d -> ^e) fa fb fc =
            Monad.bind2 (fun a b -> map (f a b) fc) fa fb

        /// Sequentially compose two effects, discarding any value produced by the first.
        let inline andThen fb fa = bind (fun _ -> fb) fa

        /// Conditional execution of effectful expressions.
        let inline when_ (condition: bool) f =
            if condition then f () else wrap ()

        /// <summary>Generalizes the sequence-based filter function.</summary>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        let inline filterA p source =
            Seq.foldBack (fun x -> map2 (fun flg xs -> if flg then x::xs else xs) (p x)) source (wrap [])

        /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        let inline sequenceA (source: Lens< ^b, ^a> seq) : Lens< ^b, ^a list> =
            Seq.foldBack (map2 (fun x xs -> x::xs)) source (wrap [])

        /// <summary>Produce an effect for the elements in the sequence from left to right then evaluate each effect, and collect the results.</summary>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        let inline forA f (source: ^a seq) : Lens< ^c, ^b list> =
            sequenceA (Seq.map f source)

        /// <summary>Produce an effect for each pair of elements in the sequences from left to right then evaluate each effect, and collect the results.</summary>
        /// <exception cref="System.ArgumentNullException">Thrown when either input sequence is null.</exception>
        let inline for2A f (source1: ^a seq) (source2: ^b seq) : Lens< ^d, ^c list> =
            forA ((<||) f) (Seq.allPairs source1 source2)

        /// <summary>Produce an effect for each pair of elements in the sequences from left to right, then evaluate each effect and collect the results.
        /// If one sequence is longer, its extra elements are ignored.</summary>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        let inline zipWithA f (source1: ^a seq) (source2: ^b seq) : Lens< ^d, ^c list> =
            sequenceA (Seq.map2 f source1 source2)  

        /// Performs the effect 'n' times.
        let inline replicateA (n: uint32) m =
            sequenceA (Seq.replicate (int n) m)


    /// Supplementary Functor operations on the given type.
    module Functor =

        /// Replace all locations in the input with the same value.
        let inline replace (b: ^b) fa = map (fun _ -> b) fa

        /// Perform an operation, store its result, perform an action using both
        /// the input and output, and finally return the output.
        let inline tee (f: ^a -> ^b) (g: ^a -> ^b -> unit) fa =
            map (fun a -> let b = f a in g a b; b) fa


    /// A functor where the first argument is contravariant and the second argument is covariant.
    module Profunctor =

        /// Map the first argument contravariantly.
        let inline lmap f (lens: Lens< ^a0, ^b>) : Lens< ^a, ^b> =
            { Get = f >> lens.Get
            ; Set = fun a _ -> a }
        
        /// Map over both arguments at the same time.
        let inline dimap (f: ^a -> ^a0) (g: ^b -> ^c) (lens: Lens< ^a0, ^b>) : Lens< ^a, ^c> =
            map g (lmap f lens)
        
        /// Map the second argument covariantly.
        let inline rmap f (lens: Lens< ^a, ^b>) : Lens< ^a, ^c> = map f lens


    /// Types with a binary, associative composition operation.
    module Semigroup =

        /// An associative composition operation.
        let inline sappend a b : Lens< ^a, ^b> =
            Applicative.map2 (fun a b -> (^b: (static member Append: ^b -> ^b -> ^b) (a, b))) a b


    /// Category -- includes an identity element and an associative composition function.
    module Cat =

        /// Identity element of a category.
        let inline identity<'a> : Lens< ^a, ^a> = { Get = id ; Set = fun x _ -> x }

        /// Compose two members of a category together.
        let inline compose (lens2: Lens< ^b, ^c>) (lens1: Lens< ^a, ^b>) : Lens< ^a, ^c> =
            { Get = lens1.Get >> lens2.Get
            ; Set = fun a c -> lens1.Set a (lens2.Set (lens1.Get a) c) }


    /// Arrows are a general, abstract view of computation. In particular, they allow
    /// notions of computation that may be independent of the input or may take multiple inputs.
    module Arrow =

        /// Lift a function to an arrow.
        let inline arr f : Lens< ^a, ^b> =
            { Get = f
            ; Set = fun a _ -> a }

        /// Send the first component of the input through the argument arrow,
        /// and copy the rest unchanged to the output.
        let inline arrFst (lens: Lens< ^a, ^b>) : Lens< ^a * ^c, ^b * ^c> =
            { Get = fun (a, c) -> lens.Get a, c
            ; Set = fun (a, _) (b, c) -> lens.Set a b, c }

        /// Send the second component of the input through the argument arrow,
        /// and copy the rest unchanged to the output.
        let inline arrSnd (lens: Lens< ^a, ^b>) : Lens< ^c * ^a, ^c * ^b> =
            { Get = fun (c, a) -> c, lens.Get a
            ; Set = fun (_, a) (c, b) -> c, lens.Set a b }

        /// Split the input between the two argument arrows and combine their output.
        let inline split (lens2: Lens< ^c, ^d>) (lens1: Lens< ^a, ^b>) : Lens< ^a * ^c, ^b * ^d> =
            { Get = fun (a, c) -> lens1.Get a, lens2.Get c
            ; Set = fun (a, c) (b, d) -> lens1.Set a b, lens2.Set c d }

        /// Fanout: send the input to both argument arrows and combine their output.
        let inline fanout (lens2: Lens< ^a, ^c>) (lens1: Lens< ^a, ^b>) : Lens< ^a, ^b * ^c> =
            { Get = fun a -> lens1.Get a, lens2.Get a
            ; Set = fun a (b, c) -> lens2.Set (lens1.Set a b) c }


        /// Choice, for arrows that support it. This class underlies the
        /// 'if' and 'case' constructs in arrow notation.
        module Choice =

            /// Feed marked inputs through the argument arrow, passing the
            /// rest through unchanged to the output. A mirror of 'feed2'.
            let inline feed1 (lens: Lens< ^a, ^b>) : Lens<Choice< ^a, ^c>, Choice< ^b, ^c>> =
                { Get = function
                    | Choice1Of2 a -> Choice1Of2 (lens.Get a)
                    | Choice2Of2 c -> Choice2Of2 c
                ; Set = fun ac bc ->
                    match bc with
                    | Choice1Of2 b -> match ac with
                                      | Choice1Of2 a -> Choice1Of2 (lens.Set a b)
                                      | Choice2Of2 c -> Choice2Of2 c
                    | Choice2Of2 c -> Choice2Of2 c }

            /// Feed marked inputs through the argument arrow, passing the
            /// rest through unchanged to the output. A mirror of 'feed1'.
            let inline feed2 (lens: Lens< ^a, ^b>) : Lens<Choice< ^c, ^a>, Choice< ^c, ^b>> =
                { Get = function
                    | Choice1Of2 c -> Choice1Of2 c
                    | Choice2Of2 a -> Choice2Of2 (lens.Get a)
                ; Set = fun ca cb ->
                    match cb with
                    | Choice1Of2 c -> Choice1Of2 c
                    | Choice2Of2 b -> match ca with
                                      | Choice1Of2 c -> Choice1Of2 c
                                      | Choice2Of2 a -> Choice2Of2 (lens.Set a b) }

            /// Split the input between the two argument arrows, retagging
            /// and merging their outputs.
            let inline merge (lens2: Lens< ^c, ^d>) (lens1: Lens< ^a, ^b>)
                : Lens<Choice< ^a, ^c>, Choice< ^b, ^d>> =
                { Get = function
                    | Choice1Of2 a -> Choice1Of2 (lens1.Get a)
                    | Choice2Of2 c -> Choice2Of2 (lens2.Get c)
                ; Set = fun ac bd ->
                    match bd with
                    | Choice1Of2 b -> match ac with
                                      | Choice1Of2 a -> Choice1Of2 (lens1.Set a b)
                                      | Choice2Of2 c -> Choice2Of2 c
                    | Choice2Of2 d -> match ac with
                                      | Choice1Of2 a -> Choice1Of2 a
                                      | Choice2Of2 c -> Choice2Of2 (lens2.Set c d) }

            /// Split the input between the two argument arrows and merge their outputs.
            let inline fanin (lens2: Lens< ^c, ^b>) (lens1: Lens< ^a, ^b>) : Lens<Choice< ^a, ^c>, ^b> =
                { Get = function
                    | Choice1Of2 a -> lens1.Get a
                    | Choice2Of2 c -> lens2.Get c
                ; Set = fun c b ->
                    match c with
                    | Choice1Of2 a -> Choice1Of2 (lens1.Set a b)
                    | Choice2Of2 c -> Choice2Of2 (lens2.Set c b) }


        /// Arrows that allow application of arrow inputs to other inputs.
        module Apply =

            /// Arrow that allows application of arrow inputs to other inputs.
            let inline app<'a, 'b> : Lens<Lens< ^a, ^b> * ^a, ^b> =
                { Get = fun (l, a) -> l.Get a
                ; Set = fun (l, a) b -> { Get = fun _ -> l.Get a
                                        ; Set = fun _ _ -> l.Set a b }, a }



open Std
open Composition

//  @ Operators @
type Lens<'a, 'b> with

// @ Primitive @

    /// Run a `Lens` by applying a `getter` to the given value, then apply both to the `setter` to retrieve a final value.
    static member inline ( >- ) (m, a) = runLens a m
    /// Run a `Lens` by applying a `getter` to the given value, then apply both to the `setter` to retrieve a final value.
    static member inline ( -< ) (a, m) = runLens a m

// @ Monad @

    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
    static member inline ( >>= ) (m, k) = bind k m
    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
    static member inline ( =<< ) (k, m) = bind k m

// @ Applicative @

    /// Sequential application on effects.
    static member inline ( <*> )  (ff, fx) = ap fx ff
    /// Sequential application on effects.
    static member inline ( <**> ) (fx, ff) = ap fx ff

    /// Sequentially compose two effects, discarding any value produced by the first.
    static member inline ( *> ) (fa, fb) = Applicative.andThen fb fa
    /// Sequentially compose two effects, discarding any value produced by the first.
    static member inline ( <* ) (fb, fa) = Applicative.andThen fb fa

// @ Functor @

    /// Lift a function onto effects.
    static member inline ( |>> ) (fa, f) = map f fa
    /// Lift a function onto effects.
    static member inline ( <<| ) (f, fa) = map f fa

    /// Replace all locations in the input with the same value.
    static member inline ( &> ) (b, fx) = Functor.replace b fx
    /// Replace all locations in the input with the same value.
    static member inline ( <& ) (fx, b) = Functor.replace b fx

// @ Semigroup @

    /// An associative composition operation.
    static member inline Append (e1, e2) = Semigroup.sappend e1 e2

    /// An associative composition operation.
    static member inline ( ++ ) (e1, e2) = Semigroup.sappend e1 e2

// @ Cat @

    /// Compose two members of a category together.
    static member inline ( >>> ) (ca, cb) = Cat.compose cb ca
    /// Compose two members of a category together.
    static member inline ( <<< ) (cb, ca) = Cat.compose cb ca

    /// Identity of the category.
    static member inline Id () : Lens< ^a, ^a> = Cat.identity

// @ Arrow @

    /// Split the input between the two argument arrows and combine their output.
    static member inline ( *** ) (aa, ab) = Arrow.split ab aa

    /// Fanout: send the input to both argument arrows and combine their output.
    static member inline ( &&& ) (aa, ab) = Arrow.fanout ab aa

// @ Arrow.Choice @

    /// Split the input between the two argument arrows, retagging and merging their outputs.
    static member inline ( +++ ) (aa, ab) = Arrow.Choice.merge ab aa

    /// Split the input between the two argument arrows and merge their outputs.
    static member inline ( ||| ) (aa, ab) = Arrow.Choice.fanin ab aa