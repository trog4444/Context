﻿namespace Ptr.Context.Type


/// Computations which read values from a shared environment.
[<Struct; NoComparison; NoEquality>]
type Reader<'env, 'result> = Reader of (^env -> ^result)    


/// Operations on `Reader` values.
module Reader =
  
    /// Runs a Reader and extracts the final value from it.
    let inline runReader (env: ^e) (Reader r) : ^a = r env

    /// Transform the value returned by a Reader.
    let inline mapReader f (Reader r) : Reader< ^e, ^b> = Reader (r >> f)

    /// Execute a computation in a modified environment.
    let inline withReader (f: ^e -> ^e0) (Reader r) : Reader< ^e, ^a> = Reader (f >> r)

    /// Retrieves the current environment.
    let inline ask<'e> : Reader< ^e, ^e> = Reader id

    /// Executes a computation in a modified environment.
    let inline local (f: ^e -> ^e) (Reader r) = Reader (f >> r)   

    /// Store computed results to prevent recomputation on the same inputs.
    let inline cacheReader (Reader f) : Reader< ^e, ^a> =
        let d = System.Collections.Concurrent.ConcurrentDictionary< ^e, ^a>(HashIdentity.Structural)
        let r = ref Unchecked.defaultof< ^a>
        Reader (fun e -> if d.TryGetValue(e, r) then !r
                         else d.GetOrAdd(key = e, value = f e))

    /// Flip a function then wrap it inside of a Reader.
    let inline flip f = Reader (fun a b -> f b a)

    /// 'const' function -- returns the first argument given and ignores the rest.
    let inline konst x = Reader (fun (_: ^``_``) -> x)

    /// 'const' function -- returns the first argument given and ignores the rest.
    let inline konst1<'a, '``_``> : Reader< ^a, (^``_`` -> ^a)> = Reader (fun (x: ^a) (_: ^``_``) -> x)

    /// Convert a function on a 2-tuple to a 2-arity, curried function.
    let inline curry f = Reader (fun a b -> f (a, b))

    /// Convert a 2-arity, curried function into a function on a 2-tuple.
    let inline uncurry f = Reader (fun (a, b) -> f a b)


    /// Compositional operations on `Reader` values.
    module Compose =

        /// Lift a value onto an effectful context.
        let inline wrap x : Reader< ^e, ^a> = Reader (fun _ -> x)

        /// Sequentially compose two effects, passing any value produced by the first
        /// as an argument to the second.
        let inline bind (k: ^a -> Reader< ^e, ^b>) (Reader r) =
            Reader (fun e -> match k (r e) with Reader r' -> r' e)

        /// Removes one layer of monadic context from a nested monad.
        let inline flatten mm : Reader< ^e, ^a> = bind id mm

        /// Sequential application on effects.
        let inline ap (Reader rv) (Reader rf) = Reader (fun (e: ^e) -> rf e (rv e))

        /// Lift a function onto effects.
        let inline map (f: ^a -> ^b) (Reader r) : Reader< ^e, ^b> = Reader (r >> f)


        /// Supplementary Monad operations on the given type.
        module Monad =

            /// Monadic computation builder specialised to the given monad.
            type ReaderBuilder () =
                member inline s.Bind(m, k) = bind k m
                member inline s.Return x = wrap x
                member inline s.ReturnFrom m : Reader< ^e, ^a> = m
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
            let inline composeM k2 (k1: ^a -> Reader< ^e, ^b>) : ^a -> Reader< ^e, ^c> = k1 >> bind k2
  
            /// Sequentially compose three actions, passing any value produced by the first
            /// two as arguments to the third.
            let inline bind2 (k: ^a -> ^b -> Reader< ^e, ^c>) (Reader ra) (Reader rb) =
                Reader (fun e -> match k (ra e) (rb e) with Reader r -> r e)

            /// Sequentially compose four actions, passing any value produced by the
            /// first two as arguments to the third.
            let inline bind3 (k: ^a -> ^b -> ^c -> Reader< ^e, ^d>) (Reader ra) (Reader rb) (Reader rc) =
                Reader (fun e -> match k (ra e) (rb e) (rc e) with Reader r -> r e)

            /// Sequentially compose two actions, creating a third from the result and
            /// lifting a binary function on its effects.
            let inline bindMap (k: ^a -> Reader< ^e, ^b>) (f: ^a -> ^b -> ^c) (Reader ra) =
                Reader (fun e -> let a = ra e in match k a with Reader r -> f a (r e))

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to the intermediate result.
            /// The default monadic 'identity' function is used in each iteration where the continuation is applied.
            let inline recM f x : Reader< ^e, ^a> =
                let rec go m = bind (f (wrap >> go)) m in go (f wrap x)

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to an 'effect' applied over the intermediate result.
            /// Any constructor can be used in each iteration, in the case of union-types.
            let inline recMp f x =
                let rec go m = bind (f go) m in go (f id x)        

            /// <summary>Monadic fold over a structure associating to the right.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldrM (f: ^a -> ^s -> Reader< ^e, ^s>) (s0: ^s) (source: ^a seq) : Reader< ^e, ^s> =
                let g k x s = bind k (f x s)
                Seq.fold g wrap source s0

            /// <summary>Monadic fold over a structure associating to the left.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldlM (f: ^s -> ^a -> Reader< ^e, ^s>) (s0: ^s) (source: ^a seq) : Reader< ^e, ^s> =
                let g x k s = bind k (f s x)
                Seq.foldBack g source wrap s0        


            /// Monadic zipping (combining or decomposing corresponding monadic elements).
            module Zip =
            
                /// Combine the corresponding contents of two monads into a single monad.
                let inline mzipWith (f: ^a -> ^b -> ^c) (Reader ra) (Reader rb) : Reader< ^e, ^c> =
                    Reader (fun e -> f (ra e) (rb e))

                /// Merge the contents (of corresponding pairs) of two monads into a monad of pairs.
                let inline mzip (Reader ra) (Reader rb) : Reader< ^e, ^a * ^b> =
                    Reader (fun e -> ra e, rb e)
                    
                /// Decompose a monad comprised of corresponding pairs of values.
                let inline munzip (Reader r) : Reader< ^e, ^a> * Reader< ^e, ^b> =
                    Reader (r >> fst), Reader (r >> snd)    


        /// Supplementary Applicative operations on the given type.
        module Applicative =

            /// Lift a binary function on effects.
            let inline map2 (f: ^a -> ^b -> ^c) (Reader ra) (Reader rb) : Reader< ^e, ^c> =
                Reader (fun e -> f (ra e) (rb e))

            /// Lift a ternary function on effects.
            let inline map3 (f: ^a -> ^b -> ^c -> ^d) (Reader ra) (Reader rb) (Reader rc) : Reader< ^e, ^d> =
                Reader (fun e -> f (ra e) (rb e) (rc e))

            /// Sequentially compose two effects, discarding any value produced by the first.
            let inline andThen (Reader rb) (Reader ra) : Reader< ^e, ^b> =
                Reader (fun e -> ignore (ra e); rb e)

            /// Conditional execution of effectful expressions.
            let inline when_ (condition: bool) f : Reader< ^e, unit> =
                if condition then f () else wrap ()

            /// <summary>Generalizes the sequence-based filter function.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline filterA (p: ^a -> Reader< ^e, bool>) source =
                Reader (fun e -> seq { for x in source do
                                        match p x with Reader r -> if r e then yield x })   

            /// <summary>Evaluate each effect in the sequence from left to right,
            /// and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline sequenceA (source: Reader< ^e, ^r> seq) : Reader< ^e, ^r seq> =
                Reader (fun e -> Seq.map (fun (Reader r) -> r e) source)

            /// <summary>Produce an effect for the elements in the sequence from left to right
            /// then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline forA (f: ^a -> Reader< ^e, ^r>) (source: ^a seq) : Reader< ^e, ^r seq> =
                sequenceA (Seq.map f source)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right
            /// then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when either input sequence is null.</exception>
            let inline for2A (f: ^a -> ^b -> Reader< ^e, ^r>) (source1: ^a seq) (source2: ^b seq)
                : Reader< ^e, ^r seq> =
                forA ((<||) f) (Seq.allPairs source1 source2)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right,
            /// then evaluate each effect and collect the results.
            /// If one sequence is longer, its extra elements are ignored.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline zipWithA (f: ^a -> ^b -> Reader< ^e, ^r>) (source1: ^a seq) (source2: ^b seq)
                : Reader< ^e, ^r seq> =
                sequenceA (Seq.map2 f source1 source2)

            /// Performs the effect 'n' times.
            let inline replicateA (n: uint32) (Reader r) : Reader< ^e, ^a seq> =
                Reader (r >> Seq.replicate (int n))


        /// Supplementary Functor operations on the given type.
        module Functor =

            /// Replace all locations in the input with the same value.
            let inline replace (b: ^b) (Reader r) : Reader< ^e, ^b> = Reader (fun e -> ignore (r e); b)

            /// Perform an operation, store its result, perform an action using both
            /// the input and output, and finally return the output.
            let inline tee (f: ^a -> ^b) (g: ^a -> ^b -> unit) (Reader r) : Reader< ^e, ^b> =
                Reader (fun e -> let a = r e in let b = f a in g a b; b)       
                       

        /// A functor where the first argument is contravariant and the second argument is covariant.
        module Profunctor =

            /// Map over both arguments at the same time.
            let inline dimap (f: ^e -> ^e0) (g: ^a -> ^b) (Reader r) : Reader< ^e, ^b> =
                Reader (f >> r >> g)

            /// Map the first argument contravariantly.
            let inline lmap f (Reader r) : Reader< ^e, ^a> = Reader (f >> r)

            /// Map the second argument covariantly.
            let inline rmap f (Reader r) : Reader< ^e, ^a> = Reader (r >> f)


        /// Adjunction to the given functor.
        module Adjoint =

            /// Construct a functor's Left-adjoint within itself.
            let inline unit a : Reader< ^e, ^e * ^a> = Reader (fun e -> e, a)

            /// Deconstruct a functor's Left-adjoint (containing that functor) into a value.
            let inline counit ((e: ^e), (Reader r)) : ^a = r e

            /// Lift a function on a functor's Left-adjoint into itself.
            let inline leftAdjunct f (a: ^a) : Reader< ^e, ^b> =
                Reader (fun e -> f (e, a))

            /// Deconstruct a functor's Left-adjoint using itself.
            let inline rightAdjunct (f: ^a -> Reader< ^e, ^b>) (e, a) =
                match f a with Reader r -> r e

            /// Monadic 'bind' from the right-ajoint over its left-adjoint.
            let inline leftBind f (Reader (r: ^e -> ^c * ^a)) : Reader< ^e, ^d * ^b> =
                Reader (fun e -> match r e with (c, a) -> match f a with Reader r1 -> r1 c) 


        /// Types with a binary, associative composition operation.
        module Semigroup =

            /// An associative composition operation.
            let inline sappend (Reader f) (Reader g) : Reader< ^e, ^a> =
                Reader (fun e -> (^a: (static member inline Append: ^a -> ^a -> ^a) (f e, g e)))
    

        /// Category -- includes an identity element and an associative composition function.
        module Cat =

            /// Identity element of a category.
            let inline identity<'a> : Reader< ^a, ^a> = Reader id

            /// Compose two members of a category together.
            let inline compose (Reader rb) (Reader ra) = Reader (ra >> rb)


        /// Arrows are a general, abstract view of computation. In particular, they allow
        /// notions of computation that may be independent of the input or may take multiple inputs.
        module Arrow =

            /// Lift a function to an arrow.
            let inline arr f : Reader< ^e, ^a> = Reader f

            /// Send the first component of the input through the argument arrow,
            /// and copy the rest unchanged to the output.
            let inline arrFst (Reader r) : Reader< ^a * ^c, ^b * ^c> = Reader (fun (a, c) -> r a, c)

            /// Send the second component of the input through the argument arrow,
            /// and copy the rest unchanged to the output.
            let inline arrSnd (Reader r) : Reader< ^c * ^a, ^c * ^b> = Reader (fun (c, a) -> c, r a)

            /// Split the input between the two argument arrows and combine their output.
            let inline split (Reader rb) (Reader ra) : Reader< ^a * ^c, ^b * ^d> =
                Reader (fun (a, b) -> ra a, rb b)

            /// Fanout: send the input to both argument arrows and combine their output.
            let inline fanout (Reader rb) (Reader ra) : Reader< ^a, ^b * ^c> =
                Reader (fun e -> ra e, rb e)


            /// Choice, for arrows that support it. This class underlies the
            /// 'if' and 'case' constructs in arrow notation.
            module Choice =

                /// Feed marked inputs through the argument arrow, passing the
                /// rest through unchanged to the output. A mirror of 'feed2'.
                let inline feed1 (Reader f) =
                    Reader (function
                        | Choice1Of2 x -> Choice1Of2 (f x)
                        | Choice2Of2 y -> Choice2Of2 y)
            
                /// Feed marked inputs through the argument arrow, passing the
                /// rest through unchanged to the output. A mirror of 'feed1'.
                let inline feed2 (Reader f) =
                    Reader (function
                        | Choice1Of2 x -> Choice1Of2 x
                        | Choice2Of2 y -> Choice2Of2 (f y))

                /// Split the input between the two argument arrows, retagging
                /// and merging their outputs.
                let inline merge (Reader (g: ^c -> ^d)) (Reader (f: ^a -> ^b)) = 
                    Reader (function
                        | Choice1Of2 x -> Choice1Of2 (f x)
                        | Choice2Of2 y -> Choice2Of2 (g y))

                /// Split the input between the two argument arrows and merge their outputs.
                let inline fanin (Reader g) (Reader f) : Reader<Choice< ^a, ^c>, ^b> =
                    Reader (function
                        | Choice1Of2 x -> f x
                        | Choice2Of2 y -> g y)


            /// Arrows that allow application of arrow inputs to other inputs.
            module Apply =

                /// Arrow that allows application of arrow inputs to other inputs.
                let inline app<'a, 'b> : Reader<Reader< ^a, ^b> * ^a, ^b> =
                    Reader (fun ((Reader f), b) -> f b)


    /// Creates a computation expression for the given type.
    let reader = Compose.Monad.ReaderBuilder ()



open Reader
open Compose
  
//  @ Operators @
type Reader<'e, 'a> with

// @ Primitive @

    /// The result of running a computation with a given environment.
    static member inline ( >- ) (m, e) = runReader e m
    /// The result of running a computation with a given environment.
    static member inline ( -< ) (e, m) = runReader e m

// @ Cat @

    /// Compose two members of a category together.
    static member inline ( >>> ) (ca, cb) = Cat.compose cb ca
    /// Compose two members of a category together.
    static member inline ( <<< ) (cb, ca) = Cat.compose cb ca

    /// Identity of the category.
    static member inline Id () : Reader< ^a, ^a> = Cat.identity

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