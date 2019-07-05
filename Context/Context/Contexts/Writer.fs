namespace Ptr.Context.Type


/// Type that holds both a `log` and `value`.
[<Struct>]
type Writer<'log, 'value> = { Log: ^log ; Value: ^value }
with interface System.Collections.Generic.IEnumerable< ^value> with
        override s.GetEnumerator() = match s with { Value = x } -> (Seq.singleton x).GetEnumerator()
        override s.GetEnumerator() = (s :> _ seq).GetEnumerator() :> System.Collections.IEnumerator


/// Operations on `Writer` values.
module Writer =

    /// Active patterns on `Writer` values.
    module Pattern =
    
        /// Return a 'Writer'-value "as is".
        let inline ( |Writer| ) (w: Writer< ^l, ^c>) = Writer w
    
        /// Return the log from a 'Writer'.
        let inline ( |WriterLog| ) (w: Writer< ^l, ^c>) = WriterLog w.Log
    
        /// Return the value from a 'Writer'.
        let inline ( |WriterValue| ) (w: Writer< ^l, ^c>) = WriterValue w.Value


    /// Convert between values of type `Writer` and related types.
    module Convert =

        /// Convert a pair `Writer`.
        let inline ofPair log value = { Log = log ; Value = value }

        /// Convert a `Writer` to a pair.
        let inline toPair { Log = l ; Value = v } = l, v

        /// Convert a `Writer` to a pair.
        let inline toPair1 { Log = l ; Value = v } = struct (l, v)


    /// Unwrap a Writer as a (log, value) pair.
    let inline runWriter f (w: Writer< ^l, ^c>) = f w.Log w.Value

    /// Create a Writer with just a logger.
    let inline tell log : Writer< ^l, unit> = { Log = log ; Value = () }

    /// Takes a Writer and combines its value and logger into a new value pair,
    /// while maintaining the same logger.
    let inline listen (w: Writer< ^l, ^c>) : Writer< ^l, (^l * ^c)> =
        { Log = w.Log ; Value = (w.Log, w.Value) }

    /// Adds the result of applying 'f' to the logger and combines it with the original value.
    let inline listens f (w: Writer< ^l, ^a>) : Writer< ^l, (^a * ^b)> =
        { Log = w.Log ; Value = (w.Value, f w.Log) }
        

    /// Compositional operations on `Writer` values.
    module Compose =

        /// Supplementary Monad operations on the given type.
        module Monad =

            /// Lift a value onto an effectful context.
            let inline wrap x : Writer< ^l, ^a> =
                { Log = (^l : (static member Empty: unit -> ^l) ())
                ; Value = x }

            /// Sequentially compose two effects, passing any value produced by the first
            /// as an argument to the second.
            let inline bind (k: ^a -> Writer< ^l, ^b>) (m: Writer< ^l, ^a>) =
                let w2 = k m.Value
                { Log = (^l: (static member Append: ^l -> ^l -> ^l) (m.Log, w2.Log))
                ; Value = w2.Value }

            /// Removes one layer of monadic context from a nested monad.
            let inline flatten mm = bind id mm


            /// Monadic computation builder specialised to the given monad.
            type WriterBuilder () =
                member inline s.Bind(m, k) = bind k m
                member inline s.Return x = wrap x
                member inline s.ReturnFrom m : Writer< ^l, ^a> = m
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
            let inline composeM k2 (k1: ^a -> Writer< ^l, ^b>) : ^a -> Writer< ^l, ^c> = k1 >> bind k2
  
            /// Sequentially compose three actions, passing any value produced by the first
            /// two as arguments to the third.
            let inline bind2 (k: ^a -> ^b -> Writer< ^l, ^c>)
                             (ma: Writer< ^l, ^a>)
                             (mb: Writer< ^l, ^b>) : Writer< ^l, ^c> =
                bind (fun a -> bind (k a) mb) ma

            /// Sequentially compose four actions, passing any value produced by the
            /// first two as arguments to the third.
            let inline bind3 (k: ^a -> ^b -> ^c -> Writer< ^l, ^d>)
                             (ma: Writer< ^l, ^a>)
                             (mb: Writer< ^l, ^b>)
                             (mc: Writer< ^l, ^c>) : Writer< ^l, ^d> =
                bind2 (fun a b -> bind (k a b) mc) ma mb

            /// Sequentially compose two actions, creating a third from the result and
            /// lifting a binary function on its effects.
            let inline bindMap (k: ^a -> Writer< ^l, ^b>)
                               (f: ^a -> ^b -> ^c)
                               (m: Writer< ^l, ^a>) : Writer< ^l, ^c> =
                bind (fun a -> bind (f a >> wrap) (k a)) m

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to the intermediate result.
            /// The default monadic 'identity' function is used in each iteration where the continuation is applied.
            let inline recM f x : Writer< ^l, ^a> =
                let rec go m = bind (f (wrap >> go)) m in go (f wrap x)

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to an 'effect' applied over the intermediate result.
            /// Any constructor can be used in each iteration, in the case of union-types.
            let inline recMp f x : Writer< ^l, ^a> =
                let rec go m = bind (f go) m in go (f id x)        

            /// <summary>Monadic fold over a structure associating to the right.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldrM (f: ^a -> ^s -> Writer< ^l, ^s>) (s0: ^s) (source: ^a seq) : Writer< ^l, ^s> =
                let inline g k x s = bind k (f x s)
                Seq.fold g wrap source s0

            /// <summary>Monadic fold over a structure associating to the left.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline foldlM (f: ^s -> ^a -> Writer< ^l, ^s>) (s0: ^s) (source: ^a seq) : Writer< ^l, ^s> =
                let inline g x k s = bind k (f s x)
                Seq.foldBack g source wrap s0


            /// Monadic zipping (combining or decomposing corresponding monadic elements).
            module Zip =
            
                /// Combine the corresponding contents of two monads into a single monad.
                let inline mzipWith f (ma: Writer< ^l, ^a>) (mb: Writer< ^l, ^b>) : Writer< ^l, ^c> =
                    { Log = (^l: (static member Append: ^l -> ^l -> ^l) (ma.Log, mb.Log))
                    ; Value = f ma.Value mb.Value }

                /// Merge the contents (of corresponding pairs) of two monads into a monad of pairs.
                let inline mzip (ma: Writer< ^l, ^a>) (mb: Writer< ^l, ^b>) : Writer< ^l, ^a * ^b> =
                    { Log = (^l: (static member Append: ^l -> ^l -> ^l) (ma.Log, mb.Log))
                    ; Value = ma.Value, mb.Value }
                    
                /// Decompose a monad comprised of corresponding pairs of values.
                let inline munzip { Log = (log: ^l) ; Value = (a: ^a), (b: ^b) } =
                    { Log = log ; Value = a }, { Log = log ; Value = b }


        /// Supplementary Applicative operations on the given type.
        module Applicative =

            /// Lift a value onto an effectful context.
            let inline wrap x : Writer< ^l, ^a> =
                { Log = (^l : (static member Empty: unit -> ^l) ())
                ; Value = x }

            /// Sequential application on effects.
            let inline ap (mv: Writer< ^l, ^a>) (mf: Writer< ^l, ^a -> ^b>) =
                { Log = (^l: (static member Append: ^l -> ^l -> ^l) (mf.Log, mv.Log))
                ; Value = mf.Value mv.Value }

            /// Lift a binary function on effects.
            let inline map2 f (ma: Writer< ^l, ^a>) (mb: Writer< ^l, ^b>) : Writer< ^l, ^c> =
                { Log = (^l: (static member Append: ^l -> ^l -> ^l) (ma.Log, mb.Log))
                ; Value = f ma.Value mb.Value }

            /// Lift a ternary function on effects.
            let inline map3 f (ma: Writer< ^l, ^a>) (mb: Writer< ^l, ^b>) (mc: Writer< ^l, ^c>) : Writer< ^l, ^d> =
                let inline app a b = (^l: (static member Append: ^l -> ^l -> ^l) (a, b))
                { Log = app ma.Log (app mb.Log mc.Log)
                ; Value = f ma.Value mb.Value mc.Value }

            /// Sequentially compose two effects, discarding any value produced by the first.
            let inline andThen mb ma : Writer< ^l, ^b> =
                { mb with Log = (^l: (static member Append: ^l -> ^l -> ^l) (ma.Log, mb.Log)) }

            /// Conditional execution of effectful expressions.
            let inline when_ (condition: bool) f : Writer< ^l, unit> =
                if condition then f () else wrap ()

            /// <summary>Generalizes the sequence-based filter function.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline filterA (p: ^a -> Writer< ^l, bool>) source =    
                Seq.foldBack (fun x xs -> map2 (fun flg xs -> if flg then x::xs else xs) (p x) xs) source (wrap [])

            /// <summary>Evaluate each effect in the sequence from left to right,
            /// and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline sequenceA (source: Writer< ^l, ^a> seq) : Writer< ^l, ^a list> =
                Seq.foldBack (map2 (fun x xs -> x::xs)) source (wrap [])

            /// <summary>Produce an effect for the elements in the sequence from left to right
            /// then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline forA f source : Writer< ^l, ^b list> =
                sequenceA (Seq.map f source)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right
            /// then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when either input sequence is null.</exception>
            let inline for2A f source1 source2 : Writer< ^l, ^c list> =
                forA ((<||) f) (Seq.allPairs source1 source2)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right,
            /// then evaluate each effect and collect the results.
            /// If one sequence is longer, its extra elements are ignored.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline zipWithA f source1 source2 : Writer< ^l, ^c list> =
                sequenceA (Seq.map2 f source1 source2)

            /// Performs the effect 'n' times.
            let inline replicateA (n: uint32) m : Writer< ^l, ^a list> =
                sequenceA (Seq.replicate (int n) m)


        /// Supplementary Functor operations on the given type.
        module Functor =

            /// Lift a function onto effects.
            let inline map f (m: Writer< ^l, ^a>) : Writer< ^l, ^b> =
                { Log = m.Log ; Value = f m.Value }

            /// Replace all locations in the input with the same value.
            let inline replace b { Log = log } : Writer< ^l, ^b> = { Log = log ; Value = b }

            /// Perform an operation, store its result, perform an action using both
            /// the input and output, and finally return the output.
            let inline tee f (g: ^a -> ^b -> unit) { Log = log ; Value = a } : Writer< ^l, ^b> =
                let b = f a
                do g a b
                { Log = log ; Value = b }


        /// A two paramater functor where both the first and second arguments are covariant.
        module Bifunctor =

            /// Map over both arguments at the same time.
            let inline bimap (f: ^l1 -> ^l2) (g: ^a -> ^b) { Log = l ; Value = v } : Writer< ^l2, ^b> =
                { Log = f l ; Value = g v }

            /// Map covariantly over the first argument.
            let inline mapFst (f: ^l1 -> ^l2) { Log = l ; Value = v } : Writer< ^l2, ^a> =
                { Log = f l ; Value = v }

            /// Map covariantly over the second argument.
            let inline mapSnd (g: ^a -> ^b) { Log = l ; Value = v } : Writer< ^l, ^b> =
                { Log = l ; Value = g v }


        /// Adjunction to the given functor.
        module Adjoint =

            /// Construct a functor's adjoint within itself.
            let inline unit a : ^e -> Writer< ^e, ^a> = fun e -> { Log = e ; Value = a }

            /// Deconstruct a functor's Left-adjoint (containing that functor) into a value.
            let inline counit { Log = (e: ^e) ; Value = r } : ^a = r e

            /// Lift a function on a functor's Left-adjoint into itself.
            let inline leftAdjunct f (a: ^a) : (^e -> ^b) = fun e -> f { Log = e ; Value = a }

            /// Deconstruct a functor's Left-adjoint using itself.
            let inline rightAdjunct (f: ^a -> (^e -> ^b)) { Log = e ; Value = a } = f a e

            /// Monadic 'bind' from the right-ajoint over its left-adjoint.
            let inline rightBind f (r: ^e -> Writer< ^c, ^a>) : (^e -> Writer< ^d, ^b>) =
                fun e -> match r e with w -> f w.Value w.Log


        /// Supplementary Comonad operations on the given type.
        module Comonad =

            /// Retrieve a value out of a context.
            let inline extract { Log = a } = a

            /// Sequentially compose two co-effects.
            let inline extend j (w: Writer< ^l, ^a>) : Writer< ^l, ^b> = { Log = w.Log ; Value = j w }        

            /// Takes a comonadic container and produces a container of containers.
            let inline duplicate w = extend id w

            /// Composes two comonadic functions together. Acts as the composition function in the CoKleisli category.
            let inline composeW (f2: Writer< ^l, ^b> -> ^c) f1 : Writer< ^l, ^a> -> ^c = extend f1 >> f2
            
            /// Sequentially compose two co-actions, creating a third from the result and
            /// lifting a binary function on its effects.
            let inline extendMap j f w : Writer< ^l, ^c> =
                Functor.map (fun a -> f a (j w)) w

            /// Deconstructs a comonad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function.
            let inline recW f (w: Writer< ^a, ^a>) =
                let rec go w = f go w in go (extend (f extract) w)


        /// Types with a binary, associative composition operation.
        module Semigroup =

            /// An associative composition operation.
            let inline sappend (a: Writer< ^l, ^a>) (b: Writer< ^l, ^a>) : Writer< ^l, ^a> =
                { Log = (^l: (static member Append: ^l -> ^l -> ^l) (a.Log, b.Log))
                ; Value = (^a: (static member Append: ^a -> ^a -> ^a) (a.Value, b.Value)) }


    /// Creates a computation expression for the given type.
    let writer = Compose.Monad.WriterBuilder ()



open Writer
open Compose
  
//  @ Operators @
type Writer<'l, 'a> with

// @ Primitive @

    /// Unwrap a Writer computation as a (log, value) pair, with the logger having been run.
    static member inline ( >- ) (w, f) = runWriter f w
    /// Unwrap a Writer computation as a (log, value) pair, with the logger having been run.
    static member inline ( -< ) (f, w) = runWriter f w

// @ Monad @

    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
    static member inline ( >>= ) (m, k) = Monad.bind k m
    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
    static member inline ( =<< ) (k, m) = Monad.bind k m

// @ Applicative @

    /// Sequential application on effects.
    static member inline ( <*> )  (ff, fx) = Applicative.ap fx ff
    /// Sequential application on effects.
    static member inline ( <**> ) (fx, ff) = Applicative.ap fx ff

    /// Sequentially compose two effects, discarding any value produced by the first.
    static member inline ( *> ) (fa, fb) = Applicative.andThen fb fa
    /// Sequentially compose two effects, discarding any value produced by the first.
    static member inline ( <* ) (fb, fa) = Applicative.andThen fb fa

// @ Functor @

    /// Lift a function onto effects.
    static member inline ( |>> ) (fa, f) = Functor.map f fa
    /// Lift a function onto effects.
    static member inline ( <<| ) (f, fa) = Functor.map f fa

    /// Replace all locations in the input with the same value.
    static member inline ( %> ) (b, fx) = Functor.replace b fx
    /// Replace all locations in the input with the same value.
    static member inline ( <% ) (fx, b) = Functor.replace b fx

// @ Comonad @

    /// Sequentially compose two co-effects.
    static member inline ( =>> ) (w, j) = Comonad.extend j w
    /// Sequentially compose two co-effects.
    static member inline ( <<= ) (j, w) = Comonad.extend j w

// @ Semigroup @

    /// An associative composition operation.
    static member inline Append (e1, e2) = Semigroup.sappend e1 e2

    /// An associative composition operation.
    static member inline ( ++ ) (e1, e2) = Semigroup.sappend e1 e2