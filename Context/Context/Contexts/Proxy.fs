namespace Ptr.Context.Type.Proxy


/// Type that holds type information but no actual data.
[<Struct>]
type Proxy<'a> = PROXY

/// Type that holds type information but no actual data.
[<Struct>]
type Proxy<'a, 'b> = PROXY2


/// Standard operations on `Proxy` values.
module Std =
    
    /// Create a new Proxy of the supplied type.
    let inline newProxy<'a> : Proxy< ^a> = PROXY

    /// Change a 'Proxy's' inner type.
    let inline coerce<'a, 'b> (_: Proxy< ^a>) : Proxy< ^b> = PROXY


/// Compositional operations on `Proxy` values.
module Composition =

    /// Lift a value onto an effectful context.
    let inline wrap (_: ^a) : Proxy< ^a> = PROXY

    /// Sequentially compose two effects, passing any value produced by the first
    /// as an argument to the second.
    let inline bind (_: ^a -> Proxy< ^b>) (_: Proxy< ^a>) : Proxy< ^b> = PROXY

    /// Removes one layer of monadic context from a nested monad.
    let inline flatten (_: Proxy<Proxy< ^a>>) : Proxy< ^a> = PROXY

    /// Sequential application on effects.
    let inline ap (_: Proxy< ^a>) (_: Proxy< ^a -> ^b>) : Proxy< ^b> = PROXY

    /// Lift a function onto effects.
    let inline map (_: ^a -> ^b) (_: Proxy< ^a>) : Proxy< ^b> = PROXY


    /// Supplementary Monad operations on the given type.
    module Monad =

        /// Monadic computation builder specialised to the given monad.
        type ProxyBuilder () =
            member inline s.Bind(m, k) = bind k m
            member inline s.Return x = wrap x
            member inline s.ReturnFrom m : Proxy< ^a> = m
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
        let inline composeM (k2: ^b -> Proxy< ^c>) (k1: ^a -> Proxy< ^b>) : ^a -> Proxy< ^c> = fun _ -> PROXY
  
        /// Sequentially compose three actions, passing any value produced by the first
        /// two as arguments to the third.
        let inline bind2 (k: ^a -> ^b -> Proxy< ^c>) (ma: Proxy< ^a>) (mb: Proxy< ^b>) : Proxy< ^c> = PROXY

        /// Sequentially compose four actions, passing any value produced by the
        /// first two as arguments to the third.
        let inline bind3 (k: ^a -> ^b -> ^c -> Proxy< ^d>)
            (ma: Proxy< ^a>) (mb: Proxy< ^b>) (mc: Proxy< ^c>) : Proxy< ^d> = PROXY

        /// Sequentially compose two actions, creating a third from the result and
        /// lifting a binary function on its effects.
        let inline bindMap (k: ^a -> Proxy< ^b>) (f: ^a -> ^b -> ^c) (_: Proxy< ^a>) : Proxy< ^c> = PROXY

        /// Build a monad through recursive (effectful) computations.
        /// Computation proceeds through the use of a continuation function applied to the intermediate result.
        /// The default monadic 'identity' function is used in each iteration where the continuation is applied.
        let inline recM (_: (^a -> Proxy< ^b>) -> ^a -> Proxy< ^b>) (_: ^a) : Proxy< ^b> = PROXY

        /// Build a monad through recursive (effectful) computations.
        /// Computation proceeds through the use of a continuation function applied to an 'effect' applied over the intermediate result.
        /// Any constructor can be used in each iteration, in the case of union-types.
        let inline recMp (_: (Proxy< ^a> -> Proxy< ^a>) -> ^a -> Proxy< ^a>) (_: ^a) : Proxy< ^a> = PROXY        

        /// <summary>Monadic fold over a structure associating to the right.</summary>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        let inline foldrM (_: ^a -> ^s -> Proxy< ^s>) (_: ^s) (_: ^a seq) : Proxy< ^s> = PROXY

        /// <summary>Monadic fold over a structure associating to the left.</summary>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        let inline foldlM (_: ^s -> ^a -> Proxy< ^s>) (_: ^s) (_: ^a seq) : Proxy< ^s> = PROXY        
        

        /// Monads that also support choice and failure.
        module Plus =

            /// The identity of mplus.
            let inline mzero<'a> : Proxy< ^a> = PROXY

            /// A monoidal operation on monads, supporting choice and failure.
            let inline mplus (m1: Proxy< ^a>) (m2: Proxy< ^a>) : Proxy< ^a> = PROXY

            /// Conditional blocking of effectful computations.
            let inline guard (_: bool) : Proxy<unit> = PROXY

            /// Create a new item if the previous was mzero, else keep the original.
            let inline recover (_: unit -> Proxy< ^a>) (_: Proxy< ^a>) : Proxy< ^a> = PROXY

            /// Combine two monads using a 'SQL style' inner join function.
            let inline relate (_: ^a -> ^b -> ^c) (k1: ^a -> ^k) (k2: ^b -> ^k when ^k : equality)
                (ma: Proxy< ^a>) (mb: Proxy< ^b>) : Proxy< ^c> = PROXY


            /// Generalizations of functions on other types.
            module General =

                /// <summary>Generalizes list concatenation to monads.</summary>
                /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
                let inline msum (_: ^a seq) : Proxy< ^a> = PROXY

                /// <summary>Generalizes the 'ofSeq' function.</summary>
                /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
                let inline mOfSeq (_: ^a seq) : Proxy< ^a> = PROXY

                /// Generalizes the 'Seq.where' function.
                let inline mwhere (_: ^a -> bool) (_: Proxy< ^a>) : Proxy< ^a> = PROXY

                /// Opposite of the 'mwhere' function.
                let inline mremove (_: ^a -> bool) (_: Proxy< ^a>) : Proxy< ^a> = PROXY

                /// Generalizes the 'Seq.partition' function.
                let inline mpartition (_: ^a -> bool) (_: Proxy< ^a>) : Proxy< ^a> * Proxy< ^a> = PROXY, PROXY

                /// Translate a form of Option.defaultWith to an arbitrary 'MonadPlus' type.
                let inline mofOption (_: ^a option) : Proxy< ^a> = PROXY

                /// Translate a form of 'Option.defaultWith' to an arbitrary 'MonadPlus' type.
                let inline mconcatOption (_: Proxy< ^a option>) : Proxy< ^a> = PROXY

                /// Generalizes the 'Seq.choose' function.
                let inline mchoose (_: ^a -> ^b option) (_: Proxy< ^a>) : Proxy< ^b> = PROXY

                /// Collects the values from Choice1Of2's, while discarding the rest.
                let inline mchoice1 (_: Proxy<Choice< ^a, ^``_``>>) : Proxy< ^a> = PROXY
                        
                /// Collects the values from Choice2Of2's, while discarding the rest.
                let inline mchoice2 (_: Proxy<Choice< ^``_``, ^b>>) : Proxy< ^b> = PROXY

                /// Collects the values from Choice1Of2s on the left, and from Choice2Of2s on the right.
                let inline mpartitionChoice (_: Proxy<Choice< ^a, ^b>>) : Proxy< ^a> * Proxy< ^b> = PROXY, PROXY


        /// Monadic zipping (combining or decomposing corresponding monadic elements).
        module Zip =
            
            /// Combine the corresponding contents of two monads into a single monad.
            let inline mzipWith (_: ^a -> ^b -> ^c) (ma: Proxy< ^a>) (mb: Proxy< ^b>) : Proxy< ^c> = PROXY

            /// Merge the contents (of corresponding pairs) of two monads into a monad of pairs.
            let inline mzip (ma: Proxy< ^a>) (mb: Proxy< ^b>) : Proxy< ^a * ^b> = PROXY
                    
            /// Decompose a monad comprised of corresponding pairs of values.
            let inline munzip (_: Proxy< ^a * ^b>) : Proxy< ^a> * Proxy< ^b> = PROXY, PROXY


    /// Creates a monadic workflow for the given type.
    let proxy = Monad.ProxyBuilder ()


    /// Supplementary Applicative operations on the given type.
    module Applicative =

        /// Lift a binary function on effects.
        let inline map2 (_: ^a -> ^b -> ^c)
            (_: Proxy< ^a>)
            (_: Proxy< ^b>) : Proxy< ^c> = PROXY

        /// Lift a ternary function on effects.
        let inline map3 (_: ^a -> ^b -> ^c -> ^d)
            (_: Proxy< ^a>)
            (_: Proxy< ^b>)
            (_: Proxy< ^c>) : Proxy< ^d> = PROXY

        /// Sequentially compose two effects, discarding any value produced by the first.
        let inline andThen (_: Proxy< ^b>) (_: Proxy< ^a>) : Proxy< ^b> = PROXY

        /// Conditional execution of effectful expressions.
        let inline when_ (_: bool) (_: unit -> Proxy<unit>) : Proxy<unit> = PROXY

        /// <summary>Generalizes the sequence-based filter function.</summary>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        let inline filterA (_: ^a -> Proxy<bool>) (_: ^a seq) : Proxy< ^a seq> = PROXY

        /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        let inline sequenceA (_: Proxy< ^a> seq) : Proxy< ^a seq> = PROXY

        /// <summary>Produce an effect for the elements in the sequence from left to right then evaluate each effect, and collect the results.</summary>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        let inline forA (_: ^a -> Proxy< ^b>) (_: ^a seq) : Proxy< ^b seq> = PROXY

        /// <summary>Produce an effect for each pair of elements in the sequences from left to right then evaluate each effect, and collect the results.</summary>
        /// <exception cref="System.ArgumentNullException">Thrown when maybe input sequence is null.</exception>
        let inline for2A (_: ^a -> ^b -> Proxy< ^c>) (source1: ^a seq) (source2: ^b seq) : Proxy< ^c seq> = PROXY

        /// <summary>Produce an effect for each pair of elements in the sequences from left to right, then evaluate each effect and collect the results.
        /// If one sequence is longer, its extra elements are ignored.</summary>
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        let inline zipWithA (_: ^a -> ^b -> Proxy< ^c>)
            (source1: ^a seq) (source2: ^b seq) : Proxy< ^c seq> = PROXY

        /// Performs the effect 'n' times.
        let inline replicateA (_: uint32) (_: Proxy< ^a>) : Proxy< ^a seq> = PROXY


        /// A monoid on applicative functors.
        module Alternative =

            /// The identity of orElse.
            let inline empty<'a> : Proxy< ^a> = PROXY

            /// An associative binary operation on applicative functors.
            let inline orElse (choice2: Proxy< ^a>) (choice1: Proxy< ^a>) : Proxy< ^a> = PROXY

            /// <summary>The sum of a collection of effects.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline asum (_: Proxy< ^a> seq) : Proxy< ^a> = PROXY

            /// Return one or none results on effects.
            let inline optional (_: Proxy< ^a>) : Proxy< ^a option> = PROXY

            /// Create a new item if the previous was empty, else keep the original.
            let inline alt (_: unit -> Proxy< ^a>) (_: Proxy< ^a>) : Proxy< ^a> = PROXY


    /// Supplementary Functor operations on the given type.
    module Functor =

        /// Replace all locations in the input with the same value.
        let inline replace (_: ^b) (_: Proxy< ^a>) : Proxy< ^b> = PROXY

        /// Perform an operation, store its result, perform an action using both
        /// the input and output, and finally return the output.
        let inline tee (_: ^a -> ^b) (_: ^a -> ^b -> unit) (_: Proxy< ^a>) : Proxy< ^b> = PROXY


    /// A two paramater functor where both the first and second arguments are covariant.
    module Bifunctor =

        /// Map over both arguments at the same time.
        let inline bimap (f: ^a -> ^b) (g: ^c -> ^d) (_: Proxy< ^a, ^c>) : Proxy< ^b, ^d> = PROXY2

        /// Map covariantly over the first argument.
        let inline mapFst (_: ^a -> ^b) (_: Proxy< ^a, ^c>) : Proxy< ^b, ^c> = PROXY2

        /// Map covariantly over the second argument.
        let inline mapSnd (_: ^b -> ^c) (_: Proxy< ^a, ^b>) : Proxy< ^a, ^c> = PROXY2


    /// Supplementary Contrafunctor operations on the given type.
    module Contrafunctor =

        /// Map a function contravariantly.
        let inline contramap (_: ^b -> ^a) (_: Proxy< ^a>) : Proxy< ^b> = PROXY


    /// A functor where the first argument is contravariant and the second argument is covariant.
    module Profunctor =

        /// Map over both arguments at the same time.
        let inline dimap (f: ^b -> ^a) (g: ^c -> ^d) (_: Proxy< ^a, ^b>) : Proxy< ^a, ^d> = PROXY2

        /// Map the first argument contravariantly.
        let inline lmap (_: ^b -> ^a) (_: Proxy< ^a, ^c>) : Proxy< ^b, ^c> = PROXY2

        /// Map the second argument covariantly.
        let inline rmap (_: ^b -> ^c) (_: Proxy< ^a, ^b>) : Proxy< ^a, ^c> = PROXY2


    /// Supplementary Comonad operations on the given type.
    module Comonad =

        /// Retrieve a value out of a context.
        let inline extract (_: Proxy< ^a>) : ^a = (^a: (static member Empty: unit -> ^a) ())

        /// Sequentially compose two co-effects.
        let inline extend (_: Proxy< ^a> -> ^b) (_: Proxy< ^a>) : Proxy< ^b> = PROXY

        /// Takes a comonadic container and produces a container of containers.
        let inline duplicate (_: Proxy< ^a>) : Proxy<Proxy< ^a>> = PROXY

        /// Composes two comonadic functions together. Acts as the composition function in the CoKleisli category.
        let inline composeW (f2: Proxy< ^b> -> ^c) f1 : Proxy< ^a> -> ^c = extend f1 >> f2

        /// Sequentially compose two co-actions, creating a third from the result and
        /// lifting a binary function on its effects.
        let inline extendMap (_: Proxy< ^a> -> ^b) (_: ^a -> ^b -> ^c) (_: Proxy< ^a>) : Proxy< ^c> = PROXY

        /// Deconstructs a comonad through recursive (effectful) computations.
        /// Computation proceeds through the use of a continuation function.
        let inline recW (_: (Proxy< ^a> -> ^a) -> Proxy< ^a> -> ^a) (_: Proxy< ^a>) : ^a =
            (^a: (static member Empty: unit -> ^a) ())


    /// Types with a binary, associative composition operation.
    module Semigroup =

        /// An associative composition operation.
        let inline sappend (e1: Proxy< ^a>) (e2: Proxy< ^a>) : Proxy< ^a> = PROXY


    /// Types with a binary, associative composition operation and an identity element.
    module Monoid =

        /// An associative composition operation.
        let inline mappend (e1: Proxy< ^a>) (e2: Proxy< ^a>) : Proxy< ^a> = PROXY

        /// The identity element for the composition operator.
        let inline mempty<'a> : Proxy< ^a> = PROXY

        /// Repeat a value 'n' times.
        let inline mtimes (_: uint32) (_: Proxy< ^a>) : Proxy< ^a> = PROXY

        /// <summary>Combine elements of a sequence using monoidal composition.</summary>
        /// <exception cref="System.ArgumentNullException"> Thrown when the input sequence is null.</exception>
        let inline mconcat (_: Proxy< ^a> seq) : Proxy< ^a> = PROXY


    /// Category -- includes an identity element and an associative composition function.
    module Cat =

        /// Identity element of a category.
        let inline identity<'a> : Proxy< ^a, ^a> = PROXY2

        /// Compose two members of a category together.
        let inline compose (cb: Proxy< ^b, ^c>) (ca: Proxy< ^a, ^b>) : Proxy< ^a, ^c> = PROXY2


    /// Arrows are a general, abstract view of computation. In particular, they allow
    /// notions of computation that may be independent of the input or may take multiple inputs.
    module Arrow =

        /// Lift a function to an arrow.
        let inline arr (_: ^a -> ^b) : Proxy< ^a, ^b> = PROXY2

        /// Send the first component of the input through the argument arrow,
        /// and copy the rest unchanged to the output.
        let inline arrFst (_: Proxy< ^a, ^b>) : Proxy< ^a * ^c, ^b * ^c> = PROXY2

        /// Send the second component of the input through the argument arrow,
        /// and copy the rest unchanged to the output.
        let inline arrSnd (_: Proxy< ^a, ^b>) : Proxy< ^c * ^a, ^c * ^b> = PROXY2

        /// Split the input between the two argument arrows and combine their output.
        let inline split (a2: Proxy< ^c, ^d>) (a1: Proxy< ^a, ^b>) : Proxy< ^a * ^c, ^b * ^d> = PROXY2

        /// Fanout: send the input to both argument arrows and combine their output.
        let inline fanout (a2: Proxy< ^c, ^a>) (a1: Proxy< ^a, ^b>) : Proxy< ^a, ^b * ^c> = PROXY2


        /// Choice, for arrows that support it. This class underlies the
        /// 'if' and 'case' constructs in arrow notation.
        module Choice =                

            /// Feed marked inputs through the argument arrow, passing the
            /// rest through unchanged to the output. A mirror of 'feed2'.
            let inline feed1 (_: Proxy< ^a, ^b>) : Proxy<Choice< ^a, ^c>, Choice< ^b, ^c>> = PROXY2
            
            /// Feed marked inputs through the argument arrow, passing the
            /// rest through unchanged to the output. A mirror of 'feed1'.
            let inline feed2 (_: Proxy< ^a, ^b>) : Proxy<Choice< ^c, ^a>, Choice< ^c, ^b>> = PROXY2

            /// Split the input between the two argument arrows, retagging
            /// and merging their outputs.
            let inline merge (a2: Proxy< ^c, ^d>)
                (a1: Proxy< ^a, ^b>) : Proxy<Choice< ^a, ^c>, Choice< ^b, ^d>> = PROXY2

            /// Split the input between the two argument arrows and merge their outputs.
            let inline fanin (a2: Proxy< ^c, ^b>) (a1: Proxy< ^a, ^b>) : Proxy<Choice< ^a, ^c>, ^b> = PROXY2


        /// Arrows that allow application of arrow inputs to other inputs.
        module Apply =

            /// Arrow that allows application of arrow inputs to other inputs.
            let inline app<'a, 'b> : Proxy<Proxy<'a, 'b> * 'a, 'b> = PROXY2



open Composition

//  @ Operators @
type Proxy<'a> with

// @ Cat @

    /// Compose two members of a category together.
    static member inline ( >>> ) (ca, cb) = Cat.compose cb ca
    /// Compose two members of a category together.
    static member inline ( <<< ) (cb, ca) = Cat.compose cb ca

    /// Identity of the category.
    static member inline Id () : Proxy< ^a, ^a> = Cat.identity

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

// @ Applicative.Alternative @

    /// An associative binary operation on applicative functors.
    static member inline ( <|> ) (c1, c2) = Applicative.Alternative.orElse c2 c1
    /// An associative binary operation on applicative functors.
    static member inline ( <||> ) (c2, c1) = Applicative.Alternative.orElse c2 c1

// @ Functor @

    /// Lift a function onto effects.
    static member inline ( |>> ) (m, f) = map f m
    /// Lift a function onto effects.
    static member inline ( <<| ) (f, m) = map f m

    /// Replace all locations in the input with the same value.
    static member inline ( &> ) (b, fx) = Functor.replace b fx
    /// Replace all locations in the input with the same value.
    static member inline ( <& ) (fx, b) = Functor.replace b fx

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

// @ Monoid @

    static member inline Empty () : Proxy< ^a> = Monoid.mempty