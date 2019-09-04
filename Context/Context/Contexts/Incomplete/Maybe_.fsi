namespace PTR.Context.Type.Incomplete


///// <summary>The Maybe type represents values which may or may not exist.</summary>
//[<Struct>]
//type Maybe<'T> = Nothing | Just of ^T


///// <summary>Operations on Maybes.</summary>
//module Maybe =

//    /// <summary>Return true if the given value is a Just-value, false otherwise.</summary>
//    [<CompiledName("IsJust")>]
//    val isJust: maybe: Maybe<'a> -> bool

//    /// <summary>Return true if the given value is a Nothing-value, false otherwise.</summary>
//    [<CompiledName("IsNothing")>]
//    val isNothing: maybe: Maybe<'a> -> bool

//    /// <summary>Takes a default value, a function, and a Maybe value.
//    /// If the Maybe value is Nothing, the function returns the default value.
//    /// Otherwise, it applies the function to the value inside the Just and returns the result.</summary>
//    [<CompiledName("OfMaybe")>]
//    val inline ofMaybe: def: ^b -> f: (^a -> ^b) -> maybe: Maybe< ^a> -> ^b

//    /// <summary>Return the contents of a Just-value or a default value otherwise.</summary>
//    [<CompiledName("FromMaybe")>]
//    val fromMaybe: def: 'a -> maybe: Maybe<'a> -> 'a

//    /// <summary>The catMaybes function takes a sequence of Maybes and returns a list of all the Just values.</summary>
//    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//    [<CompiledName("CatMaybes")>]
//    val catMaybes: maybes: Maybe<'a> seq -> ^a seq

//    /// <summary>Version of Seq.map that can throw out elements. If the result of 'f' is a Just value, the result is included, otherwise it is excluded.</summary>
//    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//    [<CompiledName("MapMaybes")>]
//    val inline mapMaybes: f: (^a -> Maybe< ^b>) -> source: ^a seq -> ^b seq

//    /// <summary>'Adds' a value/parameter to a Maybe-value by converting Just-values into Choice1Of2's,
//    /// otherwise puts a default 'note' value into a Choice2Of2.</summary>
//    [<CompiledName("Note")>]
//    val note: msg: 'b -> maybe: Maybe<'a> -> Choice< ^a, ^b>


//    /// <summary>Conversions between Maybes and related types.</summary>
//    module Convert =

//        /// <summary>Returns a Just-value with the head of a non-empty sequence and returns Nothing if the sequence is empty.</summary>
//        [<CompiledName("OfSeq")>]
//        val ofSeq: source: 'a seq -> Maybe< ^a>

//        /// <summary>Returns a singleton sequence if thet value is a Just-value, an empty sequence otherwise.</summary>
//        [<CompiledName("ToSeq")>]
//        val toSeq: maybe: Maybe<'a> -> ^a seq

//        /// <summary>Convert a .NET Option-type to a Maybe-type. None => Nothing and Some a => Just a.</summary>
//        [<CompiledName("OfOption")>]
//        val ofOption: option: Option<'a> -> Maybe< ^a>

//        /// <summary>Convert a Maybe-type to a .NET Option-type. Nothing => None and Just a => Some a.</summary>
//        [<CompiledName("ToOption")>]
//        val toOption: maybe: Maybe<'a> -> Option< ^a>

//        /// <summary>Create a Just-value if the given object is not null, Nothing otherwise.</summary>
//        [<CompiledName("OfObj")>]
//        val ofObj: o: 'a -> Maybe<'a> when 'a: null

//        /// <summary>Create a Just-value if the given object has a value, Nothing otherwise.</summary>
//        [<CompiledName("OfNullable")>]
//        val ofNullable: n: System.Nullable<'a> -> Maybe< ^a>


//    /// <summary>Compositional operations on Maybe values.</summary>
//    module Compose =

//        /// <summary>Supplementary Monad operations on the given type.</summary>
//        module Monad =

//            /// <summary>Lift a value onto an effectful context.</summary>
//            [<CompiledName("Wrap")>]
//            val wrap: x: 'a -> Maybe< ^a>

//            /// <summary>Sequentially compose two effects, passing any value produced by the first as an argument to the second.</summary>
//            [<CompiledName("Bind")>]
//            val inline bind: k: (^a -> Maybe< ^b>) -> m: Maybe< ^a> -> Maybe< ^b>

//            /// <summary>Removes one layer of monadic context from a nested monad.</summary>
//            [<CompiledName("Flatten")>]
//            val flatten: mm: Maybe<Maybe<'a>> -> Maybe< ^a>


//            /// <summary>Monadic computation builder specialised to the given monad.</summary>
//            [<Sealed>]
//            type MaybeBuilder =
//                new: unit -> MaybeBuilder
//                member inline Bind: m: Maybe< ^a> * k: (^a -> Maybe< ^b>) -> Maybe< ^b>
//                member inline Return: x: ^a -> Maybe< ^a>
//                member inline ReturnFrom: m: Maybe< ^a> -> Maybe< ^a>
//                member inline Zero: unit -> Maybe<unit>

//                member inline TryWith: body: (unit -> Maybe< ^a>) * handler: (exn -> Maybe< ^a>) -> Maybe< ^a>
//                member inline TryFinally: body: (unit -> Maybe< ^a>) * finalizer: (unit -> unit) -> Maybe< ^a>

//                member inline Using: disp: ^d * body: (^d -> Maybe< ^a>) -> Maybe< ^a> when ^d :> System.IDisposable

//                member inline While: guard: (unit -> bool) * body: (unit -> Maybe<unit>) -> Maybe<unit>

//                member inline For: seq: ^a seq * body: (^a -> Maybe<unit>) -> Maybe<unit>


//            /// <summary>Build a monad through recursive (effectful) computations.
//            /// Computation proceeds through the use of a continuation function applied to the intermediate result.
//            /// The default monadic 'identity' function is used in each iteration where the continuation is applied.</summary>
//            [<CompiledName("RecM")>]
//            val inline recM: f: ((^a -> Maybe< ^b>) -> ^a -> Maybe< ^b>) -> x: ^a -> Maybe< ^b>

//            /// <summary>Build a monad through recursive (effectful) computations.
//            /// Computation proceeds through the use of a continuation function applied to an 'effect' applied over the intermediate result.
//            /// Any constructor can be used in each iteration, in the case of union-types.</summary>
//            [<CompiledName("RecMp")>]
//            val inline recMp: f: ((Maybe< ^a> -> Maybe< ^a>) -> ^a -> Maybe< ^a>) -> x: ^a -> Maybe< ^a>

//            /// <summary>Monadic fold over a structure associating to the right.</summary>
//            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//            [<CompiledName("FoldrM")>]
//            val inline foldrM: f: (^a -> ^s -> Maybe< ^s>) -> s0: ^s -> source: ^a seq -> Maybe< ^s>

//            /// <summary>Monadic fold over a structure associating to the left.</summary>
//            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//            [<CompiledName("FoldlM")>]
//            val inline foldlM: f: (^s -> ^a -> Maybe< ^s>) -> s0: ^s -> source: ^a seq -> Maybe< ^s>


//            /// <summary>Monads that also support choice and failure.</summary>
//            module Plus =

//                /// <summary>The identity of mplus.</summary>
//                [<CompiledName("MZero")>]
//                val mzero<'a> : Maybe< ^a>

//                /// <summary>A monoidal operation on monads, supporting choice and failure.</summary>
//                [<CompiledName("MPlus")>]
//                val mplus: m1: Maybe<'a> -> m2: Maybe<'a> -> Maybe< ^a>

//                /// <summary>Conditional blocking of effectful computations.</summary>
//                [<CompiledName("Guard")>]
//                val guard: condition: bool -> Maybe<unit>

//                /// <summary>Create a new item if the previous was mzero, else keep the original.</summary>
//                [<CompiledName("Recover")>]
//                val inline recover: makeNew: (unit -> Maybe< ^a>) -> m: Maybe< ^a> -> Maybe< ^a>

//                /// <summary>Combine two monads using a 'SQL style' inner join function.</summary>
//                [<CompiledName("Relate")>]
//                val inline relate: f: (^a -> ^b -> ^c) -> k1: (^a -> ^k) -> k2: (^b -> ^k) -> ma: Maybe< ^a> -> mb: Maybe< ^b> -> Maybe< ^c> when ^k : equality


//                /// <summary>Generalizations of functions on other types.</summary>
//                module General =

//                    /// <summary>Generalizes list concatenation to monads.</summary>
//                    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//                    [<CompiledName("MSum")>]
//                    val inline msum: source: ^a Maybe seq -> Maybe< ^a>
//                        // Seq.foldBack mplus source mzero

//                    /// <summary>Generalizes the 'ofSeq' function.</summary>
//                    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//                    [<CompiledName("MOfSeq")>]
//                    val mOfSeq: source: 'a seq -> Maybe< ^a>
//                        // msum (Seq.map wrap xs)

//                    /// <summary>Generalizes the 'Seq.where' function.</summary>
//                    [<CompiledName("MWhere")>]
//                    val inline mwhere: p: (^a -> bool) -> m: Maybe< ^a> -> Maybe< ^a>
//                        // bind (fun a -> if p a then m else mzero) m

//                    /// <summary>Opposite of the 'mwhere' function.</summary>
//                    [<CompiledName("MRemove")>]
//                    val inline mremove: p: (^a -> bool) -> m: Maybe< ^a> -> Maybe< ^a>
//                        // mwhere (fun x -> not (p x)) m

//                    /// <summary>Generalizes the 'Seq.partition' function.</summary>
//                    [<CompiledName("MPartition")>]
//                    val inline mpartition: p: (^a -> bool) -> m: Maybe< ^a> -> Maybe< ^a> * Maybe< ^a>
//                        // mwhere p m, mremove p m

//                    /// <summary>Translate a form of Option.defaultWith to an arbitrary 'MonadPlus' type.</summary>
//                    [<CompiledName("MOfOption")>]
//                    val mofOption: m: Option<'a> -> Maybe< ^a>
//                        // let ofOption b f m = match m with None -> b | Some a -> f a
//                        // ofOption mzero wrap m

//                    /// <summary>Translate a form of 'Option.defaultWith' to an arbitrary 'MonadPlus' type.</summary>
//                    [<CompiledName("MConcatOption")>]
//                    val mconcatOption: m: 'a option Maybe -> Maybe< ^a>
//                        // let ofOption b f m = match m with None -> b | Some a -> f a
//                        // bind (ofOption mzero wrap) m

//                    /// <summary>Generalizes the 'Seq.choose' function.</summary>
//                    [<CompiledName("MChoose")>]
//                    val inline mchoose: f: (^a -> ^b option) -> m: Maybe< ^a> -> Maybe< ^b>
//                        // bind (fun a -> match f a with None -> Nothing | Some a -> Just a)

//                    /// <summary>Collects the values from Choice1Of2's, while discarding the rest.</summary>
//                    [<CompiledName("MChoice1")>]
//                    val mchoice1: m: Maybe<Choice<'a, '``_``>> -> Maybe< ^a>
//                        // let l = function Choice1Of2 a -> Some a | Choice2Of2 _ -> None
//                        // mconcatOption (map l m)

//                    /// <summary>Collects the values from Choice2Of2's, while discarding the rest.</summary>
//                    [<CompiledName("MChoice2")>]
//                    val mchoice2: m: Maybe<Choice<'``_``, 'a>> -> Maybe< ^a>
//                        // let r = function Choice2Of2 a -> Some a | Choice1Of2 _ -> None
//                        // mconcatOption (map r m)

//                    /// <summary>Collects the values from Choice1Of2s on the left, and from Choice2Of2s on the right.</summary>
//                    [<CompiledName("MPartitionChoice")>]
//                    val mpartitionChoice: m: Maybe<Choice<'a, 'b>> -> Maybe< ^a> * Maybe< ^b> // mchoice1 m, mchoice2 m


//        /// <summary>Supplementary Applicative operations on the given type.</summary>
//        module Applicative =

//            /// <summary>Lift a value onto an effectful context.</summary>
//            [<CompiledName("Wrap")>]
//            val inline wrap: x: ^a -> Maybe< ^a>

//            /// <summary>Sequential application on effects.</summary>
//            [<CompiledName("Ap")>]
//            val inline ap: fv: Maybe< ^a> -> ff: Maybe<(^a -> ^b)> -> Maybe< ^b>

//            /// <summary>Lift a binary function on effects.</summary>
//            [<CompiledName("Map2")>]
//            val inline map2: f: (^a -> ^b -> ^c) -> fa: Maybe< ^a> -> fb: Maybe< ^b> -> Maybe< ^c>

//            /// <summary>Lift a ternary function on effects.</summary>
//            [<CompiledName("Map3")>]
//            val inline map3: f: (^a -> ^b -> ^c -> ^d) -> fa: Maybe< ^a> -> fb: Maybe< ^b> -> fc: Maybe< ^c> -> Maybe< ^d>

//            /// <summary>Sequentially compose two effects, discarding any value produced by the first.</summary>
//            [<CompiledName("AndThen")>]
//            val andThen: fb: Maybe<'b> -> fa: Maybe<'a> -> Maybe< ^b>

//            /// <summary>Conditional execution of effectful expressions.</summary>
//            [<CompiledName("When")>]
//            val inline when_: condition: bool -> f: (unit -> Maybe<unit>) -> Maybe<unit>

//            /// <summary>Generalizes the sequence-based filter function.</summary>
//            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//            [<CompiledName("FilterA")>]
//            val inline filterA: p: (^a -> Maybe<bool>) -> source: ^a seq -> Maybe< ^a seq>

//            /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
//            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//            [<CompiledName("SequenceA")>]
//            val inline sequenceA: source: ^a Maybe seq -> Maybe< ^a seq>

//            /// <summary>Produce an effect for the elements in the sequence from left to right then evaluate each effect, and collect the results.</summary>
//            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//            [<CompiledName("ForA")>]
//            val inline forA: f: (^a -> Maybe< ^b>) -> source: ^a seq -> Maybe< ^b seq>

//            /// <summary>Produce an effect for each pair of elements in the sequences from left to right, then evaluate each effect and collect the results.
//            /// If one sequence is longer, its extra elements are ignored.</summary>
//            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//            [<CompiledName("ZipWithA")>]
//            val inline zipWithA: f: (^a -> ^b -> Maybe< ^c>) -> source1: ^a seq -> source2: ^b seq -> Maybe< ^c seq>

//            /// <summary>Performs the effect 'n' times.</summary>
//            [<CompiledName("ReplicateA")>]
//            val replicateA: count: int -> fa: Maybe<'a> -> Maybe< ^a seq>


//            /// <summary>A monoid on applicative functors.</summary>
//            module Alternative =

//                /// <summary>The identity of orElse.</summary>
//                [<CompiledName("Empty")>]
//                val empty<'a> : Maybe< ^a>

//                /// <summary>An associative binary operation on applicative functors.</summary>
//                [<CompiledName("OrElse")>]
//                val orElse: choice2: Maybe<'a> -> choice1: Maybe<'a> -> Maybe< ^a>

//                /// <summary>The sum of a collection of effects.</summary>
//                /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//                [<CompiledName("ASum")>]
//                val inline asum: t_fa: ^a Maybe seq -> Maybe< ^a>

//                /// <summary>Return one or none results on effects.</summary>
//                [<CompiledName("Optional")>]
//                val optional: fa: Maybe<'a> -> Maybe<Option< ^a>>
//                    // orElse (wrap None) (map Some fa)

//                /// <summary>Create a new item if the previous was empty, else keep the original.</summary>
//                [<CompiledName("Alt")>]
//                val inline alt: def: (unit -> Maybe< ^a>) -> fa: Maybe< ^a> -> Maybe< ^a>


//        /// <summary>Supplementary Functor operations on the given type.</summary>
//        module Functor =

//            /// <summary>Lift a function onto effects.</summary>
//            [<CompiledName("Map")>]
//            val inline map: f: (^a -> ^b) -> m: Maybe< ^a> -> Maybe< ^b>

//            /// <summary>Replace all locations in the input with the same value.</summary>
//            [<CompiledName("Replace")>]
//            val replace: b: 'b -> fa: Maybe<'a> -> Maybe< ^b>

//            /// <summary>Perform an operation, store its result, perform an action using both the input and output, and finally return the output.</summary>
//            [<CompiledName("Tee")>]
//            val inline tee: f: (^a -> ^b) -> g: (^a -> ^b -> unit) -> fa: Maybe< ^a> -> Maybe< ^b>


//        /// <summary>Types with a binary, associative composition operation.</summary>
//        module Semigroup =

//            /// <summary>An associative composition operation.</summary>
//            [<CompiledName("SAppend")>]
//            val inline sappend: e1: Maybe< ^a> -> e2: Maybe< ^a> -> Maybe< ^a>
//                when ^a: (static member Append: ^a -> ^a -> ^a)


//        /// <summary>Types with a binary, associative composition operation and an identity element.</summary>
//        module Monoid =

//            /// <summary>An associative composition operation.</summary>
//            [<CompiledName("MAppend")>]
//            val inline mappend: e1: Maybe< ^a> -> e2: Maybe< ^a> -> Maybe< ^a>
//                when ^a: (static member Append: ^a -> ^a -> ^a)

//            /// <summary>The identity element for the composition operator.</summary>
//            [<CompiledName("MEmpty")>]
//            val mempty<'a> : Maybe< ^a>

//            /// <summary>Repeat a value 'n' times.</summary>
//            [<CompiledName("MTimes")>]
//            val inline mtimes: n: int -> e: Maybe< ^a> -> Maybe< ^a>
//                when ^a: (static member Append: ^a -> ^a -> ^a)

//            /// <summary>Combine elements of a sequence using monoidal composition.</summary>
//            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//            [<CompiledName("MConcat")>]
//            val inline mconcat: source: Maybe< ^a> seq -> Maybe< ^a>
//                when ^a: (static member Append: ^a -> ^a -> ^a)


//    /// <summary>Creates a computation expression for the given type.</summary>
//    val maybe: Compose.Monad.MaybeBuilder



//    //open Maybe
//    //open Compose

//    //// @ Operators @
//    //type Maybe<'T> with

//    //// @ Primitive @

//    ///// <summary>Return the contents of a Just-value or a default value otherwise.</summary>
//    //static member inline ( >- ) (m, d) = fromMaybe d m
//    ///// <summary>Return the contents of a Just-value or a default value otherwise.</summary>
//    //static member inline ( -< ) (d, m) = fromMaybe d m

//    //// @ Monad @

//    ///// <summary>Sequentially compose two effects, passing any value produced by the first as an argument to the second.</summary>
//    //static member inline ( >>= ) (m, k) = Monad.bind k m
//    ///// <summary>Sequentially compose two effects, passing any value produced by the first as an argument to the second.</summary>
//    //static member inline ( =<< ) (k, m) = Monad.bind k m

//    //// @ Applicative @

//    ///// <summary>Sequential application on effects.</summary>
//    //static member inline ( <*> ) (ff, fx) = Applicative.ap fx ff
//    ///// <summary>Sequential application on effects.</summary>
//    //static member inline ( <**> ) (fx, ff) = Applicative.ap fx ff

//    ///// <summary>Sequentially compose two effects, discarding any value produced by the first.</summary>
//    //static member inline ( *> ) (fa, fb) = Applicative.andThen fb fa
//    ///// <summary>Sequentially compose two effects, discarding any value produced by the first.</summary>
//    //static member inline ( <* ) (fb, fa) = Applicative.andThen fb fa

//    //// @ Applicative.Alternative @

//    ///// <summary>An associative binary operation on applicative functors.</summary>
//    //static member inline ( <|> ) (c1, c2) = Applicative.Alternative.orElse c2 c1
//    ///// <summary>An associative binary operation on applicative functors.</summary>
//    //static member inline ( <||> ) (c2, c1) = Applicative.Alternative.orElse c2 c1

//    //// @ Functor @

//    ///// <summary>Lift a function onto effects.</summary>
//    //static member inline ( |%> ) (fa, f) = Functor.map f fa
//    ///// <summary>Lift a function onto effects.</summary>
//    //static member inline ( <%| ) (f, fa) = Functor.map f fa

//    ///// <summary>Replace all locations in the input with the same value.</summary>
//    //static member inline ( %> ) (fa, b) = Functor.replace b fa
//    ///// <summary>Replace all locations in the input with the same value.</summary>
//    //static member inline ( <% ) (b, fa) = Functor.replace b fa

//    //// @ Semigroup @

//    ///// <summary>An associative composition operation.</summary>
//    //static member inline Append (e1, e2) = Semigroup.sappend e1 e2

//    //// @ Monoid @

//    ///// <summary>The identity element for the composition operator.</summary>
//    //static member inline Empty () : ^a Maybe = Nothing