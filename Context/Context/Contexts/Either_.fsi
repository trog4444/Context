namespace PTR.Context.Type.Either


/// <summary>The Either type represents values with two possibilities:
/// a value of type Either<A, B> is either `Left A` or `Right B`.</summary>
[<Struct>]
type Either<'A, 'B> = Left of L: 'A | Right of R: 'B with
    member inline Match : fLeft: System.Func< ^A, ^C> * fRight: System.Func< ^B ,^C> -> ^C


/// <summary>Operations on Eithers.</summary>
module Either =

    /// <summary>Return True if the given value is a Left-value, False otherwise.</summary>
    [<CompiledName("IsLeft")>]
    val isLeft: either: Either<'a, 'b> -> bool

    /// <summary>Return True if the given value is a Right-value, False otherwise.</summary>
    [<CompiledName("IsRight")>]
    val isRight: either: Either<'a, 'b> -> bool

    /// <summary>Apply one of the given functions to the value held within an Either, depending on if the value is 'Left' or 'Right.'</summary>
    [<CompiledName("OfEither")>]
    val inline ofEither: fLeft: (^a -> ^c) -> fRight: (^b -> ^c) -> either: Either< ^a, ^b> -> ^c

    /// <summary>Return the contents of a Left-value or a default value otherwise.</summary>
    [<CompiledName("FromLeft")>]
    val fromLeft: def: 'a -> either: Either< ^a, 'b> -> ^a

    /// <summary>Return the contents of a Right-value or a default value otherwise.</summary>
    [<CompiledName("FromRight")>]
    val fromRight: def: 'b -> either: Either<'a, ^b> -> ^b

    /// <summary>Removes the Left-value type from an Either, carrying Right-value(s) into a Some; None otherwise.</summary>
    [<CompiledName("Hush")>]
    val hush: either: Either<'``_``, 'a> -> ^a option
     
        
    /// <summary>Conversions between Eithers and related types.</summary>
    module Convert =
        
        /// <summary>Returns a Right-value with the head of a non-empty sequence and returns a Left-value if the sequence is empty. The Left-value will say if the input was either null or empty.</summary>
        [<CompiledName("OfSeq")>]
        val ofSeq: source: 'a seq -> Either<string, ^a>
    
        /// <summary>Returns a singleton sequence if thet value is a Right-value, an empty sequence otherwise.</summary>
        [<CompiledName("ToSeq")>]
        val toSeq: either: Either<'a, 'b> -> ^b seq

        /// <summary>Convert a Choice (Of2) to an Either.</summary>
        [<CompiledName("OfChoice")>]
        val ofChoice: choice: Choice<'b, 'a> -> Either< ^a, ^b>

        /// <summary>Convert an Either to a Choice (Of2).</summary>
        [<CompiledName("ToChoice")>]
        val toChoice: either: Either<'a, 'b> -> Choice< ^b, ^a>

        /// <summary>Convert a Result to an Either.</summary>
        [<CompiledName("OfResult")>]
        val ofResult: result: Result<'b, 'err> -> Either< ^err, ^b>

        /// <summary>Convert an Either to a Result.</summary>
        [<CompiledName("ToResult")>]
        val toResult: either: Either<'err, 'b> -> Result< ^b, ^err>


    /// <summary>Compositional operations on Either values.</summary>
    module Compose =

        /// <summary>Supplementary Monad operations on the given type.</summary>
        module Monad =

            /// <summary>Lift a value onto an effectful context.</summary>
            [<CompiledName("Wrap")>]
            val inline wrap: x: ^b -> Either< ^a, ^b>

            /// <summary>Sequentially compose two effects, passing any value produced by the first as an argument to the second.</summary>
            [<CompiledName("Bind")>]
            val inline bind: k: (^b -> Either< ^a, ^c>) -> m: Either< ^a, ^b> -> Either< ^a, ^c>

            /// <summary>Removes one layer of monadic context from a nested monad.</summary>
            [<CompiledName("Flatten")>]
            val flatten: mm: Either<'a, Either< ^a, 'b>> -> Either< ^a, ^b>


            /// <summary>Monadic computation builder specialised to the given monad.</summary>
            type EitherBuilder =
                new: unit -> EitherBuilder
                member inline Bind: m: Either< ^a, ^b> * k: (^b -> Either< ^a, ^c>) -> Either< ^a, ^c>
                member inline Return: x: ^b -> Either< ^a, ^b>
                member inline ReturnFrom: m: Either< ^a, ^b> -> Either< ^a, ^b>
                member inline Zero: unit -> Either< ^a, unit>

                member inline TryWith: body: (unit -> Either< ^a, ^b>) * handler: (exn -> Either< ^a, ^b>) -> Either< ^a, ^b>
                member inline TryFinally: body: (unit -> Either< ^a, ^b>) * finalizer: (unit -> unit) -> Either< ^a, ^b>

                member inline Using: disp: ^d * body: (^d -> Either< ^a, ^b>) -> Either< ^a, ^b> when ^d :> System.IDisposable

                member inline While: guard: (unit -> bool) * body: (unit -> Either< ^a, unit>) -> Either< ^a, unit>

                member inline For: seq: #seq< ^b> * body: (^b -> Either< ^a, unit>) -> Either< ^a, unit>


            /// <summary>Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to the intermediate result.
            /// The default monadic 'identity' function is used in each iteration where the continuation is applied.</summary>
            [<CompiledName("RecM")>]
            val inline recM: f: ((^b -> Either< ^a, ^c>) -> ^b -> Either< ^a, ^c>) -> x: ^b -> Either< ^a, ^c>

            /// <summary>Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to an 'effect' applied over the intermediate result.
            /// Any constructor can be used in each iteration, in the case of union-types.</summary>
            [<CompiledName("RecM1")>]
            val inline recM1 : f: ((Either< ^a, ^b> -> Either< ^a, ^c>) -> ^b -> Either< ^a, ^c>) -> x: ^b -> Either< ^a, ^c>

            /// <summary>Monadic fold over a structure associating to the right.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            [<CompiledName("FoldrM")>]
            val inline foldrM: f: (^b -> ^s -> Either< ^a, ^s>) -> s0: ^s -> source: ^b seq -> Either< ^a, ^s>

            /// <summary>Monadic fold over a structure associating to the left.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            [<CompiledName("FoldlM")>]
            val inline foldlM: f: (^s -> ^b -> Either< ^a, ^s>) -> s0: ^s -> source: ^b seq -> Either< ^a, ^s>


        /// <summary>Supplementary Applicative operations on the given type.</summary>
        module Applicative =

            /// <summary>Lift a value onto an effectful context.</summary>
            [<CompiledName("Wrap")>]
            val inline wrap: x: ^b -> Either< ^a, ^b>

            /// <summary>Sequential application on effects.</summary>
            [<CompiledName("Ap")>]
            val inline ap: fv: Either< ^a, ^b> -> ff: Either< ^a, (^b -> ^c)> -> Either< ^a, ^c>

            /// <summary>Lift a binary function on effects.</summary>
            [<CompiledName("Map2")>]
            val inline map2: f: (^b -> ^c -> ^d) -> fb: Either< ^a, ^b> -> fc: Either< ^a, ^c> -> Either< ^a, ^d>

            /// <summary>Lift a ternary function on effects.</summary>
            [<CompiledName("Map3")>]
            val inline map3: f: (^b -> ^c -> ^d -> ^e) -> fb: Either< ^a, ^b> -> fc: Either< ^a, ^c> -> fd: Either< ^a, ^d> -> Either< ^a, ^e>

            /// <summary>Sequentially compose two effects, discarding any value produced by the first.</summary>
            [<CompiledName("AndThen")>]
            val andThen: fc: Either<'a, 'c> -> fb: Either<'a, 'b> -> Either< ^a, ^c>

            /// <summary>Conditional execution of effectful expressions.</summary>
            [<CompiledName("When")>]
            val inline when_: condition: bool -> f: (unit -> Either< ^a, unit>) -> Either< ^a, unit>

            /// <summary>Generalizes the sequence-based filter function.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            [<CompiledName("FilterA")>]
            val inline filterA: p: (^b -> Either< ^a, bool>) -> source: ^b seq -> Either< ^a, ^b seq>

            /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            [<CompiledName("SequenceA")>]
            val inline sequenceA: source: Either< ^a, ^b> seq -> Either< ^a, ^b seq>

            /// <summary>Produce an effect for the elements in the sequence from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            [<CompiledName("ForA")>]
            val inline forA: f: (^b -> Either< ^a, ^c>) -> source: #seq< ^b> -> Either< ^a, ^c seq>

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right, then evaluate each effect and collect the results. If one sequence is longer, its extra elements are ignored.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            [<CompiledName("ZipWithA")>]
            val inline zipWithA: f: (^b -> ^c -> Either< ^a, ^d>) -> source1: #seq< ^b> -> source2: #seq< ^c> -> Either< ^a, ^d seq>

            /// <summary>Performs the effect 'n' times.</summary>
            [<CompiledName("ReplicateA")>]
            val replicateA: count: int -> fb: Either<'a, 'b> -> Either< ^a, ^b seq>


        /// <summary>Supplementary Functor operations on the given type.</summary>
        module Functor =

            /// <summary>Lift a function onto effects.</summary>
            [<CompiledName("Map")>]
            val inline map: f: (^b -> ^c) -> fb: Either< ^a, ^b> -> Either< ^a, ^c>

            /// <summary>Replace all locations in the input with the same value.</summary>
            [<CompiledName("Replace")>]
            val replace: c: 'c -> fb: Either<'a, 'b> -> Either< ^a, ^c>


        /// <summary>A two paramater functor where both the first and second arguments are covariant.</summary>
        module Bifunctor =

            /// <summary>Map over both arguments at the same time.</summary>
            [<CompiledName("Bimap")>]
            val inline bimap: f: (^a -> ^c) -> g: (^b -> ^d) -> bf: Either< ^a, ^b> -> Either< ^c, ^d>

            /// <summary>Map covariantly over the first argument.</summary>
            [<CompiledName("MapFst")>]
            val inline mapFst: f: (^a -> ^c) -> bf: Either< ^a, ^b> -> Either< ^c, ^b>

            /// <summary>Map covariantly over the second argument.</summary>
            [<CompiledName("MapSnd")>]
            val inline mapSnd: g: (^b -> ^c) -> bf: Either< ^a, ^b> -> Either< ^a, ^c>


        /// <summary>Types with a binary, associative composition operation.</summary>
        module Semigroup =

            /// <summary>An associative composition operation.</summary>
            [<CompiledName("SAppend")>]
            val sappend: e1: Either<'a, 'b> -> e2: Either< ^a, ^b> -> Either< ^a, ^b>


    /// <summary>Creates a computation expression for the given type.</summary>
    val either : Compose.Monad.EitherBuilder