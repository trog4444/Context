namespace PTR.Context.Type


/// A "continuation-passing style" (CPS) computation that uses an intermediate
/// value of type `T` within a CPS computation to produce a final result of type `R`.</summary>
[<Struct; NoComparison; NoEquality>]
type Cont<'R, 'T> = Cont of ((^T -> ^R) -> ^R)


/// <summary>Operations on Conts.</summary>
module Cont =

    /// <summary>The result of running a CPS computation with a given final continuation.</summary>
    [<CompiledName("RunCont")>]
    val inline runCont: k: (^a -> ^r) -> Cont< ^r, ^a> -> ^r

    /// <summary>The result of running a CPS computation with the identity as the final continuation.</summary>
    [<CompiledName("EvalCont")>]
    val evalCont: Cont<'r, ^r> -> ^r

    /// <summary>Apply a function to transform the result of a continuation-passing computation.</summary>
    [<CompiledName("MapCont")>]
    val inline mapCont: f: (^r -> ^r) -> Cont< ^r, ^a> -> Cont< ^r, ^a>

    /// <summary>Apply a function to transform the continuation passed to a CPS computation.</summary>
    [<CompiledName("WithCont")>]
    val inline withCont: f: ((^b -> ^r) -> ^a -> ^r) -> Cont< ^r, ^a> -> Cont< ^r, ^b>

    /// <summary>End the current continuation chain with a specific value.</summary>
    [<CompiledName("Exit")>]
    val exit: x: 'r -> Cont< ^r, '``_``>

    /// <summary>shift 'f' captures the continuation up to the nearest enclosing 'reset' and passes it to 'f'.</summary>
    [<CompiledName("Shift")>]
    val inline shift: f: ((^a -> ^r) -> Cont< ^r, ^r>) -> Cont< ^r, ^a>

    /// <summary>reset 'm' delimits the continuation of any shift inside 'm'.</summary>
    [<CompiledName("Reset")>]
    val reset: Cont<'r, 'r> -> Cont<'r0, ^r>

    /// <summary>Call with current continuation.</summary>
    [<CompiledName("CallCC")>]
    val inline callCC: f: ((^a -> Cont< ^r, ^``_``>) -> Cont< ^r, ^a>) -> Cont< ^r, ^a>

    /// <summary>Allows looping with a given continuation function and input.</summary>
    [<CompiledName("GetCC")>]
    val inline getCC: x0: ^a -> Cont< ^r, struct (^a * (^a -> Cont< ^r, ^``_``>))>

    /// <summary>Attempt to apply `fOk` on an input and return a continuation. If the function fails,
    /// return a continuation with the result of applying `fErr` to the input and exception.</summary>
    [<CompiledName("TryCC")>]
    val inline tryCC: fOk: (^a -> ^b) -> fErr: (^a -> exn -> ^b) -> input: ^a -> Cont< ^r, ^b>

    /// <summary>Caches the result(s) of a `Cont` computation.</summary>
    [<CompiledName("CacheCont")>]
    val cacheCont: Cont<'r, 'a> -> Cont< ^r, ^a> when 'a : equality


    /// <summary>Compositional operations on Conts.</summary>
    module Compose =

        /// <summary>Supplementary Monad operations on the given type.</summary>
        module Monad =

            /// <summary>Lift a value onto an effectful context.</summary>
            [<CompiledName("Wrap")>]
            val inline wrap: x: ^a -> Cont< ^r, ^a>

            /// <summary>Sequentially compose two effects, passing any value produced by the first as an argument to the second.</summary>
            [<CompiledName("Bind")>]
            val inline bind: k: (^a -> Cont< ^r, ^b>) -> Cont< ^r, ^a> -> Cont< ^r, ^b>

            /// <summary>Removes one layer of monadic context from a nested monad.</summary>
            [<CompiledName("Flatten")>]
            val inline flatten: Cont< ^r, Cont< ^r, ^a>> -> Cont< ^r, ^a>


            /// <summary>Monadic computation builder specialised to the given monad.</summary>
            [<Sealed>]
            type ContBuilder =
                new: unit -> ContBuilder
                member inline Bind: m: Cont< ^r, ^a> * k: (^a -> Cont< ^r, ^b>) -> Cont< ^r, ^b>
                member inline Return: x: ^a -> Cont< ^r, ^a>
                member inline ReturnFrom: m: Cont< ^r, ^a> -> Cont< ^r, ^a>
                member inline Zero: unit -> Cont< ^r, unit>

                member inline TryWith: body: (unit -> Cont< ^r, ^a>) * handler: (exn -> Cont< ^r, ^a>) -> Cont< ^r, ^a>
                member inline TryFinally: body: (unit -> Cont< ^r, ^a>) * finalizer: (unit -> unit) -> Cont< ^r, ^a>

                member inline Using: disp: ^d * body: (^d -> Cont< ^r, ^a>) -> Cont< ^r, ^a> when ^d :> System.IDisposable

                member inline While: guard: (unit -> bool) * body: (unit -> Cont< ^r, unit>) -> Cont< ^r, unit>

                member inline For: seq: ^a seq * body: (^a -> Cont< ^r, unit>) -> Cont< ^r, unit>


            /// <summary>Build a monad through recursive (effectful) computations. Computation proceeds through the use of a continuation function applied to the intermediate result.
            /// The default monadic 'identity' function is used in each iteration where the continuation is applied.</summary>
            [<CompiledName("RecM")>]
            val inline recM: f: ((^a -> Cont< ^r, ^a>) -> ^a -> Cont< ^r, ^a>) -> x: ^a -> Cont< ^r, ^a>

            /// <summary>Monadic fold over a structure associating to the right.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            [<CompiledName("FoldrM")>]
            val inline foldrM: f: (^a -> ^s -> Cont< ^r, ^s>) -> s0: ^s -> source: ^a seq -> Cont< ^r, ^s>

            /// <summary>Monadic fold over a structure associating to the left.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            [<CompiledName("FoldlM")>]
            val inline foldlM: f: (^s -> ^a -> Cont< ^r, ^s>) -> s0: ^s -> source: ^a seq -> Cont< ^r, ^s>


        /// <summary>Supplementary Applicative operations on the given type.</summary>
        module Applicative =

            /// <summary>Lift a value onto an effectful context.</summary>
            [<CompiledName("Wrap")>]
            val inline wrap: x: ^a -> Cont< ^r, ^a>

            /// <summary>Sequential application on effects.</summary>
            [<CompiledName("Ap")>]
            val inline ap: Cont< ^r, ^a> -> Cont< ^r, (^a -> ^b)> -> Cont< ^r, ^b>

            /// <summary>Lift a binary function on effects.</summary>
            [<CompiledName("Map2")>]
            val inline map2: f: (^a -> ^b -> ^c) -> Cont< ^r, ^a> -> Cont< ^r, ^b> -> Cont< ^r, ^c>

            /// <summary>Lift a ternary function on effects.</summary>
            [<CompiledName("Map3")>]
            val inline map3: f: (^a -> ^b -> ^c -> ^d) -> Cont< ^r, ^a> -> Cont< ^r, ^b> -> Cont< ^r, ^c> -> Cont< ^r, ^d>

            /// <summary>Sequentially compose two effects, discarding any value produced by the first.</summary>
            [<CompiledName("AndThen")>]
            val inline andThen: Cont< ^r, ^b> -> Cont< ^r, ^a> -> Cont< ^r, ^b>

            /// <summary>Conditional execution of effectful expressions.</summary>
            [<CompiledName("When")>]
            val inline when_: condition: bool -> f: (unit -> Cont< ^r, unit>) -> Cont< ^r, unit>

            /// <summary>Generalizes the sequence-based filter function.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            [<CompiledName("FilterA")>]
            val inline filterA: p: (^a -> Cont< ^r, bool>) -> source: ^a seq -> Cont< ^r, ^a list>

            /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            [<CompiledName("SequenceA")>]
            val inline sequenceA: source: Cont< ^r, ^a> seq -> Cont< ^r, ^a list>

            /// <summary>Produce an effect for the elements in the sequence from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            [<CompiledName("ForA")>]
            val inline forA: f: (^a -> Cont< ^r, ^b>) -> source: ^a seq -> Cont< ^r, ^b list>

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right, then evaluate each effect and collect the results. If one sequence is longer, its extra elements are ignored.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            [<CompiledName("ZipWithA")>]
            val inline zipWithA: f: (^a -> ^b -> Cont< ^r, ^c>) -> source1: ^a seq -> source2: ^b seq -> Cont< ^r, ^c list>

            /// <summary>Performs the effect 'n' times.</summary>
            [<CompiledName("ReplicateA")>]
            val inline replicateA: count: int -> Cont< ^r, ^a> -> Cont< ^r, ^a seq>


        /// <summary>Supplementary Functor operations on the given type.</summary>
        module Functor =

            /// <summary>Lift a function onto effects.</summary>
            [<CompiledName("Map")>]
            val inline map: f: (^a -> ^b) -> Cont< ^r, ^a> -> Cont< ^r, ^b>

            /// <summary>Replace all locations in the input with the same value.</summary>
            [<CompiledName("Replace")>]
            val replace: b: 'b -> Cont<'r, 'a> -> Cont< ^r, ^b>

            /// <summary>Perform an operation, store its result, perform an action using both the input and output, and finally return the output.</summary>
            [<CompiledName("Tee")>]
            val inline tee: f: (^a -> ^b) -> g: (^a -> ^b -> unit) -> Cont< ^r, ^a> -> Cont< ^r, ^b>


        /// <summary>Supplementary Comonad operations on the given type.</summary>
        module Comonad =

            /// <summary>Retrieve a value out of a context.</summary>
            [<CompiledName("Extract")>]
            val extract: Cont<'r, ^r> -> ^r

            /// <summary>Sequentially compose two co-effects.</summary>
            [<CompiledName("Extend")>]
            val inline extend: j: (Cont< ^r, ^a> -> ^b) -> Cont< ^r, ^a> -> Cont< ^r, ^b>

            /// <summary>Takes a comonadic container and produces a container of containers.</summary>
            [<CompiledName("Duplicate")>]
            val duplicate: Cont<'r, 'a> -> Cont< ^r, Cont< ^r, ^a>>

            /// <summary>Deconstructs a comonad through recursive (effectful) computations. Computation proceeds through the use of a continuation function.</summary>
            [<CompiledName("RecW")>]
            val inline recW: f: ((Cont< ^r, ^r> -> ^r) -> Cont< ^r, ^r> -> ^r) -> Cont< ^r, ^r> -> ^r


        /// <summary>Types with a binary, associative composition operation.</summary>
        module Semigroup =

            /// <summary>An associative composition operation.</summary>
            [<CompiledName("SAppend")>]
            val inline sappend: Cont< ^r, ^a> -> Cont< ^r, ^a> -> Cont< ^r, ^a>
                when ^a: (static member Append: ^a -> ^a -> ^a)


    /// <summary>Creates a computation expression for the given type.</summary>
    val cont: Compose.Monad.ContBuilder