namespace Ptr.Context.Type


/// `State/Value` pair.
[<Struct>]
type StateValue<'state, 'value> = { State: ^state ; Value: ^value }


/// Stateful computations, i.e. computations that consume an initial state and
/// return a value along with a new state.
[<Struct; NoComparison; NoEquality>]
type State<'state, '``state*``, 'value> = State of (^state -> StateValue< ^``state*``, ^value>)


/// Operations on `State` values.
module State = 

    /// `State/Value` pair conversion methods.
    type StateConvert =
    
        /// Create a state/value pair.
        static member inline Curried state value = { State = state ; Value = value }
    
        /// Create a state/value pair.
        static member inline OfPair (state, value) = { State = state ; Value = value }
    
        /// Create a state/value pair.
        static member inline OfPair (struct (state, value)) = { State = state ; Value = value }
    
        /// Convert a function yielding a pair into a State/Value function.
        static member inline OfPair (f: ^s0 -> ^s1 * ^a) : State< ^s0, ^s1, ^a> =
            State (fun s -> match f s with (s, a) -> { State = s ; Value = a })
    
        /// Convert a function yielding a pair into a State/Value function.
        static member inline OfPair f = State (fun s -> match f s with struct (s, a) -> { State = s ; Value = a })
    
        /// Create a ref-tuple from a state/value pair.
        static member inline ToPair (sv: StateValue<_, _>) = (sv.State, sv.Value)
    
        /// Create a struct-tuple from a state/value pair.
        static member inline ToPair1 (sv: StateValue<_, _>) = struct (sv.State, sv.Value)


    /// Run a state computation with the given initial state and return the final state and value from it.
    let inline runState (s: ^s) (State sa) : StateValue< ^``s*``, ^b> = sa s

    /// Evaluate a state computation with the given initial state and return the final value,
    /// discarding the final state.
    let inline evalState (s: ^s) (State sa) : ^a =
        match sa s with (r: StateValue< ^``_``, ^a>) -> r.Value

    /// Execute a state computation with the given initial state and return the final state,
    /// discarding the final value.
    let inline execState (s: ^s) (State sa) : ^s =
        match sa s with r -> r.State

    /// Map both the return value and final state of a computation using the given function.
    let inline mapState f (State st) : State< ^s, ^``s**``, ^c> =
        State (fun s0 -> match st s0 with
                         | r -> f (r.State: ^``s*``) r.Value)

    /// withState f m executes action m on a state modified by applying f.
    let inline withState f (State st) : State< ^s, ^``s*``,  ^a> = State ((f: ^s -> ^s0) >> st)

    /// Retrieves the current state.
    let inline get<'s> : State< ^s, ^s, ^s> = State (fun s -> { State = s ; Value = s })

    /// Replace the state inside the computation.
    let inline put (s: ^s) = State (fun (_: ^``_``) -> { State = s ; Value = () })

    /// Maps an old state to a new state inside a state computation.
    let inline modify f : State< ^s, ^``s*``, unit> = State (fun s -> { State = f s ; Value = () })

    /// Store computed results to prevent recomputation on the same inputs.
    let inline cacheState (State st) : State< ^s, ^``s*``, ^a> =
        let d = System.Collections.Concurrent.ConcurrentDictionary< ^s, StateValue< ^``s*``, ^a>>(HashIdentity.Structural)
        let r = ref Unchecked.defaultof<StateValue< ^``s*``, ^a>>
        State (fun s -> if d.TryGetValue(s, r) then !r
                        else d.GetOrAdd(key = s, value = st s))


    /// Compositional operations on `State` values.
    module Compose =

        /// Lift a value onto an effectful context.
        let inline wrap x : State< ^s, ^s, ^a> = State (fun s -> { State = s ; Value = x })

        /// Sequentially compose two effects, passing any value produced by the first
        /// as an argument to the second.
        let inline bind (k: ^a -> State< ^``s*``, ^``s**``, ^b>) (State st) =
            State (fun (s: ^s) -> match st s with m -> match k m.Value with State st' -> st' m.State)

        /// Removes one layer of monadic context from a nested monad.
        let inline flatten (mm: State< ^s, ^``s*``, State< ^``s*``, ^``s**``, ^a>>) = bind id mm

        /// Sequential application on effects.
        let inline ap (State sv) (State sf) : State< ^s, ^``s**``, ^b> =
            State (fun s -> match sf s with
                            | f -> match sv (f.State: ^``s*``) with
                                    | v -> { State = v.State ; Value = f.Value v.Value })

        /// Lift a function onto effects.
        let inline map (f: ^a -> ^b) (State st) : State< ^s, ^``s*``, ^b> =
            State (fun s -> match st s with m -> { State = m.State ; Value = f m.Value })


        /// Supplementary Monad operations on the given type.
        module Monad =

            /// Monadic computation builder specialised to the given monad.
            type StateBuilder () =
                member inline s.Bind(m, k) = bind k m
                member inline s.Return x = wrap x
                member inline s.ReturnFrom m : State< ^s, ^``s*``, ^a> = m
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
            let inline composeM k2 (k1: ^a -> State< ^s, ^``s*``, ^b>) : ^a -> State< ^s, ^``s**``, ^c> =
                k1 >> bind k2
  
            /// Sequentially compose three actions, passing any value produced by the first
            /// two as arguments to the third.
            let inline bind2 k (State sa) (State sb) : State< ^s0, ^``s3``, ^c> =
                State (fun (s: ^s0) ->
                    match sa s  with
                    | A -> match sb (A.State: ^``s1``) with
                            | B -> match k A.Value B.Value with State sr -> sr (B.State: ^``s2``))

            /// Sequentially compose four actions, passing any value produced by the
            /// first two as arguments to the third.
            let inline bind3 (k: ^a -> ^b -> ^c -> State< ^s3, ^s4, ^d>) (State sa) (State sb) (State sc) =
                State (fun s ->
                    match sa (s: ^s0)  with
                    | A -> match sb (A.State: ^s1) with
                            | B -> match sc (B.State: ^s2) with
                                    | C -> match k A.Value B.Value C.Value with State sr -> sr (C.State: ^s3))

            /// Sequentially compose two actions, creating a third from the result and
            /// lifting a binary function on its effects.
            let inline bindMap (k: ^a -> State< ^s1, ^s2, ^b>) (f: ^a -> ^b -> ^c) (State sa) =
                State (fun (s: ^s0) ->
                    match sa s with
                    | A -> match k A.Value with
                            | State sb -> match sb (A.State: ^s1) with
                                            | B -> { State = B.State ; Value = f A.Value B.Value })

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to the intermediate result.
            /// The default monadic 'identity' function is used in each iteration where the continuation is applied.
            let inline recM f x : State< ^s, ^s, ^a> =
                let rec go m = bind (f (wrap >> go)) m in go (f wrap x)

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to an 'effect' applied over the intermediate result.
            /// Any constructor can be used in each iteration, in the case of union-types.
            let inline recMp f x : State< ^s, ^s, ^a> =
                let rec go m = bind (f go) m in go (f id x)

            /// <summary>Monadic fold over a structure associating to the right.</summary>
            /// <exception cref="System.ArgumentNullException">
            /// Thrown when the input sequence is null.</exception>
            let inline foldrM (f: ^a -> ^s0 -> State< ^s, ^s, ^s0>) (s0: ^s0) (source: ^a seq) : State< ^s, ^s, ^s0> =
                let g k x s = bind k (f x s)
                Seq.fold g wrap source s0

            /// <summary>Monadic fold over a structure associating to the left.</summary>
            /// <exception cref="System.ArgumentNullException">
            /// Thrown when the input sequence is null.</exception>
            let inline foldlM (f: ^s0 -> ^a -> State< ^s, ^s, ^s0>) (s0: ^s0) (source: ^a seq) : State< ^s, ^s, ^s0> =
                let g x k s = bind k (f s x)
                Seq.foldBack g source wrap s0


            /// Monadic zipping (combining or decomposing corresponding monadic elements).
            module Zip =
            
                /// Combine the corresponding contents of two monads into a single monad.
                let inline mzipWith (f: ^a -> ^b -> ^c) ma (mb: State< ^s1, ^s2, ^b>) : State< ^s0, ^s2, ^c> =
                    bind2 (fun a b -> wrap (f a b)) ma mb

                /// Merge the contents (of corresponding pairs) of two monads into a monad of pairs.
                let inline mzip ma (mb: State< ^s1, ^s2, ^b>) : State< ^s0, ^s2, ^a * ^b> =
                    mzipWith (fun a b -> a, b) ma mb
                    
                /// Decompose a monad comprised of corresponding pairs of values.
                let inline munzip (State st) : State< ^s, ^``s*``, ^a> * State< ^s, ^``s*``, ^b> =
                    State (fun s -> match st s with m -> { State = m.State ; Value = fst m.Value }),
                    State (fun s -> match st s with m -> { State = m.State ; Value = snd m.Value })


        /// Supplementary Applicative operations on the given type.
        module Applicative =

            /// Lift a binary function on effects.
            let inline map2 (f: ^a -> ^b -> ^c) (State sa) (State sb) : State< ^s0, ^s2, ^c> =
                State (fun s -> match sa s with
                                | A -> match sb (A.State: ^s1) with
                                        | B -> { State = B.State ; Value = f A.Value B.Value })

            /// Lift a ternary function on effects.
            let inline map3 (f: ^a -> ^b -> ^c -> ^d) (State sa) (State sb) (State sc) : State< ^s0, ^s3, ^d> =
                State (fun s -> match sa s with
                                | A -> match sb (A.State: ^s1) with
                                       | B -> match sc (B.State: ^s2) with
                                              | C -> { State = C.State
                                                     ; Value = f A.Value B.Value C.Value })

            /// Sequentially compose two effects, discarding any value produced by the first.
            let inline andThen (State sb) (State sa) : State< ^s0, ^s2, ^b> =
                State (fun s -> match sa s with a -> sb (a.State: ^s1))

            /// Conditional execution of effectful expressions.
            let inline when_ (condition: bool) f : State< ^s, ^s, unit> =
                if condition then f () else wrap ()

            /// <summary>Generalizes the sequence-based filter function.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline filterA p source : State< ^s, ^s, ^a seq> =
                State (fun s0 ->
                    let mutable s = s0
                    let xs = seq { for x in source do
                                    match p x with
                                    | State f -> match f s with
                                                    | A -> if A.Value
                                                           then s <- A.State
                                                                yield x } |> Seq.cache
                    do for _ in xs do ()
                    { State = s ; Value = xs })

            /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline sequenceA (source: State< ^s, ^s, ^a> seq) : State< ^s, ^s, ^a seq> =
                State (fun s0 -> let mutable st = s0
                                 let xs = seq { for (State sa) in source do
                                                 match sa st with
                                                 | sc -> st <- sc.State; yield sc.Value } |> Seq.cache
                                 for _ in xs do ()
                                 { State = st ; Value = xs })

            /// <summary>Produce an effect for the elements in the sequence from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline forA (f: ^a -> State< ^s, ^s, ^r>) (source: ^a seq) : State< ^s, ^s, ^r seq> =
                sequenceA (Seq.map f source)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when either input sequence is null.</exception>
            let inline for2A (f: ^a -> ^b -> State< ^s, ^s, ^r>) (source1: ^a seq) (source2: ^b seq)
                : State< ^s, ^s, ^r seq> =
                forA ((<||) f) (Seq.allPairs source1 source2)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right, then evaluate each effect and collect the results.
            /// If one sequence is longer, its extra elements are ignored.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline zipWithA (f: ^a -> ^b -> State< ^s, ^s, ^r>) (source1: ^a seq) (source2: ^b seq)
                : State< ^s, ^s, ^r seq> = sequenceA (Seq.map2 f source1 source2)

            /// Performs the effect 'n' times.
            let inline replicateA (n: uint32) (State f) : State< ^s, ^s, ^a seq> =
                State (fun s0 ->
                    let mutable st = s0
                    let xs = seq { for i = 1 to int n do
                                    match f st with
                                    | sc -> st <- sc.State; yield sc.Value } |> Seq.cache
                    for _ in xs do ()
                    { State = st ; Value = xs })


        /// Supplementary Functor operations on the given type.
        module Functor =

            /// Replace all locations in the input with the same value.
            let inline replace b (State sa) : State< ^s, ^``s*``, ^b> =
                State (fun s -> match sa s with m -> { State = (m.State: ^``s*``) ; Value = b })

            /// Perform an operation, store its result, perform an action using both
            /// the input and output, and finally return the output.
            let inline tee (f: ^a -> ^b) (g: ^a -> ^b -> unit) (State st) : State< ^s, ^``s*``, ^b> =
                State (fun s -> match st s with
                                | a -> let b = f a.Value
                                       g a.Value b
                                       { State = a.State ; Value = b })


        /// A two paramater functor where both the first and second arguments are covariant.
        module Bifunctor =

            /// Map over both arguments at the same time.
            let inline bimap (f: ^``s*`` -> ^``s**``) (g: ^a -> ^b) (State z) : State< ^s, ^``s**``, ^b> =
                State (fun s -> match z s with
                                | m -> { State = f m.State ; Value = g m.Value })

            /// Map covariantly over the first argument.
            let inline mapFst (f: ^``s*`` -> ^``s**``) (State z) : State< ^s, ^``s**``, ^a> =
                State (fun s -> match z s with
                                | m -> { State = f m.State ; Value = m.Value })

            /// Map covariantly over the second argument.
            let inline mapSnd (g: ^a -> ^b) (State z) : State< ^s, ^``s*``, ^b> =
                State (fun s -> match z s with
                                | m -> { State = m.State ; Value = g m.Value })


        /// Types with a binary, associative composition operation.
        module Semigroup =

            /// An associative composition operation.
            let inline sappend (a: State< ^s0, ^s1, ^a>) (b: State< ^s1, ^s2, ^a>) : State< ^s0, ^s2, ^a> =
                Applicative.map2 (fun a b -> (^a: (static member inline Append: ^a -> ^a -> ^a) (a, b))) a b

    
    /// Creates a computation expression for the given type.
    let state = Compose.Monad.StateBuilder ()



open State
open Compose
  
//  @ Operators @
type State<'s, '``s*``, 'a> with

// @ Primitive @

    /// The result of running a computation with a given environment.
    static member inline ( >- ) (m, s) = runState s m
    /// The result of running a computation with a given environment.
    static member inline ( -< ) (s, m) = runState s m

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