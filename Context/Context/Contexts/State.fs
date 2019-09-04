namespace PTR.Context.Type

/// Result of a `State` computation.
[<Struct>]
type StateValue<'State, 'Value> = { State: ^State ; Value: ^Value }

/// Stateful computations, i.e. computations that consume an initial state and
/// return a value along with a new state.
[<Struct; NoComparison; NoEquality>]
type State<'State1, 'State2, 'Result> = State of (^State1 -> StateValue< ^State2, ^Result>)


/// Operations on `State` values.
module State =

    /// Active patterns on `State` values.
    module Pattern =
  
        /// Returns the state and value of a Stateful computation result as a pair (tuple).
        let inline ( |SVPair| ) (sv: StateValue< ^s, ^a>) =
            SVPair (struct (sv.State, sv.Value))


    /// Run a state computation with the given initial state and return the final state and value from it.
    let runState (s: 's1) (State (sa: 's1 -> StateValue<'s2, 'a>)) : StateValue< ^s2, ^a> = sa s

    /// Evaluate a state computation with the given initial state and return the final value,
    /// discarding the final state.
    let evalState (s: 's1) (State (sa: 's1 -> StateValue<'s2, 'a>)) : ^a = (sa s).Value

    /// Execute a state computation with the given initial state and return the final state,
    /// discarding the final value.
    let execState (s: 's1) (State (sa: 's1 -> StateValue<'s2, 'a>)) : ^s2 = (sa s).State

    /// Map both the return value and final state of a computation using the given function.
    let inline mapState f (State st) : State< ^s1, ^s3, ^b> =
        State (fun s0 ->
            let st = st s0
            match f (st.State: ^s2) st.Value with
            | s, v -> { StateValue.State = s ; Value = v })

    /// withState f m executes action m on a state modified by applying f.
    let inline withState (f: ^s1 -> ^s0) (State st) : State< ^s1, ^s2, ^a> =
        State (fun s -> st (f s))

    /// Retrieves the current state.
    let get<'s> : State< ^s, ^s, ^s> = State (fun s -> { StateValue.State = s ; Value = s })

    /// Replace the state inside the computation.
    let put (s: 's) : State<'``_``, ^s, unit> =
        State (fun (_: ^``_``) -> { StateValue.State = s ; Value = () })

    /// Maps an old state to a new state inside a state computation.
    let inline modify f : State< ^s1, ^s2, unit> =
        State (fun s -> { StateValue.State = f s ; Value = () })

    /// Store computed results to prevent recomputation on the same inputs.
    let inline cacheState (State st) : State< ^s1, ^s2, ^a> =
        let d = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        State (fun s -> match d.TryGetValue(s) with
                        | true, sv -> sv
                        | false, _ -> let sv = st s in d.[s] <- sv ; sv)


    /// Compositional operations on `State` values.
    module Compose =

        /// Supplementary Monad operations on the given type.
        module Monad =

            /// Lift a value onto an effectful context.
            let wrap (x: 'a) : State<'s, ^s, ^a> =
                State (fun s -> { StateValue.State = s ; Value = x })

            /// Sequentially compose two effects, passing any value produced by the first
            /// as an argument to the second.
            let inline bind (k: ^a -> State< ^s2, ^s3, ^b>) (State st) =
                State (fun (s: ^s1) ->
                    match st s with
                    | (sv: StateValue< ^s2, ^a>) ->
                    match k sv.Value with
                    | State st' -> st' sv.State)

            /// Removes one layer of monadic context from a nested monad.
            let inline flatten (mm: State< ^s1, ^s2, State< ^s2, ^s3, ^a>>) : State< ^s1, ^s3, ^a> =
                State (fun (s: ^s1) ->
                    match mm with
                    | State st ->
                    let st' = st s
                    match st'.Value with
                    | State st'' -> st'' st'.State)


            /// Monadic computation builder specialised to the given monad.
            type StateBuilder () =
                member inline s.Bind(m, k) = bind k m
                member inline s.Return x = wrap x
                member inline s.ReturnFrom m : State< ^s1, ^s2, ^a> = m
                member inline s.Zero () = s.Return ()
 
                member inline s.Delay f = f ()
                member inline s.Run f = f
 
                member inline s.TryWith (body, handler) = try s.ReturnFrom(body ()) with e -> handler e
                member inline s.TryFinally (body, finalizer) = try s.ReturnFrom(body ()) finally finalizer ()
 
                member inline s.Using(disp: #System.IDisposable, body) =
                    s.TryFinally((fun () -> body disp), fun () -> match box disp with null -> () | _ -> disp.Dispose ())
 
                member inline s.While(guard, body) =
                    let rec loop = function
                    | false -> s.Zero ()
                    | true -> s.Bind(body (), fun x -> loop (guard x))
                    loop (guard ())
 
                member inline s.For(seq: _ seq, body) =
                    s.Using(seq.GetEnumerator(), fun enum -> s.While(enum.MoveNext, s.Delay(fun () -> body enum.Current)))


            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to the intermediate result.
            /// The default monadic 'identity' function is used in each iteration where the continuation is applied.
            let inline recM f x : State< ^s, ^s, ^a> =
                let rec go m = bind (f wrapgo) m
                and wrapgo x = go (wrap x)
                go (f wrap x)

            /// Build a monad through recursive (effectful) computations.
            /// Computation proceeds through the use of a continuation function applied to an 'effect' applied over the intermediate result.
            /// Any constructor can be used in each iteration, in the case of union-types.
            let inline recMp f x : State< ^s, ^s, ^a> =
                let rec go m = bind (f go) m in go (f id x)

            /// <summary>Monadic fold over a structure associating to the right.</summary>
            /// <exception cref="System.ArgumentNullException">
            /// Thrown when the input sequence is null.</exception>
            let inline foldrM (f: ^a -> ^s0 -> State< ^s, ^s, ^s0>) (s0: ^s0) (source: ^a seq)
                : State< ^s, ^s, ^s0> =
                let g k x s = bind k (f x s)
                Seq.fold g wrap source s0

            /// <summary>Monadic fold over a structure associating to the left.</summary>
            /// <exception cref="System.ArgumentNullException">
            /// Thrown when the input sequence is null.</exception>
            let inline foldlM (f: ^s0 -> ^a -> State< ^s, ^s, ^s0>) (s0: ^s0) (source: ^a seq)
                : State< ^s, ^s, ^s0> =
                let g x k s = bind k (f s x)
                Seq.foldBack (fun x k s -> bind k (f s x)) source wrap s0


        /// Supplementary Applicative operations on the given type.
        module Applicative =

            /// Lift a value onto an effectful context.
            let wrap (x: 'a) : State<'s, ^s, ^a> =
                State (fun s -> { StateValue.State = s ; Value = x })

            /// Sequential application on effects.
            let inline ap (State sv) (State sf) : State< ^s1, ^s3, ^b> =
                State (fun s ->
                    let sf = sf s
                    let sv = sv (sf.State: ^s2)
                    { StateValue.State = sv.State
                    ; Value = sf.Value sv.Value })

            /// Lift a binary function on effects.
            let inline map2 (f: ^a -> ^b -> ^c) (State sa) (State sb) : State< ^s1, ^s3, ^c> =
                State (fun s ->
                    let sa = sa s
                    let sb = sb (sa.State: ^s2)
                    { StateValue.State = sb.State
                    ; Value = f sa.Value sb.Value })

            /// Lift a ternary function on effects.
            let inline map3 (f: ^a -> ^b -> ^c -> ^d) (State sa) (State sb) (State sc)
                : State< ^s1, ^s4, ^d> =
                State (fun s ->
                    match sa s with
                    | sa ->
                    match sb (sa.State: ^s2) with
                    | sb ->
                    match sc (sb.State: ^s3) with
                    | sc -> { StateValue.State = sc.State
                            ; Value = f sa.Value sb.Value sc.Value })

            /// Sequentially compose two effects, discarding any value produced by the first.
            let inline andThen (State sb) (State sa) : State< ^s1, ^s3, ^b> =
                State (fun s -> sb ((sa s).State: ^s2))

            /// Conditional execution of effectful expressions.
            let inline when_ condition f : State< ^s, ^s, unit> =
                if condition then f () else wrap ()

            /// <summary>Generalizes the sequence-based filter function.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline filterA p source : State< ^s, ^s, ^a seq> =
                State (fun s0 ->
                    let mutable st = s0
                    let xs = seq { for x in source do
                                       match p x with
                                       | State f ->
                                        let sb = f st
                                        if sb.Value
                                        then st <- sb.State
                                             yield x } |> Seq.cache
                    do for _ in xs do ()
                    { StateValue.State = st ; Value = xs })

            /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline sequenceA (source: State< ^s, ^s, ^a> seq) : State< ^s, ^s, ^a seq> =
                State (fun s0 ->
                    let mutable st = s0
                    let xs = seq { for (State sa) in source do
                                       match sa st with
                                       | sa -> st <- sa.State
                                               yield sa.Value } |> Seq.cache
                    do for _ in xs do ()
                    { StateValue.State = st ; Value = xs })

            /// <summary>Produce an effect for the elements in the sequence from left to right then evaluate each effect, and collect the results.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline forA (f: ^a -> State< ^s, ^s, ^b>) (source: ^a seq) : State< ^s, ^s, ^b seq> =
                sequenceA (Seq.map f source)

            /// <summary>Produce an effect for each pair of elements in the sequences from left to right, then evaluate each effect and collect the results.
            /// If one sequence is longer, its extra elements are ignored.</summary>
            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
            let inline zipWithA (f: ^a -> ^b -> State< ^s, ^s, ^c>) (source1: ^a seq) (source2: ^b seq)
                : State< ^s, ^s, ^c seq> = sequenceA (Seq.map2 f source1 source2)

            /// Performs the effect 'n' times.
            let inline replicateA count (State f) : State< ^s, ^s, ^a seq> =
                State (fun s0 ->
                    let mutable st = s0
                    let xs = Seq.cache (seq { for __ = 1 to max 0 count do
                                                  match f st with
                                                  | sa -> st <- sa.State; yield sa.Value })
                    do for _ in xs do ()
                    { StateValue.State = st ; Value = xs })


        /// Supplementary Functor operations on the given type.
        module Functor =

            /// Lift a function onto effects.
            let inline map (f: ^a -> ^b) (State st) : State< ^s1, ^s2, ^b> =
                State (fun s ->
                    let sa = st s
                    { StateValue.State = sa.State
                    ; Value = f sa.Value })

            /// Replace all locations in the input with the same value.
            let replace (b: 'b) (State (sa: 's1 -> StateValue<'s2, 'a>)) : State< ^s1, ^s2, ^b> =
                State (fun s ->
                    let s_ = sa s
                    { StateValue.State = s_.State
                    ; Value = b })

            /// Perform an operation, store its result, perform an action using both
            /// the input and output, and finally return the output.
            let inline tee (f: ^a -> ^b) (g: ^a -> ^b -> unit) (State st) : State< ^s1, ^s2, ^b> =
                State (fun s ->
                    match st s with
                    | sa -> let b = f sa.Value
                            do g sa.Value b
                            { StateValue.State = sa.State
                            ; Value = b })


        /// A two paramater functor where both the first and second arguments are covariant.
        module Bifunctor =

            /// Map over both arguments at the same time.
            let inline bimap (f: ^s2 -> ^s3) (g: ^a -> ^b) (State z) : State< ^s1, ^s3, ^b> =
                State (fun s -> match z (s: ^s1) with
                                | sa -> { StateValue.State = f sa.State
                                        ; Value = g sa.Value })

            /// Map covariantly over the first argument.
            let inline mapFst (f: ^s2 -> ^s3) (State z) : State< ^s1, ^s3, ^a> =
                State (fun s -> match z (s: ^s1) with
                                | sa -> { StateValue.State = f sa.State
                                        ; Value = sa.Value })

            /// Map covariantly over the second argument.
            let inline mapSnd (g: ^a -> ^b) (State z) : State< ^s1, ^s2, ^b> =
                State (fun s -> match z s with
                                | sa -> { StateValue.State = sa.State
                                        ; Value = g sa.Value })


        /// Types with a binary, associative composition operation.
        module Semigroup =

            /// An associative composition operation.
            let inline sappend (a: State< ^s1, ^s2, ^a>) (b: State< ^s2, ^s3, ^a>)
                : State< ^s1, ^s3, ^a> =
                Applicative.map2 (fun a b ->
                    (^a: (static member inline Append: ^a -> ^a -> ^a) (a, b))) a b

  
    /// Creates a computation expression for the given type.
    let state = Compose.Monad.StateBuilder ()



open State
open Compose
 
// @ Operators @
type State<'S1, 'S2, 'A> with

// @ Primitive @

    /// The result of running a computation with a given environment.
    static member inline ( >- ) (m, s) = runState s m
    /// The result of running a computation with a given environment.
    static member inline ( -< ) (s, m) = runState s m

// @ Monad @

    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
    static member inline ( >>= ) (m, k) = Monad.bind k m
    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
    static member inline ( =<< ) (k, m) = Monad.bind k m

// @ Applicative @

    /// Sequential application on effects.
    static member inline ( <*> ) (ff, fx) = Applicative.ap fx ff
    /// Sequential application on effects.
    static member inline ( <**> ) (fx, ff) = Applicative.ap fx ff

    /// Sequentially compose two effects, discarding any value produced by the first.
    static member inline ( *> ) (fa, fb) = Applicative.andThen fb fa
    /// Sequentially compose two effects, discarding any value produced by the first.
    static member inline ( <* ) (fb, fa) = Applicative.andThen fb fa

// @ Functor @

    /// Lift a function onto effects.
    static member inline ( |%> ) (fa, f) = Functor.map f fa
    /// Lift a function onto effects.
    static member inline ( <%| ) (f, fa) = Functor.map f fa

    /// Replace all locations in the input with the same value.
    static member inline ( %> ) (b, fa) = Functor.replace b fa
    /// Replace all locations in the input with the same value.
    static member inline ( <% ) (fa, b) = Functor.replace b fa

// @ Semigroup @

    /// An associative composition operation.
    static member inline Append (e1, e2) = Semigroup.sappend e1 e2
