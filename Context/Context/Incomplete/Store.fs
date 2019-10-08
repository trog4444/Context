namespace PTR.Context.Type//.Incomplete.Store

// http://hackage.haskell.org/package/comonad-5.0.5/docs/src/Control.Comonad.Trans.Store.html#pos

///// The 'Store' holds a constant value along with a modifiable 'accessor'
///// function which maps the 'stored value' to the 'focus'.
//[<Struct; NoComparison; NoEquality>]
//type Store<'s, 'a> = { Access: ^s -> ^a ; Pos: ^s }


///// operations on Store values.
////module Store =

////  /// Create a 'Store' using an accessor function and a stored value.
////  let store (accessor: 's -> 'a) (value: ^s) : Store< ^s, ^a> =
////    { Store.Access = accessor ; Pos = value }

////  /// Return a Store's accessor its stored value as a pair.
////  let runStore (store: Store<'s, 'a>) : (^s -> ^a) * ^s = store.Access, store.Pos

////  /// Apply a Store's accessor to its stored value.
////  let applyStore { Store.Access = (a: 's -> 'a) ; Pos = (s: ^s) } : ^a = a s

////  /// Read the stored value.
////  let pos (store: Store<'s, _>) : ^s = store.Pos

////  /// Set the stored value.
////  let seek (s: 's) (store: Store< ^s, 'a>) : Store< ^s, ^a> = { store with Pos = s }

////  /// Modify the stored value.
////  let inline seeks f (store: Store< ^s, ^a>) : Store< ^s, ^a> = { store with Pos = f store.Pos }

////  /// Peek at what the current focus would be for a different stored value.
////  let peek (s: 's) (store: Store< ^s, 'a>) : ^a = store.Access s

////  /// Peek at what the current focus would be if the stored value was modified by some function.
////  let inline peeks f (store: Store< ^s, ^a>) : ^a = store.Access (f store.Pos)

////  /// <summary>Applies a seq-valued function to the stored value, and then uses the new accessor to read the resulting foci.</summary>.
////  /// <exception cref="System.ArgumentNullException">Thrown when the resultant sequence is null.</exception>
////  let inline experiment (f: ^s -> ^s seq) (store: Store< ^s, ^a>) =
////    Seq.map store.Access (f store.Pos)


////  /// Compositional operations on Store values.
////  module Compose =


////    /// Supplementary Applicative operations on the given type.
////    module Applicative =

////      /// Lift a value onto an effectful context.
////      let inline wrap (x: ^a) : Store< ^s, ^a> =
////        { Store.Access = fun _ -> x
////        ; Pos = (^s: (static member inline Empty: unit -> ^s) ()) }

////      /// Sequential application on effects.
////      let inline ap (fv: Store< ^s, ^a>) (ff: Store< ^s, ^a -> ^b>) =
////        { Store.Access = fun s -> ff.Access s (fv.Access s)
////        ; Pos = (^s: (static member Append: ^s -> ^s -> ^s) (ff.Pos, fv.Pos)) }

////      /// Lift a binary function on effects.
////      let inline map2 (f: ^a -> ^b -> ^c) (fa: Store< ^s, ^a>) (fb: Store< ^s, ^b>) : Store< ^s, ^c> =
////        { Store.Access = fun s -> f (fa.Access s) (fb.Access s)
////        ; Pos = (^s: (static member Append: ^s -> ^s -> ^s) (fa.Pos, fb.Pos)) }

////      /// Lift a ternary function on effects.
////      let inline map3 (f: ^a -> ^b -> ^c -> ^d) (fa: Store< ^s, ^a>) (fb: Store< ^s, ^b>) (fc: Store< ^s, ^c>) : Store< ^s, ^d> =
////        let app a b = (^s: (static member Append: ^s -> ^s -> ^s) (a, b))
////        { Store.Access = fun s -> f (fa.Access s) (fb.Access s) (fc.Access s)
////        ; Pos = app fa.Pos (app fb.Pos fc.Pos) }

////      /// Sequentially compose two effects, discarding any value produced by the first.
////      let inline andThen (fb: Store< ^s, ^b>) (fa: Store< ^s, ^a>) : Store< ^s, ^b> =
////        { Store.Access = fun s -> ignore (fa.Access s); fb.Access s
////        ; Pos = (^s: (static member Append: ^s -> ^s -> ^s) (fa.Pos, fb.Pos)) }

////      /// Conditional execution of effectful expressions.
////      let inline when_ condition f : Store< ^s, unit> =
////        if condition then f () else wrap ()

////      /// <summary>Generalizes the sequence-based filter function.</summary>
////      /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
////      let inline filterA (p: ^a -> Store< ^s, bool>) (source: ^a seq) : Store< ^s, ^a list> =
////        Seq.foldBack (fun x -> map2 (fun flg xs -> if flg then x::xs else xs) (p x)) source (wrap [])

////      /// <summary>Evaluate each effect in the sequence from left to right, and collect the results.</summary>
////      /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
////      let inline sequenceA source : Store< ^s, ^a list> =
////        Seq.foldBack (map2 (fun x xs -> x::xs)) source (wrap [])

////      /// <summary>Produce an effect for the elements in the sequence from left to right then evaluate each effect, and collect the results.</summary>
////      /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
////      let inline forA f (source: ^a seq) : Store< ^s, ^b list> =
////        sequenceA (System.Linq.Enumerable.Select(source, System.Func<_,_>f))

////      /// <summary>Produce an effect for each pair of elements in the sequences from left to right then evaluate each effect, and collect the results.</summary>
////      /// <exception cref="System.ArgumentNullException">Thrown when either input sequence is null.</exception>
////      let inline ---END HERE--- f (source1: ^a seq) (source2: ^b seq) : Store< ^s, ^c list> =
////        forA ((<||) f) (Seq.allPairs source1 source2)

////      /// <summary>Produce an effect for each pair of elements in the sequences from left to right, then evaluate each effect and collect the results.
////      /// If one sequence is longer, its extra elements are ignored.</summary>
////      /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
////      let inline zipWithA f (source1: #seq< ^a>) (source2: #seq< ^b>) : Store< ^s, ^c list> =
////        sequenceA (System.Linq.Enumerable.Zip(source1, source2, System.Func<_,_,_>f))

////      /// Performs the effect 'n' times.
////      let inline replicateA count (fa: Store< ^s, ^a>) : Store< ^s, ^a seq> =
////        //sequenceA (Seq.replicate (max 0 count) fa)
////        let c = max 0 count
////        { Store.Access = fun s -> Seq.replicate c (fa.Access s)
////        ; Pos = Seq.foldBack
////              (fun x s -> (^s: (static member Append: ^s -> ^s -> ^s) (x, s)))
////              (Seq.replicate c fa.Pos)
////              (^s : (static member inline Empty: unit -> ^s) ()) }


////    /// Supplementary Functor operations on the given type.
////    module Functor =

////      /// Lift a function onto effects.
////      let inline map (f: ^a -> ^b) { Store.Access = a ; Pos = (s0: ^s) } =
////        { Store.Access = (fun s -> f (a s)) ; Pos = s0 }

////      /// Replace all locations in the input with the same value.
////      let inline replace (b: ^b) { Store.Pos = (s: ^s) } =
////        { Store.Access = (fun _ -> b) ; Pos = s }

////      /// Perform an operation, store its result, perform an action using both
////      /// the input and output, and finally return the output.
////      let inline tee (f: ^a -> ^b) (g: ^a -> ^b -> unit) (fa: Store< ^s, ^a>) : Store< ^s, ^b> =
////        { Store.Pos = fa.Pos
////        ; Access = fun s -> let a = fa.Access s
////                  let b = f a
////                  do g a b
////                  b }


////    /// Supplementary Comonad operations on the given type.
////    module Comonad =

////      /// Retrieve a value out of a context.
////      let extract { Store.Access = (a: 's -> 'a) ; Pos = (s: ^s) } : ^a = a s

////      /// Sequentially compose two co-effects.
////      let inline extend j (w: Store< ^s, ^a>) =
////        { Store.Access = (fun _ -> j w) ; Pos = w.Pos }

////      /// Takes a comonadic container and produces a container of containers.
////      let inline duplicate (w: Store<'s, 'a>) : Store< ^s, Store< ^s, ^a>> =
////        { Store.Access = (fun _ -> w) ; Pos = w.Pos }

////      /// Deconstructs a comonad through recursive (effectful) computations.
////      /// Computation proceeds through the use of a continuation function.
////      let inline recW f w : ^a =
////        let rec go w = f go w in go (extend (f extract) w)



////open Store
////open Store.Compose

////type Store<'s, 'a> with

////  static member inline ( >- ) (p, s) = peek s p
////  static member inline ( >- ) (p, f) = peeks f p
////  static member inline ( >- ) (p, f) = experiment f p
////  static member inline ( -< ) (s, p) = peek s p
////  static member inline ( -< ) (f, p) = peeks f p
////  static member inline ( -< ) (f, p) = experiment f p

////  static member inline ( <*> ) (ff, fv) = Applicative.ap fv ff
////  static member inline ( <**> ) (fv, ff) = Applicative.ap fv ff

////  static member inline ( *> ) (fa, fb) = Applicative.andThen fb fa
////  static member inline ( <* ) (fb, fa) = Applicative.andThen fb fa

////  static member inline ( |%> ) (fa, f) = Functor.map f fa
////  static member inline ( <%| ) (f, fa) = Functor.map f fa

////  static member inline ( %> ) (fa, b) = Functor.replace b fa
////  static member inline ( <% ) (b, fa) = Functor.replace b fa

////  static member inline ( =>> ) (w, j) = Comonad.extend j w
////  static member inline ( <<= ) (j, w) = Comonad.extend j w
