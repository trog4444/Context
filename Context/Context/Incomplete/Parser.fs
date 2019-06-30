namespace Ptr.Context.Incomplete.Type.Parser


//// http://dev.stephendiehl.com/fun/002_parsers.html


/////
//[<Struct>]
//type ParseResult<'rem, 'result> = { Remaining: ^rem list ; Result: ^result }


/////
//type ParseResult =
    
//    ///
//    static member inline Curried rem res = { Remaining = rem ; Result = res }

//    ///
//    static member inline OfTuple (rem, res) = { Remaining = rem ; Result = res }

//    ///
//    static member inline OfTuple1 (struct (rem, res)) = { Remaining = rem ; Result = res }


/////
//[<Struct; NoComparison; NoEquality>]
//type Parser<'source, 'rem, 'result> =
//    Parser of (^source list -> ParseResult< ^rem, ^result> list)


/////
//module Std =

//    ///
//    let inline runParser (Parser p) source = p source

//    ///
//    let inline mapParser f (Parser p) =
//        Parser (p >> List.map (fun r -> { Remaining = List.map f r.Remaining ; Result = r.Result }))

//    ///
//    let inline withParser (f: ^s -> ^s0)  (Parser p) : Parser< ^s, ^r, ^a> = Parser (List.map f >> p)

//    /////
//    //let digit : Parser<char, char, int option> =
//    //    Parser (function
//    //        | []    -> []
//    //        | x::xs -> if System.Char.IsDigit x
//    //                   then [{ Remaining = xs ; Result = Some (int (string x)) }]
//    //                   else [{ Remaining = xs ; Result = None }])

//    ///
//    let item : Parser<'a, 'a, 'a> =
//        Parser (function [] -> [] | x::xs -> [{ Remaining = xs ; Result = x }])

//    ///
//    let inline assoc (key: ^key) =
//        Parser (function
//            | [] -> []
//            | ((k, _) as t)::xs -> if k = key then [{ Remaining = xs ; Result = t }] else []) 

//    ///
//    let inline cacheParser (Parser p) =
//        let d = System.Collections.Concurrent.ConcurrentDictionary< ^s list, ParseResult< ^r, ^a> list>(HashIdentity.Structural)
//        let r = ref Unchecked.defaultof<ParseResult< ^r, ^a> list>
//        Parser (fun xs -> if d.TryGetValue(xs, r) then !r else d.GetOrAdd(key = xs, value = p xs))



/////
//module Compose =

//    ///
//    let inline wrap x : Parser< ^s, ^s, ^a> = Parser (fun xs -> [{ Remaining = xs ; Result = x }])

//    ///
//    let inline bind (k: ^a -> Parser< ^rs, ^``r*``, ^b>) (Parser (p: ^s list -> ParseResult< ^rs, ^a> list)) =
//        Parser (fun s -> [ for x in p s do match k x.Result with Parser p -> yield! p x.Remaining ])

//    ///
//    let inline flatten (mm: Parser< ^s, ^rs, Parser< ^rs, ^``r*``, ^a>>) = bind id mm
        
//    ///
//    let inline ap (Parser pv) (Parser pf) =
//        Parser (fun xs ->
//            [ for f in pf (xs: ^s list) do
//              for v in pv (f.Remaining: ^rs list) do
//                yield { Remaining = (v.Remaining: ^``r*`` list) ; Result = f.Result v.Result }])

//    ///
//    let inline map f (Parser p) : Parser< ^s, ^r, ^b> =
//        Parser (fun s -> [ for x in p s -> { Remaining = x.Remaining ; Result = f x.Result }])


//    ///
//    module Monad =
        
//        ///
//        type ParserBuilder () = class end


//        ///
//        module Plus =

//            ///
//            let inline mzero<'s, 'r, 'a> : Parser<'s, 'r, 'a> = Parser (fun _ -> [])

//            ///
//            let inline mplus (Parser p1) (Parser p2) : Parser< ^s, ^r, ^a> =
//                Parser (fun xs -> match p1 xs with [] -> p2 xs | rs -> rs)


//            ///
//            module General = do failwith "undefined"


//        ///
//        module Zip = do failwith "undefined"


    
//    /// Creates a computation expression for the given type.
//    let parser = Monad.ParserBuilder ()


//    ///
//    module Applicative =

//        ///
//        let inline map2 f fa fb = bind (fun a -> map (f a) fb) fa


//        ///
//        module Alternative =

//            ///
//            let inline empty<'s, 'r, 'a> : Parser<'s, 'r, 'a> = Monad.Plus.mzero

//            ///
//            let inline orElse (Parser p2) (Parser p1) : Parser< ^s, ^r, ^a> =
//                Parser (fun xs -> match p1 xs with [] -> p2 xs | rs -> rs)

//            /// <summary>The sum of a collection of effects.</summary>
//            /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
//            let inline asum t_fa = do failwith "undefined" //Monad.Plus.General.msum t_fa

//            /// Return one or none results on effects.
//            let inline optional fa : Parser< ^s, ^s, ^a option> =
//                orElse (wrap None) (map Some fa)

//            /// Create a new item if the previous was empty, else keep the original.
//            let inline alt def (Parser p) : Parser< ^s, ^r, ^a> =
//                Parser (fun xs ->
//                    match p xs with
//                    | [] -> match def () with Parser p2 -> p2 xs
//                    | rs -> rs)            


//            /// Operations that 'repeat' effects.
//            module Repeat =

//                /// Returns one or more results.
//                let inline some v : Parser< ^s, ^s, ^a list> =
//                    let mt = wrap []
//                    let rec inline many_v () = orElse mt some_v
//                    and some_v = map2 (fun x xs -> x::xs) v (many_v ())
//                    some_v

//                /// Returns zero or more results.
//                let inline many v : Parser< ^s, ^s, ^a list> =
//                    let mt = wrap []
//                    let rec inline many_v () = orElse mt some_v
//                    and some_v = map2 (fun x xs -> x::xs) v (many_v ())
//                    many_v ()

                
//                let s = ([1;1;1;1;1] : int list) //|> List.pairwise
//                let p = (item) : Parser<int, int, int> //Parser<int * int, int * int, int * int>
//                let rp = runParser p s
//                let a = many p
//                let ra = runParser p s
//                let same = rp = ra

//    ///
//    module Functor = do failwith "undefined"


//    ///
//    module Bimonad = failwith "need to lookup this module in Haskell"
//        //
//        //
//        //let inline biwrap x = Parser (fun xs -> [struct (xs, x)])
//        //
//        //
//        //let inline bibind f (Parser p) =
//        //    Parser (fun xs -> [ for struct (xs', x) in p xs do match f x with Parser p' -> yield! (p' xs': struct (^c list * ^d) list) ])


//    ///
//    module Biapplicative = do failwith "undefined"


//    ///
//    module Bifunctor =
        
//        ///
//        let inline bimap (f: ^r0 -> ^r) (g: ^a -> ^b) (Parser p) : Parser< ^s, ^r, ^b> =
//            Parser (p >> List.map (fun r -> { Remaining = List.map f r.Remaining ; Result = g r.Result }))

//        /// Map covariantly over the first argument.
//        let inline mapFst (f: ^r0 -> ^r) (Parser p) : Parser< ^s, ^r, ^a> =
//            Parser (p >> List.map (fun r -> { Remaining = List.map f r.Remaining ; Result = r.Result }))

//        /// Map covariantly over the second argument.
//        let inline mapSnd g (Parser p) : Parser< ^s, ^r, ^b> =
//            Parser (p >> List.map (fun r -> { Remaining = r.Remaining ; Result = g r.Result }))



//open Std
//open Compose
  
////  @ Operators @
//type Parser<'s, 'r, 'a> with

//// @ Primitive @

//    /// The result of running a computation with a given environment.
//    static member inline ( >- ) (m, e) = runParser m e
//    /// The result of running a computation with a given environment.
//    static member inline ( -< ) (e, m) = runParser m e

////// @ Cat @

////    /// Compose two members of a category together.
////    static member inline ( >>> ) (ca, cb) = Cat.compose cb ca
////    /// Compose two members of a category together.
////    static member inline ( <<< ) (cb, ca) = Cat.compose cb ca

////    /// Identity of the category.
////    static member inline Id () : Reader< ^a, ^a> = Cat.identity

////// @ Arrow @

////    /// Split the input between the two argument arrows and combine their output.
////    static member inline ( *** ) (aa, ab) = Arrow.split ab aa

////    /// Fanout: send the input to both argument arrows and combine their output.
////    static member inline ( &&& ) (aa, ab) = Arrow.fanout ab aa

////// @ Arrow.Choice @

////    /// Split the input between the two argument arrows, retagging and merging their outputs.
////    static member inline ( +++ ) (aa, ab) = Arrow.Choice.merge ab aa

////    /// Split the input between the two argument arrows and merge their outputs.
////    static member inline ( ||| ) (aa, ab) = Arrow.Choice.fanin ab aa

//// @ Monad @

//    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
//    static member inline ( >>= ) (m, k) = bind k m
//    /// Sequentially compose two effects, passing any value produced by the first as an argument to the second.
//    static member inline ( =<< ) (k, m) = bind k m

//// @ Applicative @

//    /// Sequential application on effects.
//    static member inline ( <*> )  (ff, fx) = ap fx ff
//    /// Sequential application on effects.
//    static member inline ( <**> ) (fx, ff) = ap fx ff

//    ///// Sequentially compose two effects, discarding any value produced by the first.
//    //static member inline ( *> ) (fa, fb) = Applicative.andThen fb fa
//    ///// Sequentially compose two effects, discarding any value produced by the first.
//    //static member inline ( <* ) (fb, fa) = Applicative.andThen fb fa

//// @ Applicative.Alternative @

//    /// An associative binary operation on applicative functors.
//    static member inline ( <|> ) (c1, c2) = Applicative.Alternative.orElse c2 c1
//    /// An associative binary operation on applicative functors.
//    static member inline ( <||> ) (c2, c1) = Applicative.Alternative.orElse c2 c1

//// @ Functor @

//    /// Lift a function onto effects.
//    static member inline ( |>> ) (fa, f) = map f fa
//    /// Lift a function onto effects.
//    static member inline ( <<| ) (f, fa) = map f fa

//    ///// Replace all locations in the input with the same value.
//    //static member inline ( &> ) (b, fx) = Functor.replace b fx
//    ///// Replace all locations in the input with the same value.
//    //static member inline ( <& ) (fx, b) = Functor.replace b fx

//// @ Semigroup @

//    ///// An associative composition operation.
//    //static member inline Append (e1, e2) = Semigroup.sappend e1 e2

//    ///// An associative composition operation.
//    //static member inline ( ++ ) (e1, e2) = Semigroup.sappend e1 e2

////// @ Monoid @

////    static member inline Empty () : Maybe<'a> = Nothing