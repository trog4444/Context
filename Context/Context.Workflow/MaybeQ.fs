namespace Rogz.Context//.Workflow

//open Rogz.Context.Base



//module Maybe =

//    [<RequireQualifiedAccess>]
//    module Workflow =

//        type MaybeMonoid() =
//            member _.Yield x = Just x
//            member _.Combine(a, b) = match a with Just _ -> a | _ -> b
//            member _.Zero() = Nothing

//    let maybeMonoid = Workflow.MaybeMonoid()





//    type MaybeQueryBuilder() =

//// Functor

//        [<CustomOperation("map")>]
//        member inline _.Map(m, [<ProjectionParameter>] f) = Maybe.map f m
        
//// Applicative

//        member _.Return x = Maybe.unit x        
//        member _.ReturnFrom m : Maybe<_> = m
//        member _.Yield  x = Maybe.unit x
//        member _.YieldFrom  m : Maybe<_> = m
//        member _.Zero() = Maybe.unit ()

//        [<CustomOperation("map2", IsLikeZip=true)>]
//        member inline _.Map2(first, second, f) = Maybe.map2 f first second

//// Alternative

//        [<CustomOperation("orElse")>]
//        member _.OrElse(first, second) =
//            match first with
//            | Nothing -> second
//            | Just _  -> first
//        [<CustomOperation("orElseWith")>]
//        member inline _.OrElseWith(first, second) =
//            match first with
//            | Nothing -> second ()
//            | Just _  -> first
        
//// Monad

//        member inline _.Bind(m, f) = Maybe.bind f m
//        member inline _.For(m, f) = Maybe.bind f m

//// MonadPlus

//        [<CustomOperation("where", MaintainsVariableSpace=true)>]
//        member inline _.Where(m, [<ProjectionParameter>] p) = Maybe.filter p m

//        [<CustomOperation("join", AllowIntoPattern=true)>]
//        member inline _.GroupJoin(xs: Maybe< ^a>, ys: Maybe< ^b>, [<ProjectionParameter>] p, [<ProjectionParameter>] f) = Maybe.join p f xs ys


    
    
    
    
    
//    type MaybeZipBuilder() =
//        inherit Maybe.Workflow.MaybeBuilder()
//        //member _.Return x = Just x
//        //member _.ReturnFrom m : Maybe<_> = m
//        //member _.Zero() = Just ()
//        //member inline _.Bind(m, f) = bind f m
    
//    //// Util -- useful in debugging deeply nested things, otherwise it's "just" the get function for State.
//    //    [<CustomOperation("read", MaintainsVariableSpace=true, AllowIntoPattern=true)>]
//    //    member inline _.Read(source) = source
    
//    // Functor
//        [<CustomOperation("select")>]
//        member inline _.Select(m, [<ProjectionParameter>] f) = Maybe.map f m
    
//    // Applicative
//        //[<System.Obsolete("'For' expressions, while valid, they only used to get other functionailty and have nothing to do with sequences or looping, etc., so 'Bind' and 'Return' syntax is preferred.")>]
//        member _.Yield x = Just x // only needed for zip
            
//        //[<System.Obsolete("'For' expressions, while valid, they only used to get other functionailty and have nothing to do with sequences or looping, etc., so 'Bind' and 'Return' syntax is preferred.")>]
//        member inline _.For(m, f) = Maybe.bind f m // only needed for zip   
    
//        [<CustomOperation("zip", IsLikeZip=true)>]
//        member inline _.Zip(first, second, f) = Maybe.map2 f first second

//    // Alternative
//        [<CustomOperation("orElse", MaintainsVariableSpace=true, AllowIntoPattern=true)>]
//        member inline _.OrElse(first, second) =
//            match first with
//            | Nothing -> second
//            | Just _  -> first
//        [<CustomOperation("orElseWith", MaintainsVariableSpace=true, AllowIntoPattern=true)>]
//        member inline _.OrElseWith(first, second) =
//            match first with
//            | Nothing -> second ()
//            | Just _  -> first
    
    
//    // Monad
//        // Handled in base-class
            
    
//    // Monad-Plus
    
//        [<CustomOperation("where", MaintainsVariableSpaceUsingBind=true)>]
//        member inline _.Where(m, [<ProjectionParameter>] p) = match m with Nothing -> Nothing | Just a -> if p a then m else Nothing
    
//        [<CustomOperation("join", AllowIntoPattern=true)>]
//        // making p and f means I can make a 1-arg functions for each, where the first arg is for the given objects, while any other obj(s)' vars may be used as projections
//        member inline _.GroupJoin(xs: Maybe< ^a>, ys: Maybe< ^b>, [<ProjectionParameter>] p, [<ProjectionParameter>] f) =
//            match xs, ys with
//            | Nothing, _ | _, Nothing -> Nothing
//            | Just a, Just b          -> if p a b then Just (f a b) else Nothing
            
            
    
    
    
//    let doMaybe = MaybeZipBuilder()
    
    
    
//        //////////// "Zip tests"
//        //////////let more_tests_zip =
//        //////////    doMaybe {
//        //////////        map2 a in Just 1
//        //////////        map2 b in Just 2
//        //////////        map2 c in Just 3
//        //////////        map2 d in Just 4
//        //////////        join (Just 5) (fun it -> it > d) (fun it -> a, b, c, d, it) into xs
//        //////////        orElse (Just (-1,-2,-3,-4,-5))
//        //////////        map2 f in Just 6
//        //////////        where (f = 6)
//        //////////        map2 g in Just 7
//        //////////        select (xs, f, g)
//        //////////    }
    
//        //////////let more_tests_binds =
//        //////////    doMaybe {
//        //////////        let! a = Just 1            
//        //////////        let! b = Just 2
//        //////////        let! c = Just 3
//        //////////        let! d = Just 4
//        //////////        join (Just 5) (fun it -> it > d) (fun it -> a, b, c, d, it) into xs
//        //////////        orElseWith (fun () -> Just (-1,-2,-3,-4,-5))
//        //////////        let! f = Just 6
//        //////////        where (f = 6)
//        //////////        let! g = Just 7
//        //////////        select (xs, f, g)
//        //////////    }
    
//        //////////let same = more_tests_binds = more_tests_zip
//        //////////let results = sprintf "same: %b\nzip: %A\nbind: %A" (more_tests_zip = more_tests_binds) more_tests_zip more_tests_binds


//    //let doMaybe = MaybeQueryBuilder()