namespace PTR.Context.Type.Incomplete.Eq

// probably move to Prelude ?


[<Struct; NoComparison; NoEquality>]
type Equivalence<'a, 'b> = Eq of ('a -> 'b -> bool)


type Equivalence<'a> = Equivalence<'a, 'a>


module Eq =

    let inline testEq a b (Eq q) = q a b

    let inline withEq (f: ^b -> ^a) (g: ^d -> ^c) (Eq q) = Eq (fun a b -> q (f a) (g b))

    let inline eqAnd (Eq qa) (Eq qb) = Eq (fun a b -> qa a b && qb a b)

    let inline eqOr (Eq qa) (Eq qb) = Eq (fun a b -> qa a b || qb a b)


    module Contrafunctor =

        let inline contramap (f: ^b0 -> ^b) (Eq q) : Equivalence< ^a, ^b0> =
            Eq (fun a b -> q a (f b))


    module Semigroup =

        let inline sappend (Eq qa) (Eq qb) : Equivalence< ^a, ^b> =
            Eq (fun a b -> qa a b && qb a b)


    module Monoid =
        
        let mempty<'a, 'b> : Equivalence<'a, 'b> = Eq (fun _ _ -> true)

        let inline mappend (Eq qa) (Eq qb) : Equivalence< ^a, ^b> =
            Eq (fun a b -> qa a b && qb a b)

        let inline mtimes n e =
            let rec go acc = function
            | 0 -> mempty
            | 1 -> acc
            | n -> go (mappend e acc) (n - 1)
            go e (max 0 n)

        let inline mconcat (source: #seq<Equivalence< ^a, ^b>>) : Equivalence< ^a, ^b> =
            Eq (fun a b ->
                let mutable g = true
                use e = source.GetEnumerator()
                while g && e.MoveNext() do let (Eq q) = e.Current in g <- q a b
                g)