namespace Context.Type.Incomplete


[<Struct; NoComparison; NoEquality>]
type Tree<'N> = Nil | Node of ^N * Tree< ^N> seq

