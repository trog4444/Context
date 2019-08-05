namespace Context.Type.Incomplete


//??[<Struct>]
type Tree<'N> = Nil | Node of ^N * Tree< ^N> seq

