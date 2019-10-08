namespace PTR.Context.Type//.Incomplete.Iso

// probably move to Prelude

////[<Struct; NoComparison; NoEquality>]
////type Iso<'A, 'B> = { Forward: (^A -> ^B) ; Backward: (^B -> ^A) }


////must satisfy :
////    iso.Forward >> iso.Backward == id : 'A
////    iso.Forward << iso.Backward == id : 'B