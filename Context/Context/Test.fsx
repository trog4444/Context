
#load @"Contexts/Maybe.fs"
#load @"Extensions/Builders/Maybe.fs"

open PTR.Context
open PTR.Context.Type
open Maybe
open Extension.Builder.Maybe.Combine.AsList

let a = maybe {
    let! a = Just 1
    let! b = Just "2"
    return a + int b }

let b = maybe {
    let! a = Just 1
    return a
    let! b = Just 2    
    return b
    return! Just 3 }