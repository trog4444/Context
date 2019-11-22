// #load "FolderName\FileName.fs"
#load "Types\Maybe.fs"
#load "Types\Either.fs"
#load "Contexts\Maybe.fs"
#load "Operators.fs"
open Rogz.Context.Extra.Operators
open Rogz.Context.Data.Maybe
module M = Maybe


let j1 = Just 1
let j2 = Just 2
let jadd = Just ((+) 1)
let n = M.empty<int>
let even n = n % 2 = 0

let ``j1_map_string: Just "1"`` = j1 |%> string
let ``n_map_string: Nothing`` = n |%> string

let ``j1_replaceby_3: Just 3`` = j1 %> 3

let ``j2_from_j1: Just 2`` = j1 *> j2
let ``j1_over_n: Just 1`` = n <|> j1
let ``j1_plus1_ap: Just 2`` = jadd <*> j1

let ``j1_filtered_to_nothing: Nothing`` = j1 ?> even
let ``j2_unchanged_by_filter: Just 2`` = j2 ?> even

let ``j1_bind_string: Just "1"`` = j1 >>= (string >> M.unit)