namespace PTR.Context.Type.Incomplete.Generic


/// <summary>Empty interface type that can be used to represent any other type without preserving type information.
/// An example use of this is in making heterogeneous collections without boxing the inputs.</summary>
[<Interface>]
type IGeneric = interface end


[<Struct>]
type Generic<'T>(value: 'T) =
    member inline _.Value = value
    override _.ToString() = sprintf "Generic %A" value
    with interface IGeneric


module Generic =

    module Pattern =

        let inline ( |Generic| ) (gen: Generic<_>) = Generic gen.Value

    open Pattern


    let a = Generic 1
    //ignore (a.Value)
    //ignore (a.Value)
    let b = let (Generic av) = a in av
    //ignore (b.Value)
    //ignore (b.Value)
    let c = Generic 3
    //ignore (c.Value)
    //ignore (c.Value)
    

    let inline extract (gen: Generic< ^a>) : ^a = gen.Value

    let inline withGeneric (f: IGeneric -> ^r option) (gen: #IGeneric) : ^r option = f gen

    let aaa =
        Generic 1
        |> withGeneric
            (function
             | :? Generic<int> as i -> Some <| i.Value
             | :? Generic<string> as s -> Some <| int s.Value
             | _ -> None)

    let inline foldBy (fGeneric: IGeneric -> ^a option) folder seed (anys: #seq<#IGeneric>) : ^s =
        let mutable s = seed
        let apply any = match fGeneric any with None -> () | Some a -> s <- folder s a
        for x in anys do apply x
        s

    let inline fold (folder: ^s -> IGeneric -> ^s) seed (gens: #seq<#IGeneric>) : ^s =
        let mutable s = seed in for x in gens do s <- folder s x
        s

    let xs : IGeneric list = [Generic 1; Generic "2.1"; Generic 3.2; Generic (obj()); Generic (exn()); Generic 'a']
    let inline f (any: IGeneric) =
        match any with
        | :? Generic<int> as i -> i.Value |> string |> Some
        | :? Generic<string> as s -> s.Value |> Some
        | :? Generic<float> as f -> f.Value |> string |> Some
        | _ -> None
    let x = withGeneric f <| Generic 1
    let folder s a = sprintf "%s (%s)" s a
    let rs = foldBy f folder "start:" xs