namespace PTR.Context.Type.Incomplete.Tagged


[<Struct>]
type Phantom<'MetaData, 'T> = Phantom of 'T



[<AbstractClass; Sealed>]
/// Probably should be type PTR.Context.Class.Eq with ... static member inline Tag( ... 
type Eq =

    static member inline Of(a: Phantom<_, ^Value>, b: Phantom<_, ^Value>) =
            let (Phantom a) = a
            let (Phantom b) = b
            a = b

    static member inline Of(tag: Phantom<_, ^Value>, value: ^Value) =
        let (Phantom a) = tag in a = value

    static member inline Of(value: ^Value, tag: Phantom<_, ^Value>) =
        let (Phantom a) = tag in a = value


module Tagged =
    

    let inline tag<'meta> (x: ^a) : Phantom< ^meta, ^a> = Phantom x

    let a = tag<int> "3"

    //open type Tagging

    let a : Phantom<string, int> = Phantom 1
    let b : Phantom<int, int> = Phantom 1
    let c : Phantom<float, int> = Phantom 2
    let same1 = Eq.Of(a, b)
    let same2 = Eq.Of(a, 1)
    let same3 = Eq.Of(1, a)
    let diff1 = Eq.Of(a, c)
    let diff2 = Eq.Of(a, 2)
    let diff3 = Eq.Of(2, a)