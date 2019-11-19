namespace Rogz.Context.Data.Cont

[<Struct; NoComparison; NoEquality>]
type Cont<'R, 'T> = Cont of (('T -> 'R) -> 'R) with

// Function
    member inline s.Invoke (k: System.Func< ^T, ^R>) = 
        let (Cont cc) = s in cc k.Invoke

// Functor
    member inline s.Select(f: System.Func< ^T, ^U>) : Cont< ^R, ^U> =
        let (Cont cc) = s
        Cont (fun k -> cc (fun a -> k (f.Invoke(a))))

// Applicative
    member inline s.Zip((Cont c2), f: System.Func< ^T, ^U, ^V>) : Cont< ^R, ^V> =
        let (Cont c1) = s
        Cont (fun k -> c1 (fun a -> c2 (fun b -> k (f.Invoke(a, b)))))

    member inline s.Join((Cont c2), _: System.Func< ^T, int>, _: System.Func< ^U, int>, f: System.Func< ^T, ^U, ^V>) : Cont< ^R, ^V> =
        let (Cont c1) = s
        Cont (fun k -> c1 (fun a -> c2 (fun b -> k (f.Invoke(a, b)))))

// Monad
    member inline s.SelectMany(f: System.Func< ^T, Cont< ^R, ^U>>) : Cont< ^R, ^U> =
        let (Cont c) = s
        Cont (fun k -> c (fun a -> let (Cont cc) = f.Invoke(a) in cc k))

    member inline s.SelectMany(f: System.Func< ^T, Cont< ^R, ^U>>, g: System.Func< ^T, ^U, ^V>) : Cont< ^R, ^V> =
        let (Cont c) = s
        Cont (fun k -> c (fun a -> let (Cont cc) = f.Invoke(a) in cc (fun b -> k (g.Invoke(a, b)))))

// Comonad
    member inline s.ContinueWith(f: System.Func<Cont< ^R, ^T>, ^U>) : Cont< ^R, ^U> =
        let s = s in Cont (fun k -> k (f.Invoke(s)))