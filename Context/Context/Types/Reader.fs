namespace Rogz.Context.Data.Reader

[<Struct; NoComparison; NoEquality>]
type Reader<'Env, 'T> = Reader of ('Env -> 'T) with

// Function
    member inline s.Invoke(env) = let (Reader r) = s in r env

// Functor
    member inline s.Select(f: System.Func< ^T, ^U>) =
        let (Reader r) = s in Reader (fun e -> f.Invoke(r e))

// Profunctor
    member inline s.DiSelect(f: System.Func< ^PreEnv, ^Env>, g: System.Func< ^T, ^U>) : Reader< ^PreEnv, ^U> =
        let (Reader r) = s in Reader (fun pe -> g.Invoke(r (f.Invoke(pe))))

// Applicative
    member inline s.Zip((Reader r2), f: System.Func< ^T, ^U, ^V>) =
        let (Reader r1) = s in Reader (fun e -> f.Invoke(r1 e, r2 e))

    member inline s.Join((Reader r2), _: System.Func< ^T, int>, _: System.Func< ^U, int>, f: System.Func< ^T, ^U, ^V>) =
        let (Reader r1) = s in Reader (fun e -> f.Invoke(r1 e, r2 e))

// Monad
    member inline s.SelectMany(f: System.Func< ^T, Reader< ^Env, ^U>>) =
        let (Reader r) = s in Reader (fun e -> let (Reader rr) = f.Invoke(r e) in rr e)

    member inline s.SelectMany(f: System.Func< ^T, Reader< ^Env, ^U>>, g: System.Func< ^T, ^U, ^V>) =
        let (Reader r) = s
        Reader (fun e -> let a = r e
                         let (Reader rr) = f.Invoke(a)
                         g.Invoke(a, rr e))

// Comonad
    member inline s.ContinueWith(f: System.Func<Reader< ^Env, ^T>, ^U>) : Reader< ^Env, ^U> =
        let s = s in Reader (fun _ -> f.Invoke(s))

// Semigroup
    static member inline Append((Reader r1), Reader r2) : Reader< ^e, ^a> =
        Reader (fun e -> (^a: (static member Append: ^a -> ^a -> ^a) (r1 e, r2 e)))