namespace PTR.Context.Operators


[<AutoOpen>]
module ContextOperators =

    let inline ( >>= ) m f =
        (^``Monad<a>``: (member SelectMany: System.Func< ^a, ^``Monad<b>``> -> ^``Monad<b>``)
            (m, System.Func<_,_>f))
    
    let inline ( =<< ) (f: ^a -> ^``Monad<b>``) (m: ^``Monad<a>``)
        : ^``Monad<b>`` = m >>= f


    let inline ( <*> ) ff fv =
        (^``Applicative<a -> b>``: (member Select2: ^``Applicative<a>`` * System.Func<(^a -> ^b), ^a, ^b> -> ^``Applicative<b>``)
            (ff, fv, System.Func<_,_,_>(fun f a -> f a)))
    
    let inline ( <**> ) (fv: ^``Applicative<a>``) (ff: ^``Applicative<a -> b>``)
        : ^``Applicative<b>`` = ff <*> fv


    let inline ( *> ) fa fb =
        (^``Applicative<a>``:
            (member Select2: ^``Applicative<b>`` * System.Func< ^a, ^b, ^b> -> ^``Applicative<b>``)
                (fa, fb, System.Func<_,_,_>(fun _ b -> b)))
        
    let inline ( <* ) fb (fa: ^``Applicative<a>``) : ^``Applicative<b>`` = fa *> fb


    let inline ( <|> ) f1 f2 =
        (^``Alternative<a>``:
            (member OrElse: ^``Alternative<a>`` -> ^``Alternative<a>``) (f1, f2))
        
    let inline ( <||> ) f2 f1 : ^``Alternative<a>`` = f1 <|> f2



    let inline ( |%> ) fa f =
        (^``Functor<a>``:
            (member Select: System.Func< ^a, ^b> -> ^``Functor<b>``) (fa, System.Func<_,_>f))
    
    let inline ( <%| ) (f: ^a -> ^b) (fa: ^``Functor<a>``) : ^``Functor<b>`` = fa |%> f


    let inline ( %> ) (fa: ^``Functor<a>``) (b: ^b) : ^``Functor<b>`` = fa |%> fun _ -> b

    let inline ( <% ) (b: ^b) (fa: ^``Functor<a>``) : ^``Functor<b>`` = fa %> b



    let inline ( =>> ) w j =
        (^``Comonad<a>``:
            (member ContinueWith: System.Func< ^``Comonad<a>``, ^b> -> ^``Comonad<b>``)
                (w, System.Func<_,_>j))
      
    let inline ( <<= ) (j: ^``Comonad<a>`` -> ^b) w : ^``Comonad<b>`` = w =>> j



    let inline ( ++ ) e1 e2 =
        (^``Semigroup<a>``:
            (static member Append: ^``Semigroup<a>`` -> ^``Semigroup<a>`` -> ^``Semigroup<a>``) (e1, e2))