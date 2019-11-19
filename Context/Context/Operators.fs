namespace Rogz.Context.Extra.Operators


[<AutoOpen>]
module ContextOperators =

    let inline ( >>= ) m f =
        (^``Monad<a>``: (member SelectMany: System.Func< ^a, ^``Monad<b>``> -> ^``Monad<b>``)
            (m, System.Func<_,_>f))
    
    let inline ( =<< ) (f: ^a -> ^``Monad<b>``) (m: ^``Monad<a>``)
        : ^``Monad<b>`` = m >>= f

    let inline ( >=> ) (f: ^a -> ^``Monad<b>``) (g: ^b -> ^``Monad<c>``) a = f a >>= g

    let inline ( <=< ) (g: ^b -> ^``Monad<c>``) (f: ^a -> ^``Monad<b>``) = f >=> g

    let inline ( ?> ) m p =
        (^``MonadPlus<a>``: (member Where: System.Func< ^a, bool> -> ^``MonadPlus<a>``)
            (m, System.Func<_,_>p))

    let inline ( <? ) (p: ^a -> bool) (m: ^``MonadPlus<a>``) : ^``MonadPlus<a>`` = m ?> p


    let inline ( <*> ) ff fv =
        (^``Applicative<a -> b>``: (member Join: ^``Applicative<a>`` * System.Func<(^a -> ^b), ^a, ^b> -> ^``Applicative<b>``)
            (ff, fv, System.Func<_,_,_>(fun f a -> f a)))
    
    let inline ( <**> ) (fv: ^``Applicative<a>``) (ff: ^``Applicative<a -> b>``)
        : ^``Applicative<b>`` = ff <*> fv


    let inline ( *> ) fa fb =
        (^``Applicative<a>``:
            (member Join: ^``Applicative<b>`` * System.Func< ^a, ^b, ^b> -> ^``Applicative<b>``)
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



    let inline ( =>> ) w f =
        (^``Comonad<a>``:
            (member ContinueWith: System.Func< ^``Comonad<a>``, ^b> -> ^``Comonad<b>``)
                (w, System.Func<_,_>f))
      
    let inline ( <<= ) (f: ^``Comonad<a>`` -> ^b) w : ^``Comonad<b>`` = w =>> f

    let inline ( =>= ) (f: ^``Comonad<a>`` -> ^b) (g: ^``Comonad<b>`` -> ^c) w = g (w =>> f)

    let inline ( =<= ) (g: ^``Comonad<b>`` -> ^c) (f: ^``Comonad<a>`` -> ^b) = f =>= g



    let inline ( ++ ) e1 e2 =
        (^``Semigroup<a>``:
            (static member Append: ^``Semigroup<a>`` -> ^``Semigroup<a>`` -> ^``Semigroup<a>``) (e1, e2))