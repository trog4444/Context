namespace PTR.Context.Incomplete.Extension


module Operators =

    open PTR.Context.Type

    type Maybe<'T> with
    
        static member inline Wrap x = Just x
        
        member inline s.Bind k =
            match s with Nothing -> Nothing | Just a -> k a
        
        member inline s.Map2 (struct (t, f)) =
            match s with Nothing -> Nothing | Just a -> match t with Nothing -> Nothing | Just b -> Just (f a b)
        
        member inline s.Map f =
            match s with Nothing -> Nothing | Just a -> Just (f a)
        
        member inline s.OrElse t = match s with Nothing -> t | Just _ -> s
    
        static member inline Append (struct (s, t)) =
            match s with
            | Nothing -> t
            | Just a  -> match t with
                         | Nothing -> s
                         | Just b  -> Just ((^A: (static member Append: ^A -> ^A -> ^A) (a, b)))
    
        static member inline Empty () = Nothing


    let inline ( >>= ) m k =
        (^Ta: (member Bind: (^a -> ^Tb) -> ^Tb) (m, k))

    let inline ( =<< ) k m = m >>= k

    let inline ( <*> ) ff fv =
        (^Tf: (member Map2: struct (^Ta * ((^a -> ^b) -> ^a -> ^b)) -> ^Tb) (ff, (fv, fun f a -> f a)))

    let inline ( <**> ) fv ff = ff <*> fv

    let inline ( *> ) fa fb = 
        (^Ta: (member Map2: struct (^Tb * (^a -> ^b -> ^b)) -> ^Tb) (fa, (fb, fun _ b -> b)))

    let inline ( <* ) fb fa = fa *> fb

    let inline ( <|> ) f1 f2 =
        (^T: (member OrElse: ^T -> ^T) (f1, f2))

    let inline ( <||> ) f2 f1 = f1 <|> f2

    let inline ( |%> ) fa f =
        (^Ta: (member Map: (^a -> ^b) -> ^Tb) (fa, f))

    let inline ( <%| ) f fa = fa |%> f

    let inline ( %> ) fa b = fa |%> fun _ -> b

    let inline ( <% ) b fa = fa %> b

    let inline ( ++ ) e1 e2 =
        (^T: (static member Append: ^T -> ^T -> ^T) (e1, e2))

