module Prelude

%access public export

infixr 20 ::

data List : Type -> Type where

    ||| The empty list.
    Nil  : List a

    ||| A nonempty list, consisting of a head element and the rest of the list.
    (::) : a -> List a -> List a


infixr 15 ++

(++) : List a -> List a -> List a
[] ++ ys        = ys
(x :: xs) ++ ys = x :: (xs ++ ys)


%access export

infixr 9 .

(.) : (b -> c) -> (a -> b) -> a -> c
f . g = \x => f (g x)


uncurry : (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b


foldr : (a -> b -> b) -> b -> List a -> b
foldr _ z []        = z
foldr f z (x :: xs) = f x (foldr f z xs)


map : (a -> b) -> List a -> List b
map _ []        = []
map f (x :: xs) = f x :: map f xs


zip : List a -> List b -> (List (a, b))
zip [] _                = []
zip _ []                = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys
