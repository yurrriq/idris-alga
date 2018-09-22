module Algebra.Graph

import Prelude.Show
import Prelude.Strings

%access public export


infixl 8 +
infixl 9 *

data Graph : Type -> Type where

    ||| The empty graph.
    E : Graph a

    ||| The single-vertex graph.
    V : a -> Graph a

    ||| The composition of two graphs, i.e. the union of their vertices and edges.
    (+) : Graph a -> Graph a -> Graph a

    ||| Like `Overlay` with edges between vertices of the two graphs.
    (*) : Graph a -> Graph a -> Graph a


implementation Show a => Show (Graph a) where

    show E       = "{}"
    show (V x)   = show x
    show (x + y) = show x ++ "+" ++ show y
    show (x * y) = show x ++ "---" ++ show y
