module Algebra.Graph.API

import Prelude
import Prelude.Show
import public Algebra.Graph.Types


||| The empty graph.
empty : Graph a
empty = E


||| Construct a graph with a single vertex.
vertex : a -> Graph a
vertex = V


overlay : Graph a -> Graph a -> Graph a
overlay = (+)


connect : Graph a -> Graph a -> Graph a
connect = (*)


edge : a -> a -> Graph a
edge x y = connect (vertex x) (vertex y)


overlays : List (Graph a) -> Graph a
overlays = foldr overlay empty


connects : List (Graph a) -> Graph a
connects = foldr connect empty


||| Construct a graph that contains a given collection of isolated vertices.
vertices : List a -> Graph a
vertices = overlays . map vertex


edges : List (a, a) -> Graph a
edges = overlays . map (uncurry edge)


graph : (vertices : List a) -> (edges : List (a,a)) -> Graph a
graph vs es = overlay (vertices vs) (edges es)


foldg : b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
foldg e v o c = go
  where
    go : Graph a -> b
    go E       = e
    go (V x)   = v x
    go (x + y) = o (go x) (go y)
    go (x * y) = c (go x) (go y)


path : List a -> Graph a
path []        = empty
path [x]       = vertex x
path (x :: xs) = edges (zip (x :: xs) xs)


circuit : List a -> Graph a
circuit []        = empty
circuit (x :: xs) = path ([ x ] ++ xs ++ [ x ])


||| Construct a fully connected graph on a given collection of vertices.
clique : List a -> Graph a
clique = connects . map vertex


biclique : List a -> List a -> Graph a
biclique xs ys = connect (vertices xs) (vertices ys)


star : a -> List a -> Graph a
star x ys = connect (vertex x) (vertices ys)


close : Graph a -> Graph a
close E       = empty
close (V x)   = vertex x
close (x + y) = x * y
close (x * y) = x * y
