module Algebra.Graph.API


import public Algebra.Graph.Types


empty : Graph a
empty = Empty


vertex : a -> Graph a
vertex = Vertex


overlay : Graph a -> Graph a -> Graph a
overlay = Overlay


connect : Graph a -> Graph a -> Graph a
connect = Connect


edge : a -> a -> Graph a
edge x y = connect (vertex x) (vertex y)


overlays : Foldable t =>  t (Graph a) -> Graph a
overlays = foldr overlay empty


connects : Foldable t => t (Graph a) -> Graph a
connects = foldr connect empty


||| Construct a graph that contains a given collection of isolated vertices.
vertices : (Foldable t, Functor t) => t a -> Graph a
vertices = overlays . map vertex


edges : (Foldable t, Functor t) => t (a, a) -> Graph a
edges = overlays . map (uncurry edge)


graph : (Foldable t, Functor t) => t a -> t (a,a) -> Graph a
graph vs es = overlay (vertices vs) (edges es)


||| Construct a fully connected graph on a given collection of vertices.
clique : (Foldable t, Functor t) => t a -> Graph a
clique = connects . map vertex
