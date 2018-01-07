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


overlays : List (Graph a) -> Graph a
overlays = foldr overlay empty


connects : List (Graph a) -> Graph a
connects = foldr connect empty


vertices : List a -> Graph a
vertices = overlays . map vertex


edges : List (a, a) -> Graph a
edges = overlays . map (uncurry edge)


graph : List a -> List (a,a) -> Graph a
graph vs es = overlay (vertices vs) (edges es)


clique : List a -> Graph a
clique = connects . map vertex
