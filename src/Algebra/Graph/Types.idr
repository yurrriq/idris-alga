module Algebra.Graph.Types


%access public export


data Graph : Type -> Type where

  ||| The empty graph.
  Empty : Graph a
  
  ||| The single-vertex graph.
  Vertex : a -> Graph a
  
  ||| The composition of two graphs, i.e. the union of their vertices and edges.
  Overlay : Graph a -> Graph a -> Graph a

  ||| Like `Overlay` with edges between vertices of the two graphs.
  Connect : Graph a -> Graph a -> Graph a
