{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
import TupleTH
import Criterion.Main
import Control.DeepSeq

data Vertex = A | B | C | D deriving(Eq)

allVertices = [A,B,C,D]
allVertices' = (A,B,C,D)

data Facet = ABC | ABD | ACD | BCD

allFacets = [ABC,ABD,ACD,BCD]
allFacets' = (ABC,ABD,ACD,BCD)

instance NFData Facet where
    rnf f = seq f ()


vertices f = case f of
                ABC -> (A,B,C)
                ABD -> (A,B,D)
                ACD -> (A,C,D)
                BCD -> (B,C,D)

isSubface v f = case vertices f of
                     (v1,v2,v3) -> v==v1 || v==v2 || v==v3

facetsContainingVertex v = filter (v `isSubface`) allFacets

benchList = bench "Lists" (nf (map facetsContainingVertex) allVertices) 

facetsContainingVertex' v = $(filterTupleE 4) (v `isSubface`) allFacets' 

benchTuple = bench "Tuples"  (nf ($(mapTupleE 4) facetsContainingVertex') allVertices')

main = defaultMain [benchTuple, benchList]
