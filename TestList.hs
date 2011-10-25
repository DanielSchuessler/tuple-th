{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -ddump-splices #-}


module TestList(evens) where


numbers = [0::Int,1,2,3]

evens :: [Int]
evens = filter even numbers


