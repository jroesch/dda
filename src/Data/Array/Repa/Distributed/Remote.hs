{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, EmptyDataDecls #-}
module Data.Array.Repa.Distributed.Remote where

import Data.Array.Repa
import Data.Array.Repa.Eval
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Debug.Trace
import GHC.Exts

data R repr

instance (Source r e) => Source (R r) e where
    data Array (R repr) sh a = Bogus

    index = undefined

    linearIndex = undefined

    extent = undefined

    deepSeqArray = undefined

{- instance (Source (R repr) e, Shape sh) => Load (R repr) sh e where
    loadS = undefined
    loadP = undefined -}

