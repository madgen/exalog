module Language.Exalog.Tuples
  ( Tuples
  , isEmpty
  ) where

import Protolude

import Language.Exalog.Core
import Util.Vector

type Tuples n = [ Vector n Sym ]

isEmpty :: Tuples n -> Bool
isEmpty = null
