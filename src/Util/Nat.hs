{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Util.Nat
  ( Nat(..)
  , SNat
  ) where

import Protolude hiding (Nat)

import Data.Singletons.TH (singletons)
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Bool

$(singletons [d|
  data Nat = Zero | Succ Nat deriving (Eq)
  |])
