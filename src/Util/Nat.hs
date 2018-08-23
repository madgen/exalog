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
{-# LANGUAGE InstanceSigs #-}

module Util.Nat
  ( Nat(..)
  , SNat
  , Sing(..)
  ) where

import Protolude hiding (Nat)

import Data.Singletons.TH
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Show
import Data.Singletons.Prelude.Bool

$(singletons [d|
  data Nat = Zero | Succ Nat deriving (Eq, Show)
  |])
