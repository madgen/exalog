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

$(singletons [d|
  data Nat = Zero | Succ Nat deriving (Eq, Ord, Show)
  |])
