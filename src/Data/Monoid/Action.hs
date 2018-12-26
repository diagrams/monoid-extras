{-# LANGUAGE ConstraintKinds #-}

module Data.Monoid.Action
  ( Action
  , act
  ) where

import Data.Monoid.Action.LeftAction

type Action = LeftAction

act :: Action m s => m -> s -> s
act = leftAct
