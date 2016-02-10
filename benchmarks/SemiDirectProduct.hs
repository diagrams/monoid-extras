{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           Control.Monad
import           Criterion.Main

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

import Data.Word

import           Data.Monoid.Action
import qualified Data.Monoid.SemiDirectProduct        as L
import qualified Data.Monoid.SemiDirectProduct.Strict as S

instance Action (Sum Word) () where
  act _ = id
  {-# NOINLINE act #-}

main :: IO ()
main = defaultMain
       [ bench "mconcat/strict"   $ whnf mconcat strict
       , bench "mconcat/lazy"     $ whnf mconcat lazy
       , bench "strict/quotient"  $ whnf (S.quotient . mconcat) strict
       , bench "lazy/quotient"    $ whnf (L.quotient . mconcat) lazy
       ]
  where strict :: [S.Semi () (Sum Word)]
        strict =  map (S.embed . Sum) $ take 1000 [1..]
        lazy   :: [L.Semi () (Sum Word)]
        lazy   =  map (L.embed . Sum) $ take 1000 [1..]
