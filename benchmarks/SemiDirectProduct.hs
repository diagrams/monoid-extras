{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Criterion.Main

#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid
import           Data.Word
#else
import           Data.Monoid (Sum(..))
#endif
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup (Semigroup)
#endif

import           Data.Monoid.Action
import qualified Data.Monoid.SemiDirectProduct        as L
import qualified Data.Monoid.SemiDirectProduct.Strict as S

newtype MyMonoid = MyMonoid (Sum Word) deriving (Semigroup, Monoid)

instance Action MyMonoid () where
  act _ = id
  {-# NOINLINE act #-}

main :: IO ()
main = defaultMain
       [ bench "mconcat/strict"   $ whnf mconcat strict
       , bench "mconcat/lazy"     $ whnf mconcat lazy
       , bench "strict/quotient"  $ whnf (S.quotient . mconcat) strict
       , bench "lazy/quotient"    $ whnf (L.quotient . mconcat) lazy
       ]
  where strict :: [S.Semi () MyMonoid]
        strict =  map (S.embed . MyMonoid . Sum) $ take 1000 [1..]
        lazy   :: [L.Semi () (MyMonoid)]
        lazy   =  map (L.embed . MyMonoid . Sum) $ take 1000 [1..]
