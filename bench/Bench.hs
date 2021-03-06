{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

-- mostly for fget_qux0
{-# LANGUAGE TypeOperators, ExplicitNamespaces #-}

module Main (main) where

import Data.Vinyl (Rec(..), FieldRec, type (∈))

import ProxySymbolTH -- stage restriction

import Criterion.Main
import qualified Data.Hetero.Dict as D
import qualified Data.Hetero.DynDict as DD
import Data.HVect (HVect(..), (!!), SNat(..))
import Data.Hetero.KVList
import Prelude hiding ((!!))

main :: IO ()
main = defaultMain
    [ bgroup "n = 3"  small
    , bgroup "n = 15" large
    ]

small :: [Benchmark]
small =
    [ bench "Index Dict"    $ nf (D.get [key|qux0|]) dict
    , bench "Index DynDict" $ nf (DD.get [key|qux0|]) dynDict
    , bench "Index HVect"   $ nf ((SSucc SZero) !!) hvect
    , bench "Index Vinyl"   $ nf (fget_qux0) vinyl
    ]
  where

    vinyl
        = field [ps|foo0|] (1 :: Int)
       :& field [ps|bar0|] "bar"
       :& field [ps|qux0|] True
       :& RNil

    getAllVinyl d =
      ( fget [ps|foo0|] d :: Int
       --NOTE the annotations are unnecessary with:
       -- type Bar0 = '("bar0",String)
       -- fget [pr|Bar0|] -- pr from Data.Tagged.TH
      , fget [ps|bar0|] d :: String
      , fget [ps|qux0|] d :: Bool
      )

    hvect = (1 :: Int)  :&: (1 :: Int)
                        :&: "bar"
                        :&: True
                        :&: HNil

    getAllDict d =
        ( D.get [key|foo0|] d
        , D.get [key|bar0|] d
        , D.get [key|qux0|] d
        )

    dict = D.mkDict . D.add [key|foo0|] (1 :: Int)
                    . D.add [key|bar0|] "bar"
                    . D.add [key|qux0|] True
                    $ D.emptyStore

    getAllDynDict d =
        ( DD.get [key|foo0|] d
        , DD.get [key|bar0|] d
        , DD.get [key|qux0|] d
        )

    getAllHVect v =
        ( ((SZero)!!) v
        , ((SSucc SZero)!!) v
        , ((SSucc $ SSucc SZero)!!) v
        )

    dynDict = DD.add [key|foo0|] (1 :: Int)
            . DD.add [key|bar0|] "bar"
            . DD.add [key|qux0|] True
            $ DD.empty

large :: [Benchmark]
large =
    [ bench "Index Dict"    $ nf (D.get [key|qux0|]) dict
    , bench "Index DynDict" $ nf (DD.get [key|qux0|]) dynDict
    , bench "Index HVect"   $ nf ((SSucc SZero)!!) hvect
    , bench "Index Vinyl"   $ nf (fget_qux0) vinyl

    , bench "Modify DynDict" $ nf (DD.get [key|qux0|] . DD.modify [key|qux0|] not) dynDict
    , bench "Modify Vinyl"   $ nf (fget_qux0 . fmodify [ps|qux0|] not) vinyl
    ]

  where

    getAllVinyl d = (
        ( fget [ps|foo0|] d :: Int
        , fget [ps|foo1|] d :: Int
        , fget [ps|foo2|] d :: Int
        , fget [ps|foo3|] d :: Int
        , fget [ps|foo4|] d :: Int
        ),
        ( fget [ps|bar0|] d :: String
        , fget [ps|bar1|] d :: String
        , fget [ps|bar2|] d :: String
        , fget [ps|bar3|] d :: String
        , fget [ps|bar4|] d :: String
        ),
        ( fget [ps|qux0|] d :: Bool
        , fget [ps|qux1|] d :: Bool
        , fget [ps|qux2|] d :: Bool
        , fget [ps|qux3|] d :: Bool
        , fget [ps|qux4|] d :: Bool
        ))

    vinyl
        = field [ps|foo0|] (1 :: Int)
       :& field [ps|foo1|] (1 :: Int)
       :& field [ps|foo2|] (1 :: Int)
       :& field [ps|foo3|] (1 :: Int)
       :& field [ps|foo4|] (1 :: Int)
       :& field [ps|bar0|] "bar"
       :& field [ps|bar1|] "bar"
       :& field [ps|bar2|] "bar"
       :& field [ps|bar3|] "bar"
       :& field [ps|bar4|] "bar"
       :& field [ps|qux0|] True
       :& field [ps|qux1|] True
       :& field [ps|qux2|] True
       :& field [ps|qux3|] True
       :& field [ps|qux4|] True
       :& RNil


    getAllDict d = (
        ( D.get [key|foo0|] d
        , D.get [key|foo1|] d
        , D.get [key|foo2|] d
        , D.get [key|foo3|] d
        , D.get [key|foo4|] d
        ),
        ( D.get [key|bar0|] d
        , D.get [key|bar1|] d
        , D.get [key|bar2|] d
        , D.get [key|bar3|] d
        , D.get [key|bar4|] d
        ),
        ( D.get [key|qux0|] d
        , D.get [key|qux1|] d
        , D.get [key|qux2|] d
        , D.get [key|qux3|] d
        , D.get [key|qux4|] d
        ))

    getAllDynDict d = (
        ( DD.get [key|foo0|] d
        , DD.get [key|foo1|] d
        , DD.get [key|foo2|] d
        , DD.get [key|foo3|] d
        , DD.get [key|foo4|] d
        ),
        ( DD.get [key|bar0|] d
        , DD.get [key|bar1|] d
        , DD.get [key|bar2|] d
        , DD.get [key|bar3|] d
        , DD.get [key|bar4|] d
        ),
        ( DD.get [key|qux0|] d
        , DD.get [key|qux1|] d
        , DD.get [key|qux2|] d
        , DD.get [key|qux3|] d
        , DD.get [key|qux4|] d
        ))

    getAllHVect v = (
        ( ((SZero)!!) v
        , ((SSucc SZero)!!) v
        , ((SSucc $ SSucc SZero)!!) v
        , ((SSucc $ SSucc $ SSucc SZero)!!) v
        , ((SSucc $ SSucc $ SSucc $ SSucc SZero)!!) v
        ),
        ( ((SSucc $ SSucc $ SSucc $ SSucc $ SSucc SZero)!!) v
        , ((SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc SZero)!!) v
        , ((SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc SZero)!!) v
        , ((SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc SZero)!!) v
        , ((SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc SZero)!!) v
        ),
        ( ((SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $SSucc $ SSucc SZero)!!) v
        , ((SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc SZero)!!) v
        , ((SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc SZero)!!) v
        , ((SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc SZero)!!) v
        , ((SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc SZero)!!) v
        ))

    hvect = (1 :: Int)  :&: (1 :: Int)
                        :&: (1 :: Int)
                        :&: (1 :: Int)
                        :&: (1 :: Int)
                        :&: (1 :: Int)
                        :&: "bar"
                        :&: "bar"
                        :&: "bar"
                        :&: "bar"
                        :&: "bar"
                        :&: True
                        :&: True
                        :&: True
                        :&: True
                        :&: True
                        :&: HNil


    dict = D.mkDict . D.add [key|foo0|] (1 :: Int)
                    . D.add [key|foo1|] (1 :: Int)
                    . D.add [key|foo2|] (1 :: Int)
                    . D.add [key|foo3|] (1 :: Int)
                    . D.add [key|foo4|] (1 :: Int)
                    . D.add [key|bar0|] "bar"
                    . D.add [key|bar1|] "bar"
                    . D.add [key|bar2|] "bar"
                    . D.add [key|bar3|] "bar"
                    . D.add [key|bar4|] "bar"
                    . D.add [key|qux0|] True
                    . D.add [key|qux1|] True
                    . D.add [key|qux2|] True
                    . D.add [key|qux3|] True
                    . D.add [key|qux4|] True
                    $ D.emptyStore

    dynDict = DD.add [key|foo0|] (1 :: Int)
            . DD.add [key|foo1|] (1 :: Int)
            . DD.add [key|foo2|] (1 :: Int)
            . DD.add [key|foo3|] (1 :: Int)
            . DD.add [key|foo4|] (1 :: Int)
            . DD.add [key|bar0|] "bar"
            . DD.add [key|bar1|] "bar"
            . DD.add [key|bar2|] "bar"
            . DD.add [key|bar3|] "bar"
            . DD.add [key|bar4|] "bar"
            . DD.add [key|qux0|] True
            . DD.add [key|qux1|] True
            . DD.add [key|qux2|] True
            . DD.add [key|qux3|] True
            . DD.add [key|qux4|] True
            $ DD.empty

fget_qux0 :: ('("qux0",Bool) ∈ fields) => FieldRec fields -> Bool
fget_qux0 = fget [ps|qux0|]
