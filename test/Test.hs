{-# LANGUAGE OverloadedStrings, DataKinds, MagicHash #-}

module Main where

import Type.Reflection.Name
import Type.Reflection.List

import Test.Tasty
import Test.Tasty.HUnit

import Type.Reflection
import Control.Exception
import Control.Monad ( void )

import Data.Proxy
import GHC.Exts

data a + b

main :: IO ()
main = defaultMain all_tests

all_tests :: TestTree
all_tests = testGroup "All"
  [ testGroup "unqualified"
    [ testCase "Int" $ showUnqualified (typeRep @Int) @?= "Int"
    , testCase "Maybe Bool" $ showUnqualified (typeRep @(Maybe Bool)) @?= "Maybe Bool"
    , testCase "[Double]" $ showUnqualified (typeRep @[Double]) @?= "[] Double"
    , testCase "Proxy 5" $ showUnqualified (typeRep @(Proxy 5)) @?= "Proxy @Natural 5"
    , testCase "'Proxy @5" $ showUnqualified (typeRep @('Proxy @5)) @?= "'Proxy @Natural @5"
    , testCase "(Int, Bool, Double)" $ showUnqualified (typeRep @(Int, Bool, Double)) @?= "(,,) Int Bool Double"
    , testCase "Char + ()" $ showUnqualified (typeRep @(Char + ())) @?= "+ @(TYPE ('BoxedRep 'Lifted)) @(TYPE ('BoxedRep 'Lifted)) Char ()"
    , testCase "Int -> Bool" $ showUnqualified (typeRep @(Int -> Bool)) @?= "Int -> Bool"
    , testCase "(Char -> Double) -> Int# -> Word#" $ showUnqualified (typeRep @((Char -> Double) -> Int# -> Word#)) @?= "(Char -> Double) -> Int# -> Word#"
    , testCase "Either (Maybe Bool) Double" $ showUnqualified (typeRep @(Either (Maybe Bool) Double)) @?= "Either (Maybe Bool) Double"
    ]
  , testGroup "qualified"
    [ testCase "Int" $ showQualified (typeRep @Int) @?= "GHC.Types.Int"
    , testCase "Maybe Bool" $ showQualified (typeRep @(Maybe Bool)) @?= "GHC.Maybe.Maybe GHC.Types.Bool"
    , testCase "[Double]" $ showQualified (typeRep @[Double]) @?= "GHC.Types.[] GHC.Types.Double"
    , testCase "Proxy 5" $ showQualified (typeRep @(Proxy 5)) @?= "Data.Proxy.Proxy @GHC.Num.Natural.Natural GHC.TypeLits.5"
    , testCase "'Proxy @5" $ showQualified (typeRep @('Proxy @5)) @?= "Data.Proxy.'Proxy @GHC.Num.Natural.Natural @GHC.TypeLits.5"
    , testCase "(Int, Bool, Double)" $ showQualified (typeRep @(Int, Bool, Double)) @?= "GHC.Tuple.(,,) GHC.Types.Int GHC.Types.Bool GHC.Types.Double"
    ]
  , testGroup "package qualified"
    [ testCase "Int" $ showPackageQualified (typeRep @Int) @?= "ghc-prim.GHC.Types.Int"
    , testCase "Maybe Bool" $ showPackageQualified (typeRep @(Maybe Bool)) @?= "base.GHC.Maybe.Maybe ghc-prim.GHC.Types.Bool"
    , testCase "[Double]" $ showPackageQualified (typeRep @[Double]) @?= "ghc-prim.GHC.Types.[] ghc-prim.GHC.Types.Double"
    , testCase "Proxy 5" $ showPackageQualified (typeRep @(Proxy 5)) @?= "base.Data.Proxy.Proxy @ghc-bignum.GHC.Num.Natural.Natural base.GHC.TypeLits.5"
    , testCase "'Proxy @5" $ showPackageQualified (typeRep @('Proxy @5)) @?= "base.Data.Proxy.'Proxy @ghc-bignum.GHC.Num.Natural.Natural @base.GHC.TypeLits.5"
    , testCase "(Int, Bool, Double)" $ showPackageQualified (typeRep @(Int, Bool, Double)) @?= "ghc-prim.GHC.Tuple.(,,) ghc-prim.GHC.Types.Int ghc-prim.GHC.Types.Bool ghc-prim.GHC.Types.Double"
    ]
    -- these functions can go wrong only by throwing exceptions, due to their precise types
    -- actually, these should use Control.DeepSeq.force, but neither TypeRep nor P.List support NFData.
    -- I've filed https://github.com/GaloisInc/parameterized-utils/issues/136 and
    -- https://github.com/haskell/deepseq/issues/82
  , testCase "typeRepListKind" $ void $ evaluate (typeRepListKind (typeRep @[Int, Bool, Double]))
  , testCase "typeRepList" $ void $ evaluate (typeRepList (typeRep @[Int, Bool, Double]))
  ]