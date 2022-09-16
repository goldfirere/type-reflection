{-# LANGUAGE DataKinds, GADTs #-}

{-|
Description : Utilities for manipulating runtime representations of compile-time lists
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

Provides utilities for manipulating a 'TypeRep' of compile-time lists.

-}

module Type.Reflection.List (
  typeRepList, typeRepListKind,
  ) where

import Type.Reflection
import Data.SOP.NP

-- | Given a representation of a list, get a representation of the kind of the list's
-- elements.
typeRepListKind :: TypeRep @[k] xs -> TypeRep k
typeRepListKind tr
  | App _list tr_k <- typeRepKind tr
  = tr_k

-- | Turn a representation of a list into a list of representations.
typeRepList :: forall {k} (xs :: [k]). TypeRep xs -> NP TypeRep xs
typeRepList tr_list
  | App (App cons tr) trs <- tr_list
  , Just HRefl <- eqTypeRep cons (withTypeable tr_k $ typeRep @((:) @k))
  = tr :* typeRepList trs

  | Just HRefl <- eqTypeRep tr_list (withTypeable tr_k $ typeRep @('[] @k))
  = Nil

  | otherwise
  = error "list is neither (:) nor []."

  where
    tr_k = typeRepListKind tr_list
