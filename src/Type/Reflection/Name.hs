{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Utilities for controlling how to serialize type representations into strings
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

This module provides three different ways of serializing a 'TypeRep' into a 'Text':
qualifying no names, qualifying all names, or package-qualifying all names. In order
to support meaningful notions of equality on these string representations (for e.g.
using them as keys in a map), @newtype@s are provided for each format.

-}

module Type.Reflection.Name (
  -- * Unqualified names
  Unqualified(..), getUnqualified, showUnqualified,

  -- * Qualified names
  Qualified(..), getQualified, showQualified,

  -- * Package-qualified names
  PackageQualified(..), getPackageQualified, showPackageQualified,

  -- * Class-based access
  TypeText(..), defaultRenderTypeRep,

  ) where

import Data.String ( IsString(..) )
import Data.Hashable ( Hashable )
import Data.Text ( Text )

import Type.Reflection

------------------------------------------------------
-- TypeText, a class that describes how TyCons are rendered

-- | The 'TypeText' class describes how a 'TyCon' is rendered
-- into a textual representation, and it is used to render a 'TypeRep'
-- by 'renderTypeRep'. The 'IsString' superclass is needed to insert
-- connectives like spaces, dots, and parentheses, and the 'Monoid'
-- superclass is needed to stitch these pieces together.
--
-- Only 'renderTyCon' is needed for instances; if 'renderTypeRep' is
-- omitted, then the default rendering, via 'defaultRenderTypeRep' is
-- used. See that function for more details.
--
-- The only consistency law for 'TypeText' is this, holding for all
-- @tr@ of type 'TypeRep' :
--
-- > case tr of Con' tc [] -> renderTypeRep tr == renderTyCon tc
-- >            _ -> True
--
-- In other words, rendering a 'TypeRep' consisting only of a 'TyCon'
-- is the same as rendering the 'TyCon' itself. If the 'TypeRep' is not
-- a plain 'TyCon' (with no kind arguments), then there are no applicable
-- laws. Note that the law uses '==', even though 'Eq' is not a superclass
-- of 'TypeText'.
class (IsString tt, Monoid tt, Ord tt) => TypeText tt where
  renderTyCon :: TyCon -> tt

  renderTypeRep :: TypeRep t -> tt
  renderTypeRep = defaultRenderTypeRep

  {-# MINIMAL renderTyCon #-}

------------------------------------------------------
-- Unqualified

-- | A rendering of a type where no components have module or package qualifications.
-- Data constructors used in types are preceded with a @'@, and infix operator types
-- are printed without parentheses. Uses 'defaultRenderTypeRep' to render types; see
-- that function for further details.
newtype Unqualified = MkUnqualified Text
  deriving (Eq, Ord, Show, Hashable, Semigroup, Monoid, IsString)

instance TypeText Unqualified where
  renderTyCon = fromString . tyConName

-- | Extract the contents of an 'Unqualified' rendering
getUnqualified :: Unqualified -> Text
getUnqualified (MkUnqualified str) = str

-- | Convert a 'TypeRep' into an 'Unqualified' representation
showUnqualified :: TypeRep t -> Unqualified
showUnqualified = renderTypeRep

----------------------------------------------------------
-- Qualified

-- | A rendering of a type where all components have module qualifications.
-- Data constructors used in types are preceded first with their module qualification,
-- and then with a @'@, and infix operator types
-- are printed without parentheses. So we have @Data.Proxy.'Proxy@ for the v'Proxy' constructor.
-- Uses 'defaultRenderTypeRep' to render types; see
-- that function for further details.j
--
-- Note that module qualifications arise from the module that defines a type, even if this
-- is a hidden or internal module. This fact means that internal-structure changes in
-- your libraries may affect how your types are rendered, including changes in e.g. @base@.
newtype Qualified = MkQualified Text
  deriving (Eq, Ord, Show, Hashable, Semigroup, Monoid, IsString)

instance TypeText Qualified where
  renderTyCon tc = fromString (tyConModule tc) <> "." <> fromString (tyConName tc)

-- | Extract the contents of a 'Qualified' rendering
getQualified :: Qualified -> Text
getQualified (MkQualified str) = str

-- | Convert a 'TypeRep' into a 'Qualified' representation
showQualified :: TypeRep t -> Qualified
showQualified = renderTypeRep

------------------------------------------------------------
-- PackageQualified

-- | A rendering of a type where all components have package and module qualifications
-- Data constructors used in types are preceded first with their package qualification,
-- and then their module qualification,
-- and then with a @'@, and infix operator types
-- are printed without parentheses. So we have @base.Data.Proxy.'Proxy@ for the v'Proxy' constructor.
-- Uses 'defaultRenderTypeRep' to render types; see
-- that function for further details.
--
-- Note that module qualifications arise from the module that defines a type, even if this
-- is a hidden or internal module. This fact means that internal-structure changes in
-- your libraries may affect how your types are rendered, including changes in e.g. @base@.
newtype PackageQualified = MkPackageQualified Text
  deriving (Eq, Ord, Show, Hashable, Semigroup, Monoid, IsString)

instance TypeText PackageQualified where
 renderTyCon tc = fromString (tyConPackage tc) <> "." <>
                  fromString (tyConModule tc) <> "." <>
                  fromString (tyConName tc)

-- | Extract the contents of a 'PackageQualified' name
getPackageQualified :: PackageQualified -> Text
getPackageQualified (MkPackageQualified str) = str

-- | Convert a 'TypeRep' into a 'PackageQualified' representation
showPackageQualified :: TypeRep t -> PackageQualified
showPackageQualified = renderTypeRep

----------------------------------------------
-- Worker

-- | Render a 'TypeRep' into an instance of 'TypeText'. This follows the following
-- rules for how to render a type:
--
-- * Function types are rendered infix. If a function takes another function as an
-- argument, the argument function is rendered in parentheses (as required by the
-- right-associative @->@ operator).
--
-- * All other type constructors are rendered prefix, including the list constructor @[]@
-- and the tuple constructors e.g. @(,,)@, along with any other type operators.
--
-- * All kind arguments to type constructors are rendered, with \@ prefixes. Any
-- non-atomic (that is, not simply a type constructor with no kind arguments) kinds
-- are enclosed in parentheses.
--
-- * Non-atomic arguments are enclosed in parentheses.
--
-- * Type synonyms are always expanded. This means that we get @[] Char@, not @String@,
-- and we get @TYPE ('BoxedRep 'Lifted)@, not @Type@.
defaultRenderTypeRep ::
  forall tn outer_t.
  TypeText tn =>
  TypeRep outer_t -> tn
defaultRenderTypeRep = go
  where
    go :: TypeRep t -> tn
    go (Fun arg@(Fun {}) res) = "(" <> go arg <> ") -> " <> go res
    go (Fun arg res) = go arg <> " -> " <> go res
    go (App t1 t2@(Con' _ [])) = go t1 <> " " <> go t2
    go (App t1 t2) = go t1 <> " (" <> go t2 <> ")"
    go (Con' tc kinds) = renderTyCon tc <> foldMap go_kind kinds

    go_kind :: SomeTypeRep -> tn
    go_kind (SomeTypeRep kind_rep@(Con' _ [])) = " @" <> go kind_rep
    go_kind (SomeTypeRep kind_rep) = " @(" <> go kind_rep <> ")"
