{- splitfuns - splitting function definitions over multiple modules
Copyright (C) 2021 Jaro Reinders

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Haskell.TH.Serialize where

import Data.Serialize
import Language.Haskell.TH.Syntax

instance Serialize Name
instance Serialize NameFlavour
instance Serialize PkgName
instance Serialize NameSpace
instance Serialize ModName
instance Serialize OccName
instance Serialize Clause
instance Serialize Pat
instance Serialize Exp
instance Serialize Range
instance Serialize Match
instance Serialize Body
instance Serialize Guard
instance Serialize Stmt
instance Serialize Dec
instance Serialize PatSynDir
instance Serialize PatSynArgs
instance Serialize Role
instance Serialize TypeFamilyHead
instance Serialize InjectivityAnn
instance Serialize FamilyResultSig
instance Serialize TySynEqn
instance Serialize Pragma
instance Serialize AnnTarget
instance Serialize RuleBndr
instance Serialize Phases
instance Serialize RuleMatch
instance Serialize Inline
instance Serialize Fixity
instance Serialize FixityDirection
instance Serialize Foreign
instance Serialize Safety
instance Serialize Callconv
instance Serialize Overlap
instance Serialize FunDep
instance Serialize DerivClause
instance Serialize DerivStrategy
instance Serialize Con
instance Serialize Bang
instance Serialize SourceUnpackedness
instance Serialize SourceStrictness
instance Serialize Lit
instance Serialize Bytes where
  put = error "TODO define Serialize for Bytes"
  get = error "TODO define Serialize for Bytes"
instance Serialize Type
instance Serialize TyVarBndr
instance Serialize TyLit

