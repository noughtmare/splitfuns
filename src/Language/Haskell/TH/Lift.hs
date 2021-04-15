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
{-# LANGUAGE DeriveLift, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Haskell.TH.Lift where

import Language.Haskell.TH.Syntax

deriving instance Lift Name
deriving instance Lift NameFlavour
deriving instance Lift PkgName
deriving instance Lift NameSpace
deriving instance Lift ModName
deriving instance Lift OccName
deriving instance Lift Clause
deriving instance Lift Pat
deriving instance Lift Exp
deriving instance Lift Range
deriving instance Lift Match
deriving instance Lift Body
deriving instance Lift Guard
deriving instance Lift Stmt
deriving instance Lift Dec
deriving instance Lift PatSynDir
deriving instance Lift PatSynArgs
deriving instance Lift Role
deriving instance Lift TypeFamilyHead
deriving instance Lift InjectivityAnn
deriving instance Lift FamilyResultSig
deriving instance Lift TySynEqn
deriving instance Lift Pragma
deriving instance Lift AnnTarget
deriving instance Lift RuleBndr
deriving instance Lift Phases
deriving instance Lift RuleMatch
deriving instance Lift Inline
deriving instance Lift Fixity
deriving instance Lift FixityDirection
deriving instance Lift Foreign
deriving instance Lift Safety
deriving instance Lift Callconv
deriving instance Lift Overlap
deriving instance Lift FunDep
deriving instance Lift DerivClause
deriving instance Lift DerivStrategy
deriving instance Lift Con
deriving instance Lift Bang
deriving instance Lift SourceUnpackedness
deriving instance Lift SourceStrictness
deriving instance Lift Lit
instance Lift Bytes where
  liftTyped = error "TODO define Lift for Bytes"
deriving instance Lift Type
deriving instance Lift TyVarBndr
deriving instance Lift TyLit

