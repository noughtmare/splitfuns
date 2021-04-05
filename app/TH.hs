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
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -Wno-orphans #-}

-- The serialization does not work for locally defined unexported values, but
-- it seems to work for values defined in modules not accessible by the module
-- where the @collect@ function is used. So perhaps this is already good enough.

module TH (define, collect, splitfunsModule, splitfunsImport, i2bs, bs2i) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Optics
-- import Data.Hashable
import Data.Serialize
import Data.Proxy
import Data.ByteString

newtype DefMap = DefMap (Map String (Name, [Clause])) deriving Serialize

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

rename :: GPlate Name t => Name -> Name -> (t -> t)
rename oldname newname = gplate %~ rename'
  where
    rename' name
      | name == oldname = newname
      | otherwise       = name

-- -- Haskell base64 encoding which is compatible with Haskell variable names
-- hs64 :: Integral a => a -> String
-- hs64 n = "hs64_" ++ go n where
--   go 0 = ""
--   go n' = let (q, r) = quotRem n' 64 in table !! fromIntegral r : go q
--   table = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "'_"

i2bs :: Integer -> ByteString
i2bs = pack . go where
  go 0 = []
  go n = let (q, r) = quotRem n (2 ^ (9 :: Int)) in fromIntegral (r - 1) : go q

bs2i :: ByteString -> Integer
bs2i = go (0 :: Int) . unpack where
  go i (x:xs) = (2 ^ (9 * i)) * (fromIntegral x + 1) + go (i + 1) xs
  go _ [] = 0

define :: Q [Dec] -> Q [Dec]
define q = do
  [FunD name clauses] <- q
  let str = nameBase name
  DefMap defMap <- fromMaybe (DefMap M.empty) <$> getQ @DefMap

  let
    defMap' = M.alter
      (Just . (name,) . (++ clauses) . maybe []
        \(name', clauses') -> clauses' & rename name' name)
      str
      defMap

  putQ @DefMap (DefMap defMap')

  return []

collect :: String -> Q Exp
collect str = do
  Just (DefMap defMap) <- getQ @DefMap
  name <- newName (str ++ "'")
  Just (name', clauses') <- pure $ M.lookup str defMap
  pure (LetE [FunD name (clauses' & rename name' name)] (VarE name))

splitfunsModule :: Q Exp
splitfunsModule = do
  Just defMap <- getQ @DefMap
  pure $ SigE
    (ConE 'Proxy)
    (ConT ''Proxy `AppT` LitT (NumTyLit (bs2i (encode @DefMap defMap))))

splitfunsImport :: Name -> Q [Dec]
splitfunsImport x' = do
  VarI _ (AppT _ (LitT (NumTyLit x))) _ <- reify x'
  let DefMap defMap' = either error id $ decode @DefMap (i2bs x)
  runIO $ print defMap'
  DefMap defMap <- fromMaybe (DefMap M.empty) <$> getQ @DefMap
  putQ $ DefMap $ M.unionWith
    (\(n1,cl1) (n2,cl2) -> (n2, (cl1 & rename n1 n2) ++ cl2))
    defMap
    defMap'
  pure []
