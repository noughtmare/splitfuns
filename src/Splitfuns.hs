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
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- The serialization does not work for locally defined unexported values, but
-- it seems to work for values defined in modules not accessible by the module
-- where the @collect@ function is used. So perhaps this is already good enough.

module Splitfuns (define, collect, sfModule, sfImport) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Module
import Language.Haskell.TH.Serialize ()
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Optics
import Data.Hashable
import Data.Serialize
import Data.Proxy
import Data.Foldable

data Init = Init
data Manual = Manual

newtype DefMap = DefMap (Map String (Name, [Clause])) deriving Serialize

instance Semigroup DefMap where (<>) = mappend
instance Monoid DefMap where
  mempty = DefMap mempty
  mappend (DefMap x) (DefMap y) = DefMap
    $ M.unionWith (\(n1,cl1) (n2,cl2) -> (n2, (cl1 & rename n1 n2) ++ cl2)) x y

rename :: GPlate Name t => Name -> Name -> (t -> t)
rename oldname newname = gplate %~ rename'
  where
    rename' name
      | name == oldname = newname
      | otherwise       = name

instance Hashable Module
instance Hashable PkgName
instance Hashable ModName

initialize :: Q ()
initialize = do
  md <- thisModule
  addModFinalizer do
    -- disable automatic module generation when a manual module is defined
    getQ @Manual >>= \case
      Just _ -> pure ()
      Nothing -> do
        mtyp <- moduleType (Proxy @DefMap)
        name <- newName (hs64 (fromIntegral (hash ("splitfuns", md))))
        addTopDecls
          [ SigD name mtyp
          , ValD (VarP name) (NormalB (VarE 'undefined)) []
          ]
  reifyModule md >>= \(ModuleInfo mds) -> for_ mds \md' -> do
    lookupValueName (hs64 (fromIntegral (hash ("splitfuns", md')))) >>= \case
      Nothing -> pure ()
      Just name -> () <$ sfImport name

define :: Q [Dec] -> Q [Dec]
define q = do
  getQ @Init >>= \case
    Nothing -> initialize
    _ -> pure ()
  putQ Init

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

  pure []

collect :: String -> Q Exp
collect str = do
  getQ @Init >>= \case
    Nothing -> initialize
    _ -> pure ()
  putQ Init

  Just (DefMap defMap) <- getQ @DefMap
  name <- newName (str ++ "'")
  Just (name', clauses') <- pure $ M.lookup str defMap
  pure (LetE [FunD name (clauses' & rename name' name)] (VarE name))

sfModule :: Q Exp
sfModule = do
  putQ Manual
  module' (Proxy @DefMap)

sfImport :: Name -> Q [Dec]
sfImport = import' (Proxy @DefMap)
