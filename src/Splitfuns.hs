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
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- The lifting does not work for locally defined unexported values, but
-- it seems to work for values defined in modules not accessible by the module
-- where the @collect@ function is used. So perhaps this is already good enough.
--
-- TODO It might be possible to mitigate this by using those variables in a
-- class instance definition.

module Splitfuns
  ( define
  , collect
  ) where

import           Data.Data                      ( Data )
import           Data.Foldable                  ( Foldable(fold)
                                                , for_
                                                )
import qualified Data.Map                      as M
import           Data.Map                       ( Map )
import           Data.Maybe                     ( fromMaybe )
import           Language.Haskell.TH            ( thisModule )
import           Language.Haskell.TH.Instances  ( )
import           Language.Haskell.TH.Syntax
import           Optics                         ( (%~)
                                                , GPlate(gplate)
                                                )

data Init = Init

newtype DefMap = DefMap (Map String (Name, [Clause])) deriving (Data, Lift)

instance Semigroup DefMap where
  (<>) = mappend
instance Monoid DefMap where
  mempty = DefMap mempty
  mappend (DefMap x) (DefMap y) = DefMap
    $ M.unionWith (\(n1, cl1) (n2, cl2) -> (n2, rename n1 n2 cl1 ++ cl2)) x y

rename :: GPlate Name t => Name -> Name -> (t -> t)
rename oldname newname = gplate %~ rename'
 where
  rename' name | name == oldname = newname
               | otherwise       = name

initialize :: Q [Dec]
initialize = do
  md <- thisModule
  addModFinalizer do
    defMap <- lift . fromMaybe mempty =<< getQ @DefMap
    addTopDecls [PragmaD (AnnP ModuleAnnotation (SigE defMap (ConT ''DefMap)))]
  reifyModule md >>= \(ModuleInfo mds) -> for_
    mds
    \md' -> do
      anns <- reifyAnnotations @DefMap (AnnLookupModule md')
      prev <- fromMaybe mempty <$> getQ
      putQ (prev <> fold anns)
  pure []

define :: Q [Dec] -> Q [Dec]
define q = do
  initD <- maybe initialize (const (pure [])) =<< getQ @Init
  putQ Init

  [FunD name clauses] <- q
  let str = nameBase name
  DefMap defMap <- fromMaybe (DefMap M.empty) <$> getQ

  let defMap' = M.alter
        (Just . (name, ) . (++ clauses) . maybe
          []
          \(name', clauses') -> rename name' name clauses'
        )
        str
        defMap

  putQ (DefMap defMap')

  pure initD

collect :: String -> Q [Dec]
collect str = do
  initD <- maybe initialize (const (pure [])) =<< getQ @Init
  putQ Init

  Just (DefMap defMap) <- getQ
  -- name                   <- newName (str ++ "'")
  let name = mkName str
  Just (name', clauses') <- pure $ M.lookup str defMap
  pure (FunD name (rename name' name clauses') : initD)
