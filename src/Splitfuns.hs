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
{-# LANGUAGE DeriveLift, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- The serialization does not work for locally defined unexported values, but
-- it seems to work for values defined in modules not accessible by the module
-- where the @collect@ function is used. So perhaps this is already good enough.

module Splitfuns (define, collect, sfModule, sfImport, debug) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift ()
import qualified Data.Map as M
import Data.Map (Map)
import Data.Map.Internal (Map (..))
import Data.Maybe
import Optics
-- import Data.Hashable

newtype DefMap = DefMap (Map String (Name, [Clause])) deriving (Lift, Show)

deriving instance (Lift k, Lift v) => Lift (Map k v)

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

-- -- Haskell base64 encoding which is compatible with Haskell variable names
-- hs64 :: Integral a => a -> String
-- hs64 n = "hs64_" ++ go n where
--   go 0 = ""
--   go n' = let (q, r) = quotRem n' 64 in table !! fromIntegral r : go q
--   table = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "'_"

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

  pure [ValD WildP (NormalB (TupE [])) [FunD name clauses]]

collect :: String -> Q Exp
collect str = do
  Just (DefMap defMap) <- getQ @DefMap
  name <- newName (str ++ "'")
  Just (name', clauses') <- pure $ M.lookup str defMap
  pure (LetE [FunD name (clauses' & rename name' name)] (VarE name))

sfModule :: Q Exp
sfModule = lift =<< fromMaybe mempty <$> getQ @DefMap

sfImport :: DefMap -> Q [Dec]
sfImport x' = do
  x <- fromMaybe mempty <$> getQ @DefMap
  putQ (x <> x')
  pure []

debug :: Q [Dec]
debug = do
  x <- fromMaybe mempty <$> getQ @DefMap
  runIO (print x)
  pure []
