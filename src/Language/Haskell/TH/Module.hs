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
{-# LANGUAGE TypeApplications, ScopedTypeVariables, TemplateHaskellQuotes #-}
module Language.Haskell.TH.Module where

import Language.Haskell.TH.Syntax

import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable)
import Data.Serialize (Serialize, encode, decode)
import Data.Maybe (fromMaybe, fromJust)
import Data.ByteString (ByteString, pack, unpack)
import Data.List (elemIndex)

hs64table :: String
hs64table = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "'_"

-- Haskell base64 encoding which is compatible with Haskell variable names
hs64 :: Integer -> String
hs64 n'' = "hs64_" ++ go n where
  go 0 = ""
  go n' = let (q, r) = quotRem n' 64 in hs64table !! fromIntegral r : go q
  n | n'' < 0 = 1 - 2 * n''
    | otherwise = 2 * n''

unhs64 :: String -> Integer
unhs64 ('h':'s':'6':'4':'_':xs0) = post $ go xs0 where
  go "" = 0
  go (x:xs) = fromIntegral (fromJust (elemIndex x hs64table)) + 64 * go xs
  post n
    | even n = n `quot` 2
    | otherwise = (n - 1) `quot` 2
unhs64 _ = undefined

i2bs :: Integer -> ByteString
i2bs = pack . go where
  go 0 = []
  go n = let (q, r) = quotRem n (2 ^ (9 :: Int)) in fromIntegral (r - 1) : go q

bs2i :: ByteString -> Integer
bs2i = go (0 :: Int) . unpack where
  go i (x:xs) = (2 ^ (9 * i)) * (fromIntegral x + 1) + go (i + 1) xs
  go _ [] = 0

-- This can be generalized to take a list of proxies and encoding it as a type
-- level list or tuple, but this will do for now.
module' :: (Monoid a, Serialize a, Typeable a) => Proxy a -> Q Exp
module' (Proxy :: Proxy a) = do
  x <- fromMaybe mempty <$> getQ @a
  pure $ SigE (VarE 'undefined) (VarT (mkName (hs64 (bs2i (encode @a x)))))

-- This can be generalized to take a list of proxies and encoding it as a type
-- level list or tuple, but this will do for now.
moduleType :: (Monoid a, Serialize a, Typeable a) => Proxy a -> Q Type
moduleType (Proxy :: Proxy a) = do
  x <- fromMaybe mempty <$> getQ @a
  pure $ VarT (mkName (hs64 (bs2i (encode @a x))))

import' :: (Monoid a, Serialize a, Typeable a) => Proxy a -> Name -> Q [Dec]
import' (Proxy :: Proxy a) name = do
  x <- fromMaybe mempty <$> getQ @a
  VarI _ (ForallT _ _ (VarT s)) _ <- reify name
  x' <- either fail pure $ decode @a (i2bs (unhs64 (nameBase s)))
  putQ (x <> x')
  pure []
