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
{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Recursive (recursive) where

import Splitfuns (define, sfModule)

define [d|fac n = n * fac (n - 1)|]

recursive = $(sfModule)