-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module: Math.NumberTheory.Mobius
-- License: BSD-3-Clause
-- Maintainer: Preetham Gujjula <libraries@mail.preetham.io>
-- Stability: experimental
--
-- Tabulate the mobius and Mertens functions.
module Math.NumberTheory.Mobius
  ( -- * Mobius function
    mobius',
    mobiusChimera,

    -- * Mertens function
    mertens',
    mertensChimera,
  )
where

import Control.Placeholder (todo)
import Data.Chimera (UChimera)
import Data.Chimera qualified as Chimera

-- | Compute the mobius function.
--  mobius 0 is arbitrarily defined as 0.
--  Uses sharing.
mobius' :: Word -> Int
mobius' = Chimera.index mobiusChimera

-- | Chimera of the mobius function
mobiusChimera :: UChimera Int
mobiusChimera = todo

-- | Compute the Mertens function. Uses sharing
mertens' :: Word -> Int
mertens' = Chimera.index mertensChimera

-- | Chimera of the Mertens function
mertensChimera :: UChimera Int
mertensChimera = todo
