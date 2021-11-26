{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | An "empty" script validator that always succeeds.
--
-- Represents the smallest validator script possible in Plutus.
module Piecemeal.Empty (validator, validatorPir) where

import Ledger
  ( Address,
    Validator,
    ValidatorHash,
    mkValidatorScript,
    scriptAddress,
  )
import qualified Ledger.Scripts as Scripts
import qualified PlutusTx
import qualified PlutusTx.Code as PC
import PlutusTx.Prelude
import qualified Prettyprinter as PP

validatorPir :: PP.Doc ann
validatorPir =
  PP.pretty $
    PC.getPir
      $$(PlutusTx.compile [||mkValidator||])

{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [||mkValidator||])

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
