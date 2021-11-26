{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Piecemeal.Hello (validator) where

import Ledger
  ( Address,
    Validator,
    ValidatorHash,
    scriptAddress,
  )
import Ledger.Contexts (ScriptContext)
import qualified Ledger.Scripts as Scripts
import Ledger.Typed.Scripts
  ( TypedValidator,
    ValidatorTypes (..),
    mkTypedValidator,
    validatorScript,
    wrapValidator,
  )
import qualified PlutusTx
import PlutusTx.Prelude

data Hello

data MyDatum = MyDatum
  { _foo :: Integer,
    _bar :: [Integer]
  }

PlutusTx.unstableMakeIsData ''MyDatum

newtype MyRedeemer = MyRedeemer {unMyRedeemer :: Integer}

PlutusTx.unstableMakeIsData ''MyRedeemer

instance ValidatorTypes Hello where
  type DatumType Hello = MyDatum
  type RedeemerType Hello = MyRedeemer

{-# INLINEABLE mkValidator #-}
mkValidator :: MyDatum -> MyRedeemer -> ScriptContext -> Bool
mkValidator _ _ _ = True

typedValidator :: TypedValidator Hello
typedValidator =
  mkTypedValidator @Hello
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator

validator :: Validator
validator = validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
