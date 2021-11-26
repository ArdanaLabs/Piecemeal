{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-pir-flat #-}

module Piecemeal.Hello (validator, validatorPir) where

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
import qualified PlutusTx.Code as PC
import PlutusTx.Prelude
import qualified Prettyprinter as PP

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

validatorPir :: PP.Doc ann
validatorPir =
  PP.pretty $
    PC.getPir
      $$(PlutusTx.compile [||mkValidator||])

validator :: Validator
validator = validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator