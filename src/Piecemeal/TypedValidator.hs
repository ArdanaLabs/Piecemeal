{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Piecemeal.TypedValidator (validator) where

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

data Empty

instance ValidatorTypes Empty where
  type DatumType Empty = ()
  type RedeemerType Empty = ()

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ _ = True

typedValidator :: TypedValidator Empty
typedValidator =
  mkTypedValidator @Empty
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
