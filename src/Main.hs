{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Cardano.Api
import Cardano.Api.Shelley
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import qualified Piecemeal.Empty as V
import qualified Plutus.V1.Ledger.Api as Plutus
import Prelude

main :: IO ()
main = generatePlutusScriptAndReport

generatePlutusScriptAndReport :: IO ()
generatePlutusScriptAndReport = do
  case Plutus.defaultCostModelParams of
    Just m ->
      let Alonzo.Data pData = toAlonzoData (ScriptDataNumber 42)
          (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m plutusScriptShortBs [pData]
       in do
            putStr ("Log output: " :: String) >> print logout
            case e of
              Left evalErr -> putStr ("Eval Error: " :: String) >> print evalErr
              Right exbudget -> putStr ("Ex Budget: " :: String) >> print exbudget
    Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope "contract.plutus" Nothing plutusScript
  putStrLn "Wrote: contract.plutus"
  putStrLn $ "Code size (bytes): " <> show (SBS.length plutusScriptShortBs)
  print V.validatorPir
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
  where
    plutusScript :: PlutusScript PlutusScriptV1
    plutusScript = PlutusScriptSerialised plutusScriptShortBs

    plutusScriptShortBs :: SBS.ShortByteString
    plutusScriptShortBs = SBS.toShort $ LB.toStrict scriptAsCbor

    scriptAsCbor :: LB.ByteString
    scriptAsCbor = serialise V.validator
