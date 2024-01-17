{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Homework2 where

import Plutus.V1.Ledger.Interval (contains)
import Plutus.V2.Ledger.Api
  ( BuiltinData,
    POSIXTime,
    POSIXTimeRange,
    PubKeyHash,
    ScriptContext (scriptContextTxInfo),
    TxInfo (txInfoValidRange),
    Validator,
    from,
    mkValidatorScript,
  )
import Plutus.V2.Ledger.Contexts (txSignedBy)
import PlutusTx (applyCode, compile, liftCode)
import PlutusTx.Prelude (Bool, traceIfFalse, (&&), (.))
import Utilities (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINEABLE mkParameterizedVestingValidator #-}
-- This should validate if the transaction has a signature from the parameterized beneficiary and the deadline has passed.
mkParameterizedVestingValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator beneficiary deadline () ctx =
  traceIfFalse "Benificiary did not sign or is to early" checkBenificiary
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    txValidRange :: POSIXTimeRange
    txValidRange = txInfoValidRange txInfo

    checkBenificiary :: Bool
    checkBenificiary =
      txSignedBy txInfo beneficiary
        && contains (from deadline) txValidRange

{-# INLINEABLE mkWrappedParameterizedVestingValidator #-}
mkWrappedParameterizedVestingValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator = wrapValidator . mkParameterizedVestingValidator

validator :: PubKeyHash -> Validator
validator beneficiary = mkValidatorScript ($$(compile [||mkWrappedParameterizedVestingValidator||]) `applyCode` liftCode beneficiary)
