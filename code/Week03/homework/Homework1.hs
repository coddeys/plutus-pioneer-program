{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Homework1 where

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
    to,
  )
import Plutus.V2.Ledger.Contexts (txSignedBy)
import PlutusTx (compile, unstableMakeIsData)
import PlutusTx.Prelude (Bool, traceIfFalse, ($), (&&), (+), (||))
import Utilities (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
  { beneficiary1 :: PubKeyHash,
    beneficiary2 :: PubKeyHash,
    deadline :: POSIXTime
  }

unstableMakeIsData ''VestingDatum

{-# INLINEABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator dat () ctx =
  traceIfFalse "Benificiary1 did not sign or to late" checkBenificiary1
    || traceIfFalse "Benificiary2 did not sign or is to early" checkBenificiary2
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    txValidRange :: POSIXTimeRange
    txValidRange = txInfoValidRange txInfo

    checkBenificiary1 :: Bool
    checkBenificiary1 =
      txSignedBy txInfo (beneficiary1 dat)
        && contains (to (deadline dat)) txValidRange

    checkBenificiary2 :: Bool
    checkBenificiary2 =
      txSignedBy txInfo (beneficiary2 dat)
        && contains (from (1 + deadline dat)) txValidRange

{-# INLINEABLE mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [||mkWrappedVestingValidator||])
