{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Homework1 where

import Plutus.V2.Ledger.Api
  ( BuiltinData,
    POSIXTime,
    PubKeyHash,
    ScriptContext,
    Validator,
    mkValidatorScript,
  )
import PlutusTx (compile, unstableMakeIsData)
import PlutusTx.Prelude (Bool (..))
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
mkVestingValidator _dat () _ctx = False -- FIX ME!

{-# INLINEABLE mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [||mkWrappedVestingValidator||])
