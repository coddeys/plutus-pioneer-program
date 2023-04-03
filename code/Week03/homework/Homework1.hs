{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V1.Ledger.Interval        (contains, member)
import           Plutus.V2.Ledger.Contexts        (txSignedBy)
import           Plutus.V2.Ledger.Api             (BuiltinData, POSIXTime, PubKeyHash,
                                                   TxInfo (txInfoValidRange),
                                                   scriptContextTxInfo, to, from,
                                                   ScriptContext, Validator,
                                                   mkValidatorScript)
import           PlutusTx                         (compile, unstableMakeIsData)
import           PlutusTx.Prelude                 (Bool (..), (&&), (||) )
import           Utilities                        (wrapValidator)
import Data.Aeson (Value(Bool))
import PlutusTx.Bool (not)
---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator _dat () _ctx =
  validate1 (beneficiary1 _dat) (deadline _dat) txInfo
  || validate2 (beneficiary2 _dat) (deadline _dat) txInfo

  where 
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo _ctx

    

validate1 :: PubKeyHash -> POSIXTime -> TxInfo ->  Bool
validate1 b dl tx =
  txSignedBy tx b && deadlineReached

  where
    deadlineReached :: Bool
    deadlineReached = contains (to dl) (txInfoValidRange tx)


validate2 :: PubKeyHash -> POSIXTime -> TxInfo ->  Bool
validate2 b dl tx =
  txSignedBy tx b && afterDeadline

  where
    afterDeadline :: Bool
    afterDeadline =
      contains (from dl) (txInfoValidRange tx)
      && (not(member dl (txInfoValidRange tx)))


{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
