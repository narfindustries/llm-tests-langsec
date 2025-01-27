{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BitcoinTransactions where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger (Address, CurrencySymbol, Datum, DatumHash, TxId, TxOutRef)
import Ledger.Ada (Ada)
import Ledger.Value (Value)
import Plutus.Contract (Contract, EmptySchema, Endpoint, mapError)
import Plutus.Contract.StateMachine (StateMachine, StateMachineError,
                                     runInitial, State, next,
                                     update)
import PlutusTx.Prelude hiding (Semigroup(..), (<>))
import qualified Data.Map as Map


-- | Data for the Bitcoin transaction
data BitcoinTransaction = BitcoinTransaction
  { txId :: TxId
  , amount :: Integer
  , recipient :: Address
  } deriving (Show, Generic, FromJSON, ToJSON)


-- | State of the state machine
data BitcoinState = BitcoinState
  {  tx :: Maybe BitcoinTransaction
  } deriving (Show, Generic, FromJSON, ToJSON, Eq, Ord)


-- | Initial state
initialState :: BitcoinState
initialState = BitcoinState Nothing


-- | Transition function
transition :: BitcoinState -> BitcoinTransaction -> Either StateMachineError BitcoinState
transition state tx = pure $ state { tx = Just tx }


-- | State machine
instance StateMachine BitcoinState BitcoinTransaction where
    initialState = initialState
    transition = transition
    next _ _ = Nothing


-- | Contract
contract :: Contract () EmptySchema Text ()
contract = runInitial $ do
  tx <- awaitPromise $ endpoint @"submitTx"
  update tx


-- | Endpoint for submitting a Bitcoin transaction
endpoints :: Contract () EmptySchema Text ()
endpoints = contract


-- | Type instances
deriving instance Generic BitcoinTransaction
deriving instance ToJSON BitcoinTransaction
deriving instance FromJSON BitcoinTransaction
deriving instance Generic BitcoinState
deriving instance ToJSON BitcoinState
deriving instance FromJSON BitcoinState


-- | Endpoint for submitting transactions
type SubmitTx = Endpoint "submitTx" BitcoinTransaction

-- | Main function
main :: IO ()
main = return ()

