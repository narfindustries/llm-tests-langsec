module BitcoinTransactions where

import Daedalus.Type.Pos

data Transaction = Transaction
  { txid ::  ByteString
  , version :: Word32
  , inputs :: [TxIn]
  , outputs :: [TxOut]
  , locktime :: Word32
  } deriving (Show, Generic, ToJSON, FromJSON, Eq)

data TxIn = TxIn
  { txInPrevTx :: ByteString
  , txInPrevIndex :: Word32
  , txInScriptSig :: ByteString
  , txInSequence :: Word32
  } deriving (Show, Generic, ToJSON, FromJSON, Eq)

data TxOut = TxOut
  { txOutValue :: Word64
  , txOutScriptPubKey :: ByteString
  } deriving (Show, Generic, ToJSON, FromJSON, Eq)


instance Binary Transaction
instance Binary TxIn
instance Binary TxOut

{-# ANN module "HLint: ignore" #-}

main :: IO ()
main = pure ()

