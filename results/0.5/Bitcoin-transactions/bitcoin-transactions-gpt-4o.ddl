module BitcoinTransactions

import Prelude

data BitcoinTransaction = BitcoinTransaction
  { version       : Int32
  , tx_in_count   : VarInt
  , tx_in         : List TxIn
  , tx_out_count  : VarInt
  , tx_out        : List TxOut
  , lock_time     : UInt32
  }

data TxIn = TxIn
  { previous_output : OutPoint
  , script_length   : VarInt
  , script_sig      : Bytes script_length
  , sequence        : UInt32
  }

data TxOut = TxOut
  { value          : Int64
  , pk_script_len  : VarInt
  , pk_script      : Bytes pk_script_len
  }

data OutPoint = OutPoint
  { hash  : Bytes 32
  , index : UInt32
  }

data VarInt
  = VarInt8  { value : UInt8  }
  | VarInt16 { marker : UInt8, value : UInt16 }
  | VarInt32 { marker : UInt8, value : UInt32 }
  | VarInt64 { marker : UInt8, value : UInt64 }

instance IsVarInt VarInt where
  fromBytes bs = case bs of
    0xfd : rest -> VarInt16 0xfd (fromBytes rest)
    0xfe : rest -> VarInt32 0xfe (fromBytes rest)
    0xff : rest -> VarInt64 0xff (fromBytes rest)
    _          -> VarInt8 (fromBytes bs)

  toBytes varInt = case varInt of
    VarInt8 v    -> toBytes v
    VarInt16 m v -> m : toBytes v
    VarInt32 m v -> m : toBytes v
    VarInt64 m v -> m : toBytes v