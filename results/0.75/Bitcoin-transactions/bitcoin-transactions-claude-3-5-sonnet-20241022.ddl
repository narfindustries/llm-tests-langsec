def Bitcoin = {
  Version : u32 le,
  TxInCount : VarInt,
  TxIn : [TxInCount] {
    PrevTxHash : [32] u8,
    PrevOutIndex : u32 le,
    ScriptLength : VarInt,
    ScriptSig : [ScriptLength] u8,
    Sequence : u32 le
  },
  TxOutCount : VarInt,
  TxOut : [TxOutCount] {
    Value : u64 le,
    ScriptLength : VarInt,
    ScriptPubKey : [ScriptLength] u8
  },
  LockTime : u32 le
}

def VarInt = {
  prefix : u8,
  value : choose prefix {
    0xFD => u16 le,
    0xFE => u32 le,
    0xFF => u64 le,
    _    => prefix
  }
}