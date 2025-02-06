define domain bitcoin

define type uint8 = byte
define type uint16 = bytes[2]
define type uint32 = bytes[4]
define type uint64 = bytes[8]

define type BitcoinTransaction = 
  sequence {
    version: uint32 littleEndian,
    txInCount: VarInt,
    txIn: TxIn[txInCount.value],
    txOutCount: VarInt,
    txOut: TxOut[txOutCount.value],
    lockTime: uint32 littleEndian
  }

define type TxIn = 
  sequence {
    txid: bytes[32],
    vout: uint32 littleEndian,
    scriptSig: Script,
    sequence: uint32 littleEndian
  }

define type TxOut = 
  sequence {
    value: uint64 littleEndian,
    scriptPubKey: Script
  }

define type Script = 
  sequence {
    opcodes: Opcode[0..],
    data: bytes[0..]
  }

define type Opcode = 
  sequence {
    value: uint8
  }

define type VarInt = 
  sequence {
    value: uint64,
    byteSequence: bytes[1..9]
  }

define function decodeVarInt(byteSequence: bytes[1..9]): uint64 = 
  if (byteSequence[0] < 0xfd) {
    return byteSequence[0]
  } else if (byteSequence[0] == 0xfd) {
    return (byteSequence[2] << 8) | byteSequence[1]
  } else if (byteSequence[0] == 0xfe) {
    return (byteSequence[4] << 24) | (byteSequence[3] << 16) | (byteSequence[2] << 8) | byteSequence[1]
  } else {
    return (byteSequence[8] << 56) | (byteSequence[7] << 48) | (byteSequence[6] << 40) | (byteSequence[5] << 32) | (byteSequence[4] << 24) | (byteSequence[3] << 16) | (byteSequence[2] << 8) | byteSequence[1]
  }

define constraint for BitcoinTransaction {
  txInCount.value == length(txIn)
}

define constraint for BitcoinTransaction {
  txOutCount.value == length(txOut)
}