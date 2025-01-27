-- Bitcoin Transaction Daedalus Specification
-- Improved and fixed version based on the error message

import Daedalus

-- Define the Bitcoin transaction structure
def BitcoinTransaction =
  block
    version: Version
    inputCount: VarInt
    inputs: Input[inputCount]
    outputCount: VarInt
    outputs: Output[outputCount]
    lockTime: UInt32LE

def Version =
  UInt32LE

def VarInt =
  block
    prefix: UInt8
    case prefix of
      0xFF -> value: UInt64LE
      0xFE -> value: UInt32LE
      0xFD -> value: UInt16LE
      _    -> value: UInt8

def Input =
  block
    previousOutput: OutPoint
    scriptLength: VarInt
    scriptSig: Bytes[scriptLength]
    sequence: UInt32LE

def OutPoint =
  block
    hash: Bytes[32]
    index: UInt32LE

def Output =
  block
    value: UInt64LE
    scriptLength: VarInt
    scriptPubKey: Bytes[scriptLength]

-- Entry point for parsing a Bitcoin transaction
def Main =
  BitcoinTransaction