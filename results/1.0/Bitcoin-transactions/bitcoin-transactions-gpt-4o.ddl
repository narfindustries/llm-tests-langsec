module BitcoinTransactions

type UInt32 = bytes[4]
type VarInt = if (int) 
                | (value & 0x80) == 0  => (value: UInt8)
                | (value & 0x80) > 0  => (value: UInt16 & 0x7FFF)

type Hash = bytes[32]

struct TransactionInput {
    previousOutput {
        hash: Hash
        index: UInt32
    }
    scriptLength: VarInt
    scriptSig: bytes[scriptLength.value]
    sequence: UInt32
}

struct TransactionOutput {
    value: int<64>
    pkScriptLength: VarInt
    pkScript: bytes[pkScriptLength.value]
}

struct Transaction {
    version: UInt32
    inputCount: VarInt
    inputs: TransactionInput[inputCount.value]
    outputCount: VarInt
    outputs: TransactionOutput[outputCount.value]
    lockTime: UInt32
}

struct BitcoinTransactionFile {
    magicNumber: UInt32
    blocksize: UInt32
    transaction: Transaction
}