type BitcoinAddress = Bytes(20..21)
type TransactionHash = Bytes(32)
type ScriptSig = Bytes(0..255)
type ScriptPubKey = Bytes(0..255)

struct Input {
    previousTxHash: TransactionHash,
    previousOutputIndex: U32,
    scriptSig: ScriptSig,
    sequenceNumber: U32
}

struct Output {
    value: U64,
    scriptPubKey: ScriptPubKey
}

struct Transaction {
    version: U32,
    inputs: List(Input),
    outputs: List(Output),
    lockTime: U32
}

fn validateTransaction(tx: Transaction) -> Bool {
    // Basic transaction validation rules
    tx.inputs.length() > 0 &&
    tx.outputs.length() > 0 &&
    tx.inputs.length() <= 100 &&
    tx.outputs.length() <= 100
}

fn calculateTxHash(tx: Transaction) -> TransactionHash {
    // Simplified transaction hash calculation
    hash(serialize(tx))
}