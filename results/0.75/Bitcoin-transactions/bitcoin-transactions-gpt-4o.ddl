namespace BitcoinTransactions

type BitcoinTransactionHeader = struct {
    version: uint32;
    inputCount: varint;
    inputs: Input[inputCount];
    outputCount: varint;
    outputs: Output[outputCount];
    lockTime: uint32;
}

type Input = struct {
    previousTransactionHash: bytes(32);
    previousTransactionIndex: uint32;
    scriptLength: varint;
    script: bytes(scriptLength);
    sequence: uint32;
}

type Output = struct {
    value: uint64;
    scriptLength: varint;
    script: bytes(scriptLength);
}

root = BitcoinTransactionHeader
