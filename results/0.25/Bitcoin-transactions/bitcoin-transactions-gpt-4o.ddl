module BitcoinTransactions

type BitcoinTransaction = struct {
    version: u4,
    input_count: varint,
    inputs: array BitcoinInput(input_count),
    output_count: varint,
    outputs: array BitcoinOutput(output_count),
    lock_time: u4
}

type BitcoinInput = struct {
    previous_output: OutPoint,
    script_length: varint,
    script: bytes(script_length),
    sequence: u4
}

type OutPoint = struct {
    hash: bytes(32),
    index: u4
}

type BitcoinOutput = struct {
    value: u8,
    script_length: varint,
    script: bytes(script_length)
}

type varint = switch u1 {
    0xfd => u2,
    0xfe => u4,
    0xff => u8,
    _ => u1
}