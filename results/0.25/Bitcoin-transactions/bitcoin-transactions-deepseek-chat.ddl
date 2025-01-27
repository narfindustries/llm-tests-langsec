let BitcoinTransaction = struct {
    version: be32;
    input_count: varint;
    inputs: Input[input_count];
    output_count: varint;
    outputs: Output[output_count];
    lock_time: be32;
};

let Input = struct {
    previous_output: OutPoint;
    script_length: varint;
    script: bytes[script_length];
    sequence: be32;
};

let OutPoint = struct {
    txid: bytes[32];
    index: be32;
};

let Output = struct {
    value: be64;
    script_length: varint;
    script: bytes[script_length];
};

let varint = switch {
    case value < 0xFD: uint8;
    case value <= 0xFFFF: uint16;
    case value <= 0xFFFFFFFF: uint32;
    default: uint64;
};