BitcoinTransaction = struct {
    version: uint32le;
    input_count: varint;
    inputs: Input[input_count];
    output_count: varint;
    outputs: Output[output_count];
    lock_time: uint32le;
};

Input = struct {
    previous_output: OutPoint;
    script_length: varint;
    script: bytes[script_length];
    sequence: uint32le;
};

OutPoint = struct {
    hash: bytes[32];
    index: uint32le;
};

Output = struct {
    value: uint64le;
    script_length: varint;
    script: bytes[script_length];
};

varint = switch {
    case value < 0xFD: uint8;
    case value <= 0xFFFF: uint16le;
    case value <= 0xFFFFFFFF: uint32le;
    default: uint64le;
};