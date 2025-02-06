data Transaction = struct {
    version: s32;
    input_count: VarInt;
    inputs: array[input_count] of TxIn;
    output_count: VarInt;
    outputs: array[output_count] of TxOut;
    lock_time: u32;
};

data TxIn = struct {
    previous_output: OutPoint;
    script_length: VarInt;
    script_sig: bytes[script_length];
    sequence: u32;
};

data TxOut = struct {
    value: u64;
    script_length: VarInt;
    script_pub_key: bytes[script_length];
};

data OutPoint = struct {
    hash: bytes[32];
    index: u32;
};

data VarInt = union {
    u8:  u8;
    u16: u16;
    u32: u32;
    u64: u64;
} switch (peek u8) {
    case 0xFF: u64;
    case 0xFE: u32;
    case 0xFD: u16;
    default:   u8;
};