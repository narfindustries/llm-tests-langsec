RootStruct BitcoinTransaction {
    version: u32;
    input_count: VarInt;
    inputs: Input[input_count];
    output_count: VarInt;
    outputs: Output[output_count];
    lock_time: u32;
}

Struct Input {
    prev_tx_hash: u8[32];
    output_index: u32;
    script_length: VarInt;
    signature_script: u8[script_length];
    sequence: u32;
}

Struct Output {
    value: u64;
    script_length: VarInt;
    public_key_script: u8[script_length];
}

Struct VarInt {
    value: u8;
    switch (value) {
        case 0xFD: extended_value: u16;
        case 0xFE: extended_value: u32;
        case 0xFF: extended_value: u64;
    }
}