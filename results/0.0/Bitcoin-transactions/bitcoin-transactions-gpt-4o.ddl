@format little-endian

type BitcoinTransaction = struct {
    version: u32;
    input_count: varint;
    inputs: BitcoinInput[input_count];
    output_count: varint;
    outputs: BitcoinOutput[output_count];
    lock_time: u32;
}

type BitcoinInput = struct {
    previous_output: Outpoint;
    script_length: varint;
    script_sig: u8[script_length];
    sequence: u32;
}

type Outpoint = struct {
    hash: u8[32];
    index: u32;
}

type BitcoinOutput = struct {
    value: u64;
    pk_script_length: varint;
    pk_script: u8[pk_script_length];
}

type varint = switch (peek: u8) {
    0xFD => u16;
    0xFE => u32;
    0xFF => u64;
    => u8;
}