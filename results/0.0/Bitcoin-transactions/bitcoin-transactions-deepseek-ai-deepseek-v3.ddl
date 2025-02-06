BitcoinTransaction ::= {
    version: UInt32,
    input_count: VarInt,
    inputs: Input[input_count],
    output_count: VarInt,
    outputs: Output[output_count],
    locktime: UInt32,
    witness: Optional(Witness)
};

Input ::= {
    previous_tx_hash: Bytes(32),
    previous_output_index: UInt32,
    script_sig_length: VarInt,
    script_sig: Bytes(script_sig_length),
    sequence: UInt32
};

Output ::= {
    value: UInt64,
    script_pubkey_length: VarInt,
    script_pubkey: Bytes(script_pubkey_length)
};

Witness ::= {
    witness_count: VarInt,
    witness_data: Bytes[witness_count]
};

VarInt ::= Switch {
    case < 0xFD: UInt8,
    case == 0xFD: UInt16,
    case == 0xFE: UInt32,
    case == 0xFF: UInt64
};

Optional(T) ::= Switch {
    case is_present: T,
    case not_present: Empty
};

Empty ::= {};