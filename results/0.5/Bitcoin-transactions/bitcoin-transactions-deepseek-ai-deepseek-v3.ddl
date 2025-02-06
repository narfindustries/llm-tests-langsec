let transaction = {
    version: UInt32,
    input_count: VarInt,
    inputs: [input; input_count],
    output_count: VarInt,
    outputs: [output; output_count],
    lock_time: UInt32,
    witness: optional([witness; input_count])
};

let input = {
    previous_tx_hash: Bytes[32],
    previous_tx_output_index: UInt32,
    script_length: VarInt,
    script_sig: Bytes[script_length],
    sequence: UInt32
};

let output = {
    value: Int64,
    script_length: VarInt,
    script_pub_key: Bytes[script_length]
};

let witness = {
    witness_count: VarInt,
    witness_elements: [Bytes[]; witness_count]
};