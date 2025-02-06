namespace BitcoinTransactions

structure BitcoinTransaction {
    version: uint32;
    input_count: varint;
    inputs: BitcoinInput[input_count];
    output_count: varint;
    outputs: BitcoinOutput[output_count];
    locktime: uint32;
}

structure BitcoinInput {
    prev_transaction_hash: byte[32];
    prev_output_index: uint32;
    script_length: varint;
    signature_script: byte[script_length];
    sequence_number: uint32;
}

structure BitcoinOutput {
    value: uint64;
    script_length: varint;
    pubkey_script: byte[script_length];
}