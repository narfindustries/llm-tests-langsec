protocol BitcoinTransaction {
    version: u32be;
    input_count: varint;
    inputs: [TransactionInput](input_count);
    output_count: varint;
    outputs: [TransactionOutput](output_count);
    locktime: u32be;
}

protocol TransactionInput {
    previous_tx_hash: bytes(32);
    previous_output_index: u32be;
    script_sig_length: varint;
    script_sig: bytes(script_sig_length);
    sequence_number: u32be;
}

protocol TransactionOutput {
    amount: u64be;
    script_pubkey_length: varint;
    script_pubkey: bytes(script_pubkey_length);
}