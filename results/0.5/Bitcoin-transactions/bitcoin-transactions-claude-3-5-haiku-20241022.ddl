protocol BitcoinTransaction {
    version: u32;
    input_count: varint;
    inputs: list<Input> {
        previous_tx_hash: byte[32];
        previous_output_index: u32;
        script_sig_length: varint;
        script_sig: byte[script_sig_length];
        sequence_number: u32;
    }
    output_count: varint;
    outputs: list<Output> {
        amount: u64;
        script_pubkey_length: varint;
        script_pubkey: byte[script_pubkey_length];
    }
    locktime: u32;
    witness_marker?: u8;
    witness_flag?: u8;
    witness_data?: list<WitnessData> {
        witness_stack_items_count: varint;
        witness_items: list<WitnessItem> {
            item_length: varint;
            item_data: byte[item_length];
        }
    }
}