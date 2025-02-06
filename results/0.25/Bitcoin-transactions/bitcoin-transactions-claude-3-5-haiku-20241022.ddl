grammar BitcoinTransaction {
    transaction = version input_count inputs output_count outputs locktime witness_marker? witness_flag? witness_data?;

    version: u32;
    input_count: varint;
    inputs: Input[input_count];
    output_count: varint;
    outputs: Output[output_count];
    locktime: u32;
    witness_marker: u8?;
    witness_flag: u8?;
    witness_data: WitnessData[input_count]?;

    Input = {
        previous_tx_hash: bytes(32);
        previous_output_index: u32;
        script_sig_length: varint;
        script_sig: bytes(script_sig_length);
        sequence_number: u32;
    }

    Output = {
        amount: u64;
        script_pubkey_length: varint;
        script_pubkey: bytes(script_pubkey_length);
    }

    WitnessData = {
        witness_item_count: varint;
        witness_items: bytes(witness_item_count)[];
    }
}