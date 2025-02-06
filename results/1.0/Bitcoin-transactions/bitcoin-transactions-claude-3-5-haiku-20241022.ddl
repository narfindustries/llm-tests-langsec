specification BitcoinTransaction {
    type Input = struct {
        prev_transaction_hash: [32]byte,
        prev_output_index: uint32,
        script_sig_length: varlen,
        script_sig: [script_sig_length]byte,
        sequence_number: uint32
    }

    type Output = struct {
        value: uint64,
        script_pubkey_length: varlen,
        script_pubkey: [script_pubkey_length]byte
    }

    type Transaction = struct {
        version: uint32,
        input_count: varlen,
        inputs: [input_count]Input,
        output_count: varlen,
        outputs: [output_count]Output,
        locktime: uint32
    }
}