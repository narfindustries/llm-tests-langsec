def VarInt = {
    let first = byte;
    let value = if first <= 0xfc then first
                else if first == 0xfd then u16
                else if first == 0xfe then u32
                else u64;
    value
}

def TransactionInput = {
    let previous_tx_hash = bytes(32);
    let previous_output_index = u32;
    let script_length = VarInt;
    let script_sig = bytes(script_length);
    let sequence = u32;
    (previous_tx_hash, previous_output_index, script_length, script_sig, sequence)
}

def TransactionOutput = {
    let value = u64;
    let script_length = VarInt;
    let script_pubkey = bytes(script_length);
    (value, script_length, script_pubkey)
}

def Transaction = {
    let version = u32;
    let input_count = VarInt;
    let inputs = array(input_count, TransactionInput);
    let output_count = VarInt;
    let outputs = array(output_count, TransactionOutput);
    let lock_time = u32;
    (version, input_count, inputs, output_count, outputs, lock_time)
}

entry point Transaction