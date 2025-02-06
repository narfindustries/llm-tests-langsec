schema Bitcoin {
    endian little;

    type varint = varlen_int;
    type u256 = bytes[32];

    struct Outpoint {
        tx_hash: u256;
        output_index: u32;
    }

    struct ScriptElement {
        length: varint;
        data: bytes[length];
    }

    struct WitnessItem {
        item_length: varint;
        item_data: bytes[item_length];
    }

    struct WitnessData {
        stack_items_count: varint;
        witness_stack: WitnessItem[stack_items_count];
    }

    struct TxInput {
        previous_output: Outpoint;
        script_sig: ScriptElement;
        sequence: u32;
        witness: optional WitnessData;
    }

    struct TxOutput {
        value: u64;
        script_pubkey: ScriptElement;
    }

    struct Transaction {
        version: u32;
        marker: optional {
            flag_byte1: u8 = 0x00;
            flag_byte2: u8 = 0x01;
        };
        input_count: varint;
        inputs: TxInput[input_count];
        output_count: varint;
        outputs: TxOutput[output_count];
        locktime: u32;
    }
}