type BitcoinTransaction = struct {
    version uint32;
    inputs_count varint;
    inputs [Input] : length(inputs_count);
    outputs_count varint;
    outputs [Output] : length(outputs_count);
    lock_time uint32;
};

type Input = struct {
    prev_transaction_hash [uint8] : size(32);
    prev_transaction_index uint32;
    script_length varint;
    signature_script [uint8] : length(script_length);
    sequence_no uint32;
};

type Output = struct {
    value uint64;
    pk_script_length varint;
    pk_script [uint8] : length(pk_script_length);
};

type VarInt = union {
    case byte: uint8;
    case word: uint16;
    case dword: uint32;
    case qword: uint64;
} switch (peek uint8) {
    case 0xFF => qword;
    case 0xFE => dword;
    case 0xFD => word;
    default => byte;
};

type varint = struct {
    value VarInt;
};

func main() {
    stream BitcoinTransaction : parse;
}