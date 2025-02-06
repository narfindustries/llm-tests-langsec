Root : struct {
    transaction : BitcoinTransaction;
}

BitcoinTransaction : struct {
    version : uint32;
    input_count : varint;
    inputs : BitcoinInput[input_count];
    output_count : varint;
    outputs : BitcoinOutput[output_count];
    locktime : uint32;
}

BitcoinInput : struct {
    previous_transaction_hash : bytes[32];
    output_index : uint32;
    scriptSig_length : varint;
    scriptSig : bytes[scriptSig_length];
    sequence : uint32;
}

BitcoinOutput : struct {
    value : uint64;
    scriptPubKey_length : varint;
    scriptPubKey : bytes[scriptPubKey_length];
}

varint : union {
    uint8_value : uint8;
    uint16_value : struct { marker : uint8 {0xFD}; value : uint16; };
    uint32_value : struct { marker : uint8 {0xFE}; value : uint32; };
    uint64_value : struct { marker : uint8 {0xFF}; value : uint64; };
}