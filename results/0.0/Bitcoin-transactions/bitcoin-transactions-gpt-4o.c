#include <hammer/hammer.h>

HParser *create_bitcoin_transaction_parser() {
    // Define basic types
    HParser *uint32 = h_uint32();
    HParser *uint64 = h_uint64();
    HParser *varint = h_varint();

    // Define a parser for a Bitcoin transaction input
    HParser *tx_input = h_sequence(
        h_bytes(32),  // Previous transaction hash
        uint32,       // Output index
        h_length_value(varint, h_data()),  // Script length and script
        uint32,       // Sequence
        NULL
    );

    // Define a parser for a Bitcoin transaction output
    HParser *tx_output = h_sequence(
        uint64,       // Value
        h_length_value(varint, h_data()),  // Script length and script
        NULL
    );

    // Define a parser for a Bitcoin transaction
    HParser *bitcoin_transaction = h_sequence(
        uint32,       // Version
        h_length_value(varint, h_many(tx_input)),  // Input count and inputs
        h_length_value(varint, h_many(tx_output)), // Output count and outputs
        uint32,       // Lock time
        NULL
    );

    return bitcoin_transaction;
}

int main() {
    HParser *parser = create_bitcoin_transaction_parser();
    // Use the parser with your data here
    h_parser_free(parser);
    return 0;
}