#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_bitcoin_transaction_parser(void) {
    // Basic primitives
    HParser *hex_digit = h_choice(h_ch_range('0', '9'), 
                                h_ch_range('a', 'f'),
                                h_ch_range('A', 'F'), 
                                NULL);
    HParser *hex_byte = h_sequence(hex_digit, hex_digit, NULL);
    HParser *hex_string = h_many1(hex_byte);
    
    // Transaction components
    HParser *version = h_repeat_n(hex_byte, 8);  // 4 bytes
    
    // Input/Output count as var_int
    HParser *var_int = h_choice(
        h_repeat_n(hex_byte, 2),  // 1-byte
        h_sequence(h_token((uint8_t*)"fd", 2), h_repeat_n(hex_byte, 4), NULL),  // 2-bytes
        h_sequence(h_token((uint8_t*)"fe", 2), h_repeat_n(hex_byte, 8), NULL),  // 4-bytes
        h_sequence(h_token((uint8_t*)"ff", 2), h_repeat_n(hex_byte, 16), NULL), // 8-bytes
        NULL
    );

    // Transaction Input
    HParser *txid = h_repeat_n(hex_byte, 64);  // 32 bytes
    HParser *vout = h_repeat_n(hex_byte, 8);   // 4 bytes
    HParser *script_sig = h_sequence(var_int, hex_string, NULL);
    HParser *sequence = h_repeat_n(hex_byte, 8);  // 4 bytes
    HParser *tx_input = h_sequence(txid, vout, script_sig, sequence, NULL);
    HParser *tx_inputs = h_sequence(var_int, h_many1(tx_input), NULL);

    // Transaction Output
    HParser *value = h_repeat_n(hex_byte, 16);  // 8 bytes
    HParser *script_pubkey = h_sequence(var_int, hex_string, NULL);
    HParser *tx_output = h_sequence(value, script_pubkey, NULL);
    HParser *tx_outputs = h_sequence(var_int, h_many1(tx_output), NULL);

    // Locktime
    HParser *locktime = h_repeat_n(hex_byte, 8);  // 4 bytes

    // Complete transaction
    return h_sequence(version, tx_inputs, tx_outputs, locktime, NULL);
}

int main(int argc, char *argv[]) {
    HParser *parser = init_bitcoin_transaction_parser();
    
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }

    HParseResult *result = h_parse(parser, (const uint8_t*)"0100000001a6b97044d03da79c005b20ea9c0e1a6d9dc12d9f7b91a5911c9030a439eed8f5000000004948304502206e21798a42fae0e854281abd38bacd1aeed3ee3738d9e1446618c4571d1090db022100e2ac980643b0b82c0e88ffdfec6b64e3e6ba35e7ba5fdd7d5d6cc8d25c6b241501ffffffff0100f2052a010000001976a914404371705fa9bd789a2fcd52d2c580b65d35549d88ac00000000", 204);

    if (!result) {
        fprintf(stderr, "Parse failed\n");
        return 1;
    }

    printf("Parse successful\n");
    return 0;
}