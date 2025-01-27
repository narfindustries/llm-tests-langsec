#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_bitcoin_transaction_parser(void) {
    // Basic primitives
    HParser* hex_digit = h_in_range(h_ch('0'), h_ch('9'));
    HParser* hex_letter = h_in_range(h_ch('a'), h_ch('f'));
    HParser* hex_char = h_choice(hex_digit, hex_letter, NULL);
    HParser* hex_byte = h_repeat_n(hex_char, 2);
    
    // Transaction hash (32 bytes)
    HParser* txid = h_repeat_n(hex_byte, 32);
    
    // Version (4 bytes)
    HParser* version = h_repeat_n(hex_byte, 4);
    
    // Input/Output count (variable length)
    HParser* count = h_repeat_n(hex_byte, 1);
    
    // Script length (variable)
    HParser* script_length = h_repeat_n(hex_byte, 1);
    
    // Script (variable based on length)
    HParser* script = h_many(hex_byte);
    
    // Sequence (4 bytes)
    HParser* sequence = h_repeat_n(hex_byte, 4);
    
    // Value (8 bytes)
    HParser* value = h_repeat_n(hex_byte, 8);
    
    // Input structure
    HParser* input = h_sequence(txid, count, script_length, script, sequence, NULL);
    
    // Output structure
    HParser* output = h_sequence(value, script_length, script, NULL);
    
    // Locktime (4 bytes)
    HParser* locktime = h_repeat_n(hex_byte, 4);
    
    // Complete transaction
    return h_sequence(version,
                     count,  // input count
                     h_many(input),
                     count,  // output count 
                     h_many(output),
                     locktime,
                     NULL);
}

int main(int argc, char* argv[]) {
    HParser* parser = init_bitcoin_transaction_parser();
    
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }
    
    const uint8_t* input = (const uint8_t*)"0100000001a6b97044d03da79c005b20ea9c0e1a6d9dc12d9f7b91a5911c9030a439eed8f5000000004948304502206e21798a42fae0e854281abd38bacd1aeed3ee3738d9e1446618c4571d1090db022100e2ac980643b0b82c0e88ffdfec6b64e3e6ba35e7ba5fdd7d5d6cc8d25c6b241501ffffffff0100f2052a010000001976a914404371705fa9bd789a2fcd52d2c580b65d35549d88ac00000000";
    size_t input_len = strlen((char*)input);
    
    HParseResult* result = h_parse(parser, input, input_len);
    
    if (!result) {
        fprintf(stderr, "Parse failed\n");
        return 1;
    }
    
    h_parse_result_free(result);
    return 0;
}