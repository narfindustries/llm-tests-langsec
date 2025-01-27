#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_bitcoin_transaction_parser(void) {
    // Basic components
    HParser* hex_digit = h_in_range(h_ch('0'), h_ch('9'));
    HParser* hex_alpha = h_choice(h_in_range(h_ch('a'), h_ch('f')), 
                                h_in_range(h_ch('A'), h_ch('F')), NULL);
    HParser* hex = h_choice(hex_digit, hex_alpha, NULL);
    HParser* hex_byte = h_repeat_n(hex, 2);
    
    // Transaction components
    HParser* version = h_repeat_n(hex_byte, 4);
    HParser* varint = h_many1(hex_byte);
    HParser* txid = h_repeat_n(hex_byte, 32);
    HParser* vout = h_repeat_n(hex_byte, 4);
    HParser* script_length = varint;
    HParser* script = h_many1(hex_byte);
    HParser* sequence = h_repeat_n(hex_byte, 4);
    
    // Input structure
    HParser* input = h_sequence(txid, vout, script_length, script, sequence, NULL);
    HParser* input_count = varint;
    HParser* inputs = h_sequence(input_count, h_many1(input), NULL);
    
    // Output components
    HParser* value = h_repeat_n(hex_byte, 8);
    HParser* output = h_sequence(value, script_length, script, NULL);
    HParser* output_count = varint;
    HParser* outputs = h_sequence(output_count, h_many1(output), NULL);
    
    // Locktime
    HParser* locktime = h_repeat_n(hex_byte, 4);
    
    // Complete transaction
    return h_sequence(version, inputs, outputs, locktime, NULL);
}

int main(int argc, char* argv[]) {
    HParser* bitcoin_transaction = init_bitcoin_transaction_parser();
    
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <hex_transaction>\n", argv[0]);
        return 1;
    }
    
    const uint8_t* input = (const uint8_t*)argv[1];
    size_t input_len = strlen(argv[1]);
    
    HParseResult* result = h_parse(bitcoin_transaction, input, input_len);
    
    if (result) {
        printf("Transaction parsed successfully\n");
        h_parse_result_free(result);
        return 0;
    } else {
        fprintf(stderr, "Failed to parse transaction\n");
        return 1;
    }
}