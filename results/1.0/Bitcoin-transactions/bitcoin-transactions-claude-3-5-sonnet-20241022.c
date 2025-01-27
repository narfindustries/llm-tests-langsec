#include <hammer/hammer.h>
#include <stdio.h>

HParser *init_bitcoin_transaction_parser() {
    // Basic components
    HParser *hex_digit = h_ch_range(hexdigit);
    HParser *hex_byte = h_repeat_n(hex_digit, 2);
    HParser *whitespace = h_whitespace(h_ch(' '));
    
    // Transaction ID (64 hex characters)
    HParser *tx_id = h_repeat_n(hex_digit, 64);
    
    // Input count (variable length hex)
    HParser *input_count = h_repeat_n(hex_byte, 1);
    
    // Previous output hash (32 bytes)
    HParser *prev_output = h_repeat_n(hex_byte, 32);
    
    // Output index (4 bytes)
    HParser *output_index = h_repeat_n(hex_byte, 4);
    
    // Script length (variable)
    HParser *script_length = h_repeat_n(hex_byte, 1);
    
    // Script (variable length based on script_length)
    HParser *script = h_many1(hex_byte);
    
    // Sequence (4 bytes)
    HParser *sequence = h_repeat_n(hex_byte, 4);
    
    // Input structure
    HParser *input = h_sequence(prev_output,
                               output_index,
                               script_length,
                               script,
                               sequence,
                               NULL);
    
    // Output count
    HParser *output_count = h_repeat_n(hex_byte, 1);
    
    // Value (8 bytes)
    HParser *value = h_repeat_n(hex_byte, 8);
    
    // Output script length
    HParser *out_script_length = h_repeat_n(hex_byte, 1);
    
    // Output script
    HParser *out_script = h_many1(hex_byte);
    
    // Output structure
    HParser *output = h_sequence(value,
                                out_script_length,
                                out_script,
                                NULL);
    
    // Locktime (4 bytes)
    HParser *locktime = h_repeat_n(hex_byte, 4);
    
    // Version (4 bytes)
    HParser *version = h_repeat_n(hex_byte, 4);
    
    // Complete transaction
    return h_sequence(version,
                     tx_id,
                     input_count,
                     h_many1(input),
                     output_count,
                     h_many1(output),
                     locktime,
                     h_end_p(),
                     NULL);
}

int main() {
    HParser *parser = init_bitcoin_transaction_parser();
    
    // Optional test code here
    
    return 0;
}