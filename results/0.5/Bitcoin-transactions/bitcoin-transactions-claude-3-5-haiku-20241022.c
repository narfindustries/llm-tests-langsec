#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    HBytes prev_tx_hash;
    uint32_t prev_output_index;
    HBytes script_sig;
    uint32_t sequence_number;
} TxInput;

typedef struct {
    uint64_t amount;
    HBytes script_pubkey;
} TxOutput;

HParser* parse_varint() {
    return h_choice(
        h_uint8(),
        h_uint16(),
        h_uint32(),
        h_uint64()
    );
}

HParser* parse_fixed_bytes(size_t length) {
    return h_repeat_n(h_uint8(), length);
}

HParser* parse_tx_input() {
    return h_sequence(
        parse_fixed_bytes(32),               // prev_tx_hash
        h_uint32(),                          // prev_output_index
        h_length_value(parse_varint(), h_repeat_n(h_uint8(), SIZE_MAX)),  // script_sig
        h_uint32()                           // sequence_number
    );
}

HParser* parse_tx_output() {
    return h_sequence(
        h_uint64(),                          // amount
        h_length_value(parse_varint(), h_repeat_n(h_uint8(), SIZE_MAX))  // script_pubkey
    );
}

HParser* parse_bitcoin_transaction() {
    return h_sequence(
        h_uint32(),                          // version
        h_length_value(parse_varint(), h_many1(parse_tx_input())),  // inputs
        h_length_value(parse_varint(), h_many1(parse_tx_output())), // outputs
        h_uint32()                            // locktime
    );
}

void print_transaction(const HParseResult* result) {
    const HParsedToken* tx = result->ast;
    
    printf("Transaction Version: %u\n", *(const uint32_t*)h_get_value(h_get_token_value(tx), 0));
    
    const HParsedToken* inputs = h_get_value(h_get_token_value(tx), 1);
    printf("Inputs (%zu):\n", h_get_token_length(inputs));
    
    for (size_t i = 0; i < h_get_token_length(inputs); i++) {
        const HParsedToken* input = h_get_value(h_get_token_value(inputs), i);
        printf("  Input %zu:\n", i);
        
        const HBytes* prev_tx_hash = h_get_value(h_get_token_value(input), 0);
        printf("    Previous TX Hash: ");
        for (size_t j = 0; j < prev_tx_hash->len; j++) {
            printf("%02x", prev_tx_hash->token[j]);
        }
        printf("\n");
        
        printf("    Previous Output Index: %u\n", *(const uint32_t*)h_get_value(h_get_token_value(input), 1));
        
        const HBytes* script_sig = h_get_value(h_get_token_value(input), 2);
        printf("    Script Sig Length: %zu\n", script_sig->len);
        
        printf("    Sequence Number: %u\n", *(const uint32_t*)h_get_value(h_get_token_value(input), 3));
    }
    
    const HParsedToken* outputs = h_get_value(h_get_token_value(tx), 2);
    printf("Outputs (%zu):\n", h_get_token_length(outputs));
    
    for (size_t i = 0; i < h_get_token_length(outputs); i++) {
        const HParsedToken* output = h_get_value(h_get_token_value(outputs), i);
        printf("  Output %zu:\n", i);
        
        printf("    Amount: %lu satoshis\n", *(const uint64_t*)h_get_value(h_get_token_value(output), 0));
        
        const HBytes* script_pubkey = h_get_value(h_get_token_value(output), 1);
        printf("    Script PubKey Length: %zu\n", script_pubkey->len);
    }
    
    printf("Locktime: %u\n", *(const uint32_t*)h_get_value(h_get_token_value(tx), 3));
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <bitcoin_tx_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("Error reading file");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser* parser = parse_bitcoin_transaction();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        print_transaction(result);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Parsing failed\n");
        free(buffer);
        return 1;
    }

    free(buffer);
    h_parser_free(parser);
    return 0;
}