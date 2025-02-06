#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// VarInt parser
HParser* varint_parser() {
    return h_choice(
        h_uint8(),  // < 0xFD
        h_sequence(h_ch(0xFD), h_uint16(), NULL),  // <= 0xFFFF
        h_sequence(h_ch(0xFE), h_uint32(), NULL),  // <= 0xFFFFFFFF
        h_sequence(h_ch(0xFF), h_uint64(), NULL),  // > 0xFFFFFFFF
        NULL
    );
}

// Transaction Input parser
HParser* tx_input_parser() {
    return h_sequence(
        h_repeat_n(h_uint8(), 32),  // Previous Transaction Hash (32 bytes)
        h_uint32(),                 // Previous Output Index (4 bytes)
        varint_parser(),            // Script Length
        h_length_value(varint_parser(), h_uint8()),  // ScriptSig
        h_uint32(),                 // Sequence
        NULL
    );
}

// Transaction Output parser
HParser* tx_output_parser() {
    return h_sequence(
        h_uint64(),                 // Value (8 bytes)
        varint_parser(),            // Script Length
        h_length_value(varint_parser(), h_uint8()),  // ScriptPubKey
        NULL
    );
}

// Complete Transaction parser
HParser* transaction_parser() {
    return h_sequence(
        h_uint32(),                 // Version
        varint_parser(),            // Input Counter
        h_length_value(varint_parser(), tx_input_parser()),  // Inputs
        varint_parser(),            // Output Counter
        h_length_value(varint_parser(), tx_output_parser()), // Outputs
        h_uint32(),                 // Locktime
        NULL
    );
}

void print_parse_result(const HParsedToken* result, int depth) {
    for(int i = 0; i < depth; i++) printf("  ");
    
    switch(result->token_type) {
        case TT_SEQUENCE:
            printf("Sequence:\n");
            for(size_t i = 0; i < result->seq->used; i++) {
                print_parse_result(result->seq->elements[i], depth + 1);
            }
            break;
        case TT_UINT:
            printf("Integer: %lu\n", result->uint);
            break;
        case TT_BYTES:
            printf("Bytes: ");
            for(size_t i = 0; i < result->bytes.len; i++) {
                printf("%02x", result->bytes.token[i]);
            }
            printf("\n");
            break;
        default:
            printf("Unknown token type\n");
    }
}

int main(int argc, char** argv) {
    if(argc != 2) {
        fprintf(stderr, "Usage: %s <bitcoin_transaction_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if(!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = malloc(file_size);
    if(!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if(fread(buffer, 1, file_size, file) != (size_t)file_size) {
        perror("Failed to read file");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParser* parser = transaction_parser();
    if(!parser) {
        fprintf(stderr, "Failed to create parser\n");
        free(buffer);
        fclose(file);
        return 1;
    }

    const HParsedToken* result = h_parse(parser, buffer, file_size);
    if(!result) {
        fprintf(stderr, "Failed to parse transaction\n");
        free(buffer);
        fclose(file);
        return 1;
    }

    printf("Parsed Bitcoin Transaction:\n");
    print_parse_result(result, 0);

    h_parse_result_free(result);
    free(buffer);
    fclose(file);
    return 0;
}