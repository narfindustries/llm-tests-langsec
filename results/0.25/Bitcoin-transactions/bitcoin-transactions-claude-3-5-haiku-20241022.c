#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>
#include <hammer/allocator.h>

typedef struct {
    uint32_t version;
    size_t input_count;
    HParser* inputs;
    size_t output_count;
    HParser* outputs;
    uint32_t locktime;
} BitcoinTransaction;

typedef struct {
    uint8_t prev_tx_hash[32];
    uint32_t prev_output_index;
    size_t scriptsig_length;
    uint8_t* scriptsig;
    uint32_t sequence_number;
} TransactionInput;

typedef struct {
    uint64_t amount;
    size_t scriptpubkey_length;
    uint8_t* scriptpubkey;
} TransactionOutput;

HParser* bitcoin_transaction_parser() {
    HArena* arena = h_new_arena();
    
    HParser* version = h_int32();
    
    HParser* var_int = h_choice(
        h_uint8(),
        h_uint16(),
        h_uint32(),
        h_uint64(),
        NULL
    );
    
    HParser* input_count_parser = var_int;
    
    HParser* input_parser = h_sequence(
        h_repeat_n(h_uint8(), 32),  // prev tx hash
        h_uint32(),                 // prev output index
        var_int,                    // scriptsig length
        h_length_value(h_uint8(), var_int),  // scriptsig
        h_uint32(),                 // sequence number
        NULL
    );
    
    HParser* inputs_parser = h_many(input_parser);
    
    HParser* output_count_parser = var_int;
    
    HParser* output_parser = h_sequence(
        h_uint64(),                 // amount
        var_int,                    // scriptpubkey length
        h_length_value(h_uint8(), var_int),  // scriptpubkey
        NULL
    );
    
    HParser* outputs_parser = h_many(output_parser);
    
    HParser* locktime = h_uint32();
    
    HParser* transaction_parser = h_sequence(
        version,
        input_count_parser,
        inputs_parser,
        output_count_parser,
        outputs_parser,
        locktime,
        NULL
    );
    
    return transaction_parser;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <bitcoin_transaction_file>\n", argv[0]);
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
    
    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);
    
    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }
    
    HArena* arena = h_new_arena();
    HParser* parser = bitcoin_transaction_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);
    
    if (result && result->ast) {
        printf("Transaction parsed successfully\n");
    } else {
        printf("Transaction parsing failed\n");
    }
    
    h_parse_result_free(result);
    h_arena_free(arena, parser);
    h_arena_free(arena, buffer);
    
    return 0;
}