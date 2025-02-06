#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Function to parse ICMP type
static HParsedToken *act_type(const HParseResult *p, void *user_data) {
    return h_int_token_new(h_seq_index(p->ast, 0));
}

// Function to parse ICMP code
static HParsedToken *act_code(const HParseResult *p, void *user_data) {
    return h_int_token_new(h_seq_index(p->ast, 1));
}

// Function to parse ICMP checksum
static HParsedToken *act_checksum(const HParseResult *p, void *user_data) {
    return h_int_token_new(h_seq_index(p->ast, 2));
}

// Parser for ICMP message
static HParser *icmp_parser() {
    HParser *type = h_action(h_uint8(), act_type, NULL);
    HParser *code = h_action(h_uint8(), act_code, NULL);
    HParser *checksum = h_action(h_uint16(), act_checksum, NULL);
    HParser *rest = h_bytes(1);  // Placeholder for remaining bytes

    return h_sequence(type, code, checksum, rest, NULL);
}

// Main function
int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <icmp_binary_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    // Seek to the end of the file to determine the file size
    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    rewind(file);

    // Read the file into a buffer
    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        fprintf(stderr, "Failed to allocate memory\n");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        fprintf(stderr, "Failed to read file\n");
        free(buffer);
        fclose(file);
        return 1;
    }

    // Create a parser for ICMP
    HParser *parser = icmp_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("ICMP packet parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Failed to parse ICMP packet.\n");
    }

    // Cleanup
    h_parse_result_free(result);
    h_parser_unref(parser);
    free(buffer);
    fclose(file);

    return 0;
}