#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define the ARP parser
HParser *arp_parser() {
    return h_sequence(
        h_int16(),  // HTYPE
        h_int16(),  // PTYPE
        h_uint8(),  // HLEN
        h_uint8(),  // PLEN
        h_int16(),  // OPER
        h_length_value(h_uint8(), h_bits(6 * 8, false)),  // SHA
        h_length_value(h_uint8(), h_bits(4 * 8, false)),  // SPA
        h_length_value(h_uint8(), h_bits(6 * 8, false)),  // THA
        h_length_value(h_uint8(), h_bits(4 * 8, false)),  // TPA
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = arp_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("ARP packet parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ARP packet.\n");
    }

    free(buffer);
    return 0;
}