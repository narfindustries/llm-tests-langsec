#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// ICMP Parser
HParser* init_icmp_parser() {
    // Basic fields
    HParser *type = h_uint8();
    HParser *code = h_uint8();
    HParser *checksum = h_uint16();

    // Rest of header depends on type and code
    HParser *identifier = h_uint16();
    HParser *sequence = h_uint16();
    HParser *pointer = h_uint8();
    HParser *gateway_address = h_uint32();
    HParser *unused = h_uint32();
    HParser *mtu = h_uint16();

    // Optional data
    HParser *data = h_many(h_uint8());

    // Combine fields based on type
    HParser *echo_structure = h_sequence(identifier, sequence, data, NULL);
    HParser *redirect_structure = h_sequence(gateway_address, data, NULL);
    HParser *parameter_problem_structure = h_sequence(pointer, unused, data, NULL);
    HParser *timestamp_structure = h_sequence(identifier, sequence, 
        h_uint32(), h_uint32(), h_uint32(), NULL);
    HParser *info_structure = h_sequence(identifier, sequence, NULL);
    HParser *mask_structure = h_sequence(identifier, sequence, h_uint32(), NULL);

    // Complete ICMP packet
    return h_sequence(type, code, checksum,
        h_choice(echo_structure,
                redirect_structure,
                parameter_problem_structure,
                timestamp_structure,
                info_structure,
                mask_structure,
                data,
                NULL),
        NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    // Read input file
    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *input = malloc(size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(input, 1, size, f) != size) {
        perror("Failed to read file");
        free(input);
        fclose(f);
        return 1;
    }
    fclose(f);

    // Initialize parser
    HParser *icmp_parser = init_icmp_parser();
    if (!icmp_parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        free(input);
        return 1;
    }

    // Parse input
    HParseResult *result = h_parse(icmp_parser, input, size);
    if (!result) {
        fprintf(stderr, "Failed to parse ICMP packet\n");
        free(input);
        return 1;
    }

    // Clean up
    h_parse_result_free(result);
    free(input);
    return 0;
}