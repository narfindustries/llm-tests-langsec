#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parsers for ICMP fields
static HParser *icmp_type;
static HParser *icmp_code;
static HParser *icmp_checksum;
static HParser *icmp_rest_of_header;
static HParser *icmp_data;

// Initialize parsers
void init_parsers() {
    icmp_type = h_uint8();
    icmp_code = h_uint8();
    icmp_checksum = h_uint16();
    icmp_rest_of_header = h_bits(32, false); // 32-bit field for rest of header, not byte aligned
    icmp_data = h_bits(384, true); // Assume some arbitrary amount of data
}

// Read the ICMP packet from a file
uint8_t *read_file(const char *filename, size_t *size) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    *size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(*size);
    if (!data) {
        perror("Memory allocation failed");
        fclose(file);
        return NULL;
    }

    if (fread(data, 1, *size, file) != *size) {
        perror("Failed to read file");
        free(data);
        fclose(file);
        return NULL;
    }

    fclose(file);
    return data;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <icmp_packet_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    size_t size;
    uint8_t *data = read_file(argv[1], &size);
    if (!data) return EXIT_FAILURE;

    // Initialize parsers
    init_parsers();

    // Construct the packet parser
    HParser *icmp_packet = h_sequence(icmp_type, icmp_code, icmp_checksum, icmp_rest_of_header, icmp_data, NULL);

    // Parse the data
    HParseResult *result = h_parse(icmp_packet, data, size);
    if (result) {
        if (result->bit_length / 8 != size) {
            printf("Failed to parse complete ICMP packet, or incorrect packet size.\n");
        } else {
            printf("ICMP packet parsed successfully.\n");
        }

        h_release_parse_result(result); // Using correct function call for Hammer parser clean up
    } else {
        printf("Failed to parse ICMP packet.\n");
    }

    free(data);

    return EXIT_SUCCESS;
}