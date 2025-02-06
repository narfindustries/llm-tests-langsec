#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define the sizes of the fields in bits
#define LI_SIZE 2
#define VN_SIZE 3
#define MODE_SIZE 3
#define STRATUM_SIZE 8
#define POLL_SIZE 8
#define PRECISION_SIZE 8
#define ROOT_DELAY_SIZE 32
#define ROOT_DISPERSION_SIZE 32
#define REF_ID_SIZE 32
#define TIMESTAMP_SIZE 64

// Create parsers for each field
static HParser *ntp_field(uint32_t bit_size) {
    return h_bits(bit_size, false);
}

static HParser *ntp_packet() {
    HParser *ntp = h_sequence(
        ntp_field(LI_SIZE),
        ntp_field(VN_SIZE),
        ntp_field(MODE_SIZE),
        ntp_field(STRATUM_SIZE),
        ntp_field(POLL_SIZE),
        ntp_field(PRECISION_SIZE),
        ntp_field(ROOT_DELAY_SIZE),
        ntp_field(ROOT_DISPERSION_SIZE),
        ntp_field(REF_ID_SIZE),
        ntp_field(TIMESTAMP_SIZE), // reference timestamp
        ntp_field(TIMESTAMP_SIZE), // originate timestamp
        ntp_field(TIMESTAMP_SIZE), // receive timestamp
        ntp_field(TIMESTAMP_SIZE), // transmit timestamp
        NULL
    );
    return ntp;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_ntp_file>\n", argv[0]);
        return 1;
    }

    // Open the binary file containing the NTP packet
    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    // Seek to the end of the file to determine the file size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    // Read the entire file into a buffer
    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory for file buffer");
        fclose(file);
        return 1;
    }
    if (fread(buffer, 1, file_size, file) != file_size) {
        fprintf(stderr, "Failed to read the file\n");
        free(buffer);
        fclose(file);
        return 1;
    }

    // Close the file as we have the data in memory now
    fclose(file);

    // Parse the NTP packet
    HParser *ntp_parser = ntp_packet();
    HParseResult *result = h_parse(ntp_parser, buffer, file_size * 8);  // file_size * 8 to convert byte count to bit count
    if (result) {
        printf("NTP packet parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        fprintf(stderr, "Failed to parse NTP packet.\n");
    }

    // Clean up
    free(buffer);
    h_parse_result_free(result);
    h_parser_cleanup(ntp_parser);  // Use the correct cleanup function 

    return result ? 0 : 1;
}