#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    uint8_t li_vn_mode;
    uint8_t stratum;
    uint8_t poll;
    uint8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t reference_id;
    uint64_t reference_timestamp;
    uint64_t origin_timestamp;
    uint64_t receive_timestamp;
    uint64_t transmit_timestamp;
} ntp_packet;

static void print_token(const HParseResult *result) {
    if (!result || !result->ast) return;
    
    const HParsedToken *token = result->ast;
    const HCountedArray *seq = token->seq;
    
    if (!seq || seq->used < 11) return;
    
    printf("LI_VN_MODE: 0x%02x\n", (uint8_t)seq->elements[0]->uint);
    printf("Stratum: %u\n", (uint8_t)seq->elements[1]->uint);
    printf("Poll: %d\n", (int8_t)seq->elements[2]->uint);
    printf("Precision: %d\n", (int8_t)seq->elements[3]->uint);
    printf("Root Delay: %u\n", (uint32_t)seq->elements[4]->uint);
    printf("Root Dispersion: %u\n", (uint32_t)seq->elements[5]->uint);
    printf("Reference ID: 0x%08x\n", (uint32_t)seq->elements[6]->uint);
    printf("Reference Timestamp: %lu\n", (uint64_t)seq->elements[7]->uint);
    printf("Origin Timestamp: %lu\n", (uint64_t)seq->elements[8]->uint);
    printf("Receive Timestamp: %lu\n", (uint64_t)seq->elements[9]->uint);
    printf("Transmit Timestamp: %lu\n", (uint64_t)seq->elements[10]->uint);

    if (seq->used > 11 && seq->elements[11]->seq) {
        const HCountedArray *ext_fields = seq->elements[11]->seq;
        for (size_t i = 0; i < ext_fields->used; i++) {
            const HCountedArray *ext = ext_fields->elements[i]->seq;
            if (ext && ext->used >= 2) {
                printf("Extension Field Type: %u\n", (uint16_t)ext->elements[0]->uint);
                printf("Extension Field Length: %u\n", (uint16_t)ext->elements[1]->uint);
            }
        }
    }

    if (seq->used > 12 && seq->elements[12] && seq->elements[12]->seq) {
        const HCountedArray *mac = seq->elements[12]->seq;
        if (mac && mac->used >= 1) {
            printf("MAC Key ID: %u\n", (uint32_t)mac->elements[0]->uint);
        }
    }
}

static HParser* create_ntp_parser(void) {
    return h_sequence(
        h_uint8(),                // LI_VN_MODE
        h_uint8(),                // Stratum
        h_uint8(),                // Poll
        h_uint8(),                // Precision
        h_uint32(),               // Root Delay
        h_uint32(),               // Root Dispersion
        h_uint32(),               // Reference ID
        h_uint64(),               // Reference Timestamp
        h_uint64(),               // Origin Timestamp
        h_uint64(),               // Receive Timestamp
        h_uint64(),               // Transmit Timestamp
        
        // Extension Fields
        h_many(h_sequence(
            h_uint16(),           // Field Type
            h_uint16(),           // Length
            h_length_value(h_left(h_uint16(), h_nothing_p()), h_uint8()),
            NULL
        )),
        
        // Optional MAC
        h_optional(h_sequence(
            h_uint32(),           // Key ID
            h_length_value(h_int_range(h_uint16(), 16, 20), h_uint8()),
            NULL
        )),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ntp_packet_file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *data = malloc(size);
    if (!data) {
        fclose(f);
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }

    if (fread(data, 1, size, f) != size) {
        free(data);
        fclose(f);
        fprintf(stderr, "Failed to read file\n");
        return 1;
    }

    HParser *parser = create_ntp_parser();
    if (!parser) {
        free(data);
        fclose(f);
        fprintf(stderr, "Failed to create parser\n");
        return 1;
    }

    HParseResult *result = h_parse(parser, data, size);
    if (result) {
        print_token(result);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Parsing failed\n");
    }

    free(data);
    fclose(f);
    return 0;
}