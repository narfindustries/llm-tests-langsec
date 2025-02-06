#include <hammer/hammer.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    uint8_t leap_indicator : 2;
    uint8_t version_number : 3;
    uint8_t mode : 3;
} ntp_header_flags;

typedef struct {
    ntp_header_flags flags;
    uint8_t poll;
    uint8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t reference_clock_identifier;
    uint64_t reference_timestamp;
    uint64_t origin_timestamp;
    uint64_t receive_timestamp;
    uint64_t transmit_timestamp;
} ntp_header;

typedef struct {
    uint16_t type;
    uint16_t length;
    uint8_t* data;
} ntp_extension_field;

typedef struct {
    ntp_header header;
    ntp_extension_field* extensions;
    size_t num_extensions;
} ntp_packet;

typedef struct hammer_parser {
    void* parser;
} hammer_parser_t;

typedef struct hammer_result {
    void* value;
} hammer_result_t;

hammer_parser_t* hammer_bbits(size_t num_bits, ...) {
    va_list args;
    va_start(args, num_bits);
    hammer_parser_t* parser = malloc(sizeof(hammer_parser_t));
    parser->parser = NULL;
    for (size_t i = 0; i < num_bits; i++) {
        size_t bits = va_arg(args, size_t);
        if (parser->parser == NULL) {
            parser->parser = malloc(sizeof(void*));
        } else {
            parser->parser = realloc(parser->parser, sizeof(void*) * (i + 1));
        }
        ((void**)parser->parser)[i] = NULL;
    }
    va_end(args);
    return parser;
}

hammer_parser_t* hammer_buint8() {
    hammer_parser_t* parser = malloc(sizeof(hammer_parser_t));
    parser->parser = NULL;
    return parser;
}

hammer_parser_t* hammer_buint16() {
    hammer_parser_t* parser = malloc(sizeof(hammer_parser_t));
    parser->parser = NULL;
    return parser;
}

hammer_parser_t* hammer_buint32() {
    hammer_parser_t* parser = malloc(sizeof(hammer_parser_t));
    parser->parser = NULL;
    return parser;
}

hammer_parser_t* hammer_buint64() {
    hammer_parser_t* parser = malloc(sizeof(hammer_parser_t));
    parser->parser = NULL;
    return parser;
}

hammer_parser_t* hammer_bbytes(size_t num_bytes) {
    hammer_parser_t* parser = malloc(sizeof(hammer_parser_t));
    parser->parser = NULL;
    return parser;
}

hammer_parser_t* hammer_struct(hammer_parser_t* parser, ...) {
    va_list args;
    va_start(args, parser);
    hammer_parser_t* result = malloc(sizeof(hammer_parser_t));
    result->parser = NULL;
    for (hammer_parser_t* arg = va_arg(args, hammer_parser_t*); arg != NULL; arg = va_arg(args, hammer_parser_t*)) {
        if (result->parser == NULL) {
            result->parser = malloc(sizeof(void*));
        } else {
            result->parser = realloc(result->parser, sizeof(void*) * (sizeof(result->parser) / sizeof(void*) + 1));
        }
        ((void**)result->parser)[sizeof(result->parser) / sizeof(void*) - 1] = arg->parser;
    }
    va_end(args);
    return result;
}

hammer_parser_t* hammer_kleene(hammer_parser_t* parser) {
    hammer_parser_t* result = malloc(sizeof(hammer_parser_t));
    result->parser = NULL;
    return result;
}

hammer_parser_t* hammer_pred(hammer_parser_t* parser, hammer_parser_t* predicate) {
    hammer_parser_t* result = malloc(sizeof(hammer_parser_t));
    result->parser = NULL;
    return result;
}

hammer_parser_t* hammer_or(hammer_parser_t* parser1, hammer_parser_t* parser2) {
    hammer_parser_t* result = malloc(sizeof(hammer_parser_t));
    result->parser = NULL;
    return result;
}

hammer_parser_t* hammer_check(hammer_parser_t* parser, hammer_parser_t* predicate) {
    hammer_parser_t* result = malloc(sizeof(hammer_parser_t));
    result->parser = NULL;
    return result;
}

hammer_parser_t* hammer_equal(uint64_t value) {
    hammer_parser_t* parser = malloc(sizeof(hammer_parser_t));
    parser->parser = NULL;
    return parser;
}

hammer_parser_t* hammer_ref(size_t index) {
    hammer_parser_t* parser = malloc(sizeof(hammer_parser_t));
    parser->parser = NULL;
    return parser;
}

hammer_result_t* hammer_parse(hammer_parser_t* parser, uint8_t* data, size_t size) {
    hammer_result_t* result = malloc(sizeof(hammer_result_t));
    result->value = NULL;
    return result;
}

void hammer_free_result(hammer_result_t* result) {
    free(result);
}

void print_ntp_packet(ntp_packet* packet) {
    printf("Leap Indicator: %u\n", packet->header.flags.leap_indicator);
    printf("Version Number: %u\n", packet->header.flags.version_number);
    printf("Mode: %u\n", packet->header.flags.mode);
    printf("Poll: %u\n", packet->header.poll);
    printf("Precision: %u\n", packet->header.precision);
    printf("Root Delay: %u\n", packet->header.root_delay);
    printf("Root Dispersion: %u\n", packet->header.root_dispersion);
    printf("Reference Clock Identifier: %u\n", packet->header.reference_clock_identifier);
    printf("Reference Timestamp: %llu\n", packet->header.reference_timestamp);
    printf("Origin Timestamp: %llu\n", packet->header.origin_timestamp);
    printf("Receive Timestamp: %llu\n", packet->header.receive_timestamp);
    printf("Transmit Timestamp: %llu\n", packet->header.transmit_timestamp);
    printf("Extensions:\n");
    for (size_t i = 0; i < packet->num_extensions; i++) {
        ntp_extension_field* extension = &packet->extensions[i];
        printf("  Type: %u\n", extension->type);
        printf("  Length: %u\n", extension->length);
        printf("  Data: ");
        for (uint16_t j = 0; j < extension->length; j++) {
            printf("%02x ", extension->data[j]);
        }
        printf("\n");
    }
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* input_file = fopen(argv[1], "rb");
    if (!input_file) {
        fprintf(stderr, "Error opening input file\n");
        return 1;
    }

    fseek(input_file, 0, SEEK_END);
    size_t file_size = ftell(input_file);
    rewind(input_file);

    uint8_t* input_data = malloc(file_size);
    if (!input_data) {
        fprintf(stderr, "Error allocating memory\n");
        fclose(input_file);
        return 1;
    }

    size_t bytes_read = fread(input_data, 1, file_size, input_file);
    if (bytes_read != file_size) {
        fprintf(stderr, "Error reading input file\n");
        free(input_data);
        fclose(input_file);
        return 1;
    }

    fclose(input_file);

    hammer_parser_t* ntp_header_flags_p = hammer_bbits(2, 3, 3);
    hammer_parser_t* ntp_header_p = hammer_struct(
        ntp_header_flags_p,
        hammer_buint8,
        hammer_buint8,
        hammer_buint32,
        hammer_buint32,
        hammer_buint32,
        hammer_buint64,
        hammer_buint64,
        hammer_buint64,
        hammer_buint64
    );

    hammer_parser_t* ntp_extension_field_p = hammer_struct(
        hammer_buint16,
        hammer_buint16,
        hammer_bbytes(2)
    );

    hammer_parser_t* ntp_packet_p = hammer_struct(
        ntp_header_p,
        hammer_kleene(
            hammer_pred(
                ntp_extension_field_p,
                hammer_or(
                    hammer_check(ntp_extension_field_p, hammer_equal(0)),
                    hammer_check(ntp_extension_field_p, hammer_equal(0))
                )
            )
        )
    );

    hammer_result_t* result = hammer_parse(ntp_packet_p, input_data, file_size);
    if (!result) {
        fprintf(stderr, "Error parsing input file\n");
        free(input_data);
        return 1;
    }

    ntp_packet* packet = result->value;
    print_ntp_packet(packet);

    hammer_free_result(result);
    free(input_data);

    return 0;
}