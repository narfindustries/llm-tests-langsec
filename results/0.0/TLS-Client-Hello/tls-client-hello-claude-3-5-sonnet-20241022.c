#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Extension Types
#define EXT_SERVER_NAME 0x0000
#define EXT_SUPPORTED_GROUPS 0x000a
#define EXT_SIGNATURE_ALGORITHMS 0x000d
#define EXT_PADDING 0x0015
#define EXT_PRE_SHARED_KEY 0x0029
#define EXT_EARLY_DATA 0x002a
#define EXT_SUPPORTED_VERSIONS 0x002b
#define EXT_COOKIE 0x002c
#define EXT_PSK_KEY_EXCHANGE_MODES 0x002d
#define EXT_KEY_SHARE 0x0033

static HParser* make_vector(HParser* element, HParser* length) {
    return h_length_value(length, element);
}

static HParser* server_name_parser(void) {
    return h_sequence(
        h_uint16(),
        h_sequence(
            h_uint8(),
            h_length_value(h_uint16(), h_uint8()),
            NULL
        ),
        NULL
    );
}

static HParser* supported_groups_parser(void) {
    return h_length_value(h_uint16(), h_many1(h_uint16()));
}

static HParser* signature_algorithms_parser(void) {
    return h_length_value(h_uint16(), h_many1(h_uint16()));
}

static HParser* key_share_entry_parser(void) {
    return h_sequence(
        h_uint16(),
        h_length_value(h_uint16(), h_uint8()),
        NULL
    );
}

static HParser* key_share_parser(void) {
    return h_length_value(h_uint16(), h_many1(key_share_entry_parser()));
}

static HParser* extension_parser(void) {
    return h_sequence(
        h_uint16(),
        h_length_value(h_uint16(), h_uint8()),
        NULL
    );
}

static HParser* init_client_hello_parser(void) {
    return h_sequence(
        h_uint16(),
        h_repeat_n(h_uint8(), 32),
        h_length_value(h_uint8(), h_uint8()),
        h_length_value(h_uint16(), h_many1(h_uint16())),
        h_length_value(h_uint8(), h_uint8()),
        h_length_value(h_uint16(), h_many1(extension_parser())),
        NULL
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t* data = malloc(size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(data, 1, size, f) != size) {
        perror("Failed to read file");
        free(data);
        fclose(f);
        return 1;
    }
    fclose(f);

    HParser* parser = init_client_hello_parser();
    HParseResult* result = h_parse(parser, data, size);
    if (!result) {
        fprintf(stderr, "Failed to parse ClientHello\n");
        free(data);
        return 1;
    }

    h_parse_result_free(result);
    free(data);
    return 0;
}