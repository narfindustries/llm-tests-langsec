#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Custom action functions prototypes
static HParsedToken *act_parse_uint16(const HParseResult *p, void *user_data);
static HParsedToken *act_parse_uint8(const HParseResult *p, void *user_data);
static HParsedToken *act_parse_data(const HParseResult *p, void *user_data);

// Parser creation function
static HParser *create_modbus_tcp_parser(size_t data_size);

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <modbus_binary_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(fp, 0, SEEK_END);
    size_t file_size = ftell(fp);
    rewind(fp);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        fclose(fp);
        fprintf(stderr, "Memory allocation failed\n");
        return EXIT_FAILURE;
    }

    if (fread(buffer, 1, file_size, fp) != file_size) {
        fclose(fp);
        free(buffer);
        fprintf(stderr, "Failed to read file\n");
        return EXIT_FAILURE;
    }
    fclose(fp);

    HParser *modbus_tcp_parser = create_modbus_tcp_parser(file_size - 9);  // excluding non-data fields
    HParseResult *result = h_parse(modbus_tcp_parser, buffer, file_size);

    if (result) {
        printf("Parsing successful.\n");
    } else {
        printf("Parsing failed.\n");
    }

    h_parse_result_free(result);
    h_parser_unref(modbus_tcp_parser);
    free(buffer);

    return 0;
}

static HParsedToken *act_parse_uint16(const HParseResult *p, void *user_data) {
    uint16_t num = *(const uint16_t *)p->ast->token;
    return H_MAKE_UINT(num);
}

static HParsedToken *act_parse_uint8(const HParseResult *p, void *user_data) {
    uint8_t num = *(const uint8_t *)p->ast->token;
    return H_MAKE_UINT(num);
}

static HParsedToken *act_parse_data(const HParseResult *p, void *user_data) {
    // Assuming the entire payload as the data
    return h_make_bytes(p->ast->token, p->ast->seq->used);
}

static HParser *create_modbus_tcp_parser(size_t data_size) {
    HParser *p_uint16 = h_bind(h_uint16(), act_parse_uint16, NULL);
    HParser *p_uint8 = h_bind(h_uint8(), act_parse_uint8, NULL);
    HParser *p_data = h_bind(h_repeat_n(h_uint8(), data_size), act_parse_data, NULL);

    return h_sequence(
        p_uint16, // Transaction Identifier
        p_uint16, // Protocol Identifier
        p_uint16, // Length
        p_uint8,  // Unit Identifier
        p_uint8,  // Function Code
        p_data,   // Data
        p_uint16, // CRC Checksum (assuming it is a uint16)
        NULL
    );
}