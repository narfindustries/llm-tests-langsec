#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    union {
        struct {
            uint16_t identifier;
            uint16_t sequence;
        } echo;
        uint32_t gateway_addr;
        struct {
            uint8_t pointer;
            uint8_t unused[3];
        } param_prob;
        uint32_t unused;
    } rest_header;
    uint8_t *data;
    size_t data_length;
} icmp_message;

static HParser *create_icmp_parser(void) {
    HParser *type = h_uint8();
    HParser *code = h_uint8();
    HParser *checksum = h_uint16();
    HParser *echo_struct = h_sequence(h_uint16(), h_uint16(), NULL);
    HParser *gateway = h_uint32();
    HParser *param_prob = h_sequence(h_uint8(), h_repeat_n(h_uint8(), 3), NULL);
    HParser *data = h_many(h_uint8());

    HParser *rest_header = h_choice(echo_struct,
                                  h_uint32(),
                                  h_uint32(),
                                  gateway,
                                  h_uint32(),
                                  param_prob,
                                  echo_struct,
                                  echo_struct,
                                  NULL);

    return h_sequence(type, code, checksum, rest_header, data, NULL);
}

static void print_icmp_message(const HParseResult *result) {
    if (!result || !result->ast) {
        printf("Failed to parse ICMP message\n");
        return;
    }

    const HParsedToken *ast = result->ast;
    if (ast->token_type != TT_SEQUENCE) return;

    uint8_t type = ast->seq->elements[0]->uint;
    uint8_t code = ast->seq->elements[1]->uint;
    uint16_t checksum = ast->seq->elements[2]->uint;

    printf("ICMP Message:\n");
    printf("Type: %u\n", type);
    printf("Code: %u\n", code);
    printf("Checksum: 0x%04x\n", checksum);

    const HParsedToken *rest_header = ast->seq->elements[3];
    switch(type) {
        case 0:
        case 8:
        case 13:
        case 14:
        case 15:
        case 16:
            if (rest_header->token_type == TT_SEQUENCE) {
                printf("Identifier: %u\n", rest_header->seq->elements[0]->uint);
                printf("Sequence: %u\n", rest_header->seq->elements[1]->uint);
            }
            break;
        case 5:
            printf("Gateway Address: 0x%08x\n", rest_header->uint);
            break;
        case 12:
            if (rest_header->token_type == TT_SEQUENCE) {
                printf("Pointer: %u\n", rest_header->seq->elements[0]->uint);
            }
            break;
    }

    const HParsedToken *data = ast->seq->elements[4];
    if (data->token_type == TT_SEQUENCE) {
        printf("Data length: %zu bytes\n", data->seq->used);
        printf("Data: ");
        for (size_t i = 0; i < data->seq->used; i++) {
            printf("%02x ", data->seq->elements[i]->uint);
        }
        printf("\n");
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <icmp_binary_file>\n", argv[0]);
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
        fclose(file);
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != (size_t)file_size) {
        free(buffer);
        fclose(file);
        fprintf(stderr, "Failed to read file\n");
        return 1;
    }

    HParser *parser = create_icmp_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);
    
    print_icmp_message(result);

    h_parse_result_free(result);
    free(buffer);
    fclose(file);
    return 0;
}