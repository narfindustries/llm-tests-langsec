#include <hammer/hammer.h>

typedef struct {
    uint16_t id;
    uint32_t extra_flags;
    uint8_t os_type;
} gzip_header_t;

static HParsedToken *gzip_header_action(const HParseResult *p, void *user_data) {
    gzip_header_t *header = malloc(sizeof(gzip_header_t));
    header->id = H_FIELD_UINT(p, 0);
    header->extra_flags = H_FIELD_UINT(p, 1);
    header->os_type = H_FIELD_UINT(p, 2);
    return H_MAKE(H_BYTES(header, sizeof(gzip_header_t)));
}

int main(int argc, char **argv) {
    h_parse_ensure();
    
    HParser *gzip_parser = h_sequence(
        h_uint16(), // ID
        h_uint32(), // Extra Flags
        h_uint8(),  // OS Type
        NULL
    );

    HParser *gzip_header_parser = h_action(gzip_parser, gzip_header_action, NULL);
    
    const uint8_t sample_data[] = {0x1f, 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03};
    HParseResult *result = h_parse(gzip_header_parser, sample_data, sizeof(sample_data));
    
    if (result != NULL) {
        gzip_header_t *header = result->ast->user;
        printf("ID: %u, Extra Flags: %u, OS Type: %u\n", header->id, header->extra_flags, header->os_type);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse GZIP header\n");
    }

    h_parser_unref(gzip_header_parser);
    return 0;
}