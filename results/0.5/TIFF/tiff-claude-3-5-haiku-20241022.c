#include <hammer/hammer.h>
#include <hammer/glue.h>

static const HParser *tiff_parser;

static const char* parse_tiff_header(const HParseResult *p, void *user_data) {
    // Basic TIFF header validation logic
    const uint8_t *bytes = (const uint8_t*)p->ast->data;
    if (bytes[0] == 0x49 && bytes[1] == 0x49 && 
        bytes[2] == 0x2A && bytes[3] == 0x00) {
        return "Valid TIFF Header";
    }
    return NULL;
}

static HParsedToken* tiff_semantic_action(const HParseResult *p, void *user_data) {
    HParsedToken *token = h_make_token(p->arena, TT_BYTES, p->ast);
    return token;
}

static const HParser* make_tiff_parser() {
    // Intel byte order marker (0x49 0x49)
    HParser *byte_order = h_token("\x49\x49", 2);
    
    // TIFF version (0x2A 0x00)
    HParser *version = h_token("\x2A\x00", 2);
    
    // IFD offset (typically 8)
    HParser *ifd_offset = h_uint32();
    
    // Combine header components
    tiff_parser = h_sequence(byte_order, version, ifd_offset, NULL);
    
    // Add semantic action for validation
    tiff_parser = h_semantic_action(tiff_parser, tiff_semantic_action, NULL, NULL);
    
    return tiff_parser;
}

int main(int argc, char **argv) {
    h_init();
    
    const HParser *parser = make_tiff_parser();
    
    // Example usage of parser
    const char *input_data = "\x49\x49\x2A\x00\x08\x00\x00\x00";
    size_t input_len = 8;
    
    HParseResult *result = h_parse(parser, (const uint8_t*)input_data, input_len);
    
    if (result && result->ast) {
        printf("TIFF parsing successful\n");
    } else {
        printf("TIFF parsing failed\n");
    }
    
    h_destroy();
    return 0;
}