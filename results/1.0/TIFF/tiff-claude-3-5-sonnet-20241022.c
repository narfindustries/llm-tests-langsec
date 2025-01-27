#include <hammer/hammer.h>
#include <stdio.h>

static const uint8_t image_file_header[] = {0x49, 0x49, 0x2A, 0x00};

HParser* init_tiff_parser() {
    // IFD Entry parser
    HParser* tag = h_uint16();
    HParser* type = h_uint16();
    HParser* count = h_uint32();
    HParser* value_offset = h_uint32();
    HParser* ifd_entry = h_sequence(tag, type, count, value_offset, NULL);
    
    // IFD parser
    HParser* num_entries = h_uint16();
    HParser* entries = h_repeat_n(ifd_entry, h_action(num_entries, act_take_value));
    HParser* next_ifd_offset = h_uint32();
    HParser* ifd = h_sequence(num_entries, entries, next_ifd_offset, NULL);
    
    // Header parser
    HParser* header = h_token(image_file_header, sizeof(image_file_header));
    HParser* offset = h_uint32();
    
    // Complete TIFF parser
    return h_sequence(header, offset, ifd, NULL);
}

int main(int argc, char* argv[]) {
    HParser* parser = init_tiff_parser();
    
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }
    
    HParseResult* result = h_parse(parser, (const uint8_t*)"", 0);
    if (result) {
        h_parse_result_free(result);
    }
    
    h_parse_result_free(result);
    return 0;
}