#include <hammer/hammer.h>
#include <stdio.h>

static const uint8_t TIFF_LITTLE_ENDIAN[] = {0x49, 0x49, 0x2A, 0x00};
static const uint8_t TIFF_BIG_ENDIAN[] = {0x4D, 0x4D, 0x00, 0x2A};

HParser* init_tiff_parser(void) {
    // IFD Entry parser
    HParser* tag = h_uint16();
    HParser* type = h_uint16();
    HParser* count = h_uint32();
    HParser* value_offset = h_uint32();
    HParser* ifd_entry = h_sequence(tag, type, count, value_offset, NULL);
    
    // IFD parser
    HParser* num_entries = h_uint16();
    HParser* entries = h_repeat_n(ifd_entry, 1);
    HParser* next_ifd_offset = h_uint32();
    HParser* ifd = h_sequence(num_entries, entries, next_ifd_offset, NULL);
    
    // Header parser
    HParser* endianness = h_choice(h_token(TIFF_LITTLE_ENDIAN, 4),
                                 h_token(TIFF_BIG_ENDIAN, 4),
                                 NULL);
    HParser* first_ifd_offset = h_uint32();
    
    // Complete TIFF parser
    return h_sequence(endianness, first_ifd_offset, ifd, NULL);
}

int main(int argc, char** argv) {
    HParser* parser = init_tiff_parser();
    
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }
    
    return 0;
}