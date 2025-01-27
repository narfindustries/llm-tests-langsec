#include <hammer/hammer.h>
#include <stdio.h>

static const uint8_t TIFF_HEADER[] = {0x49, 0x49, 0x2A, 0x00};

HParser* init_tiff_parser() {
    // Basic integer parsers
    HParser* uint8 = h_uint8();
    HParser* uint16_le = h_uint16();
    HParser* uint32_le = h_uint32();

    // Header parser
    HParser* header = h_token(TIFF_HEADER, sizeof(TIFF_HEADER));

    // IFD entry structure
    HParser* ifd_entry = h_sequence(
        uint16_le,  // Tag
        uint16_le,  // Type
        uint32_le,  // Count
        uint32_le,  // Value/Offset
        NULL
    );

    // Number of directory entries followed by entries
    HParser* ifd_count = uint16_le;
    HParser* ifd_entries = h_repeat_n(ifd_entry, 1);
    HParser* ifd = h_sequence(
        ifd_count,
        ifd_entries,
        uint32_le,  // Next IFD offset
        NULL
    );

    // Complete TIFF parser
    HParser* tiff_parser = h_sequence(
        header,     // TIFF header
        uint32_le,  // IFD offset
        ifd,        // First IFD
        NULL
    );

    return tiff_parser;
}

int main() {
    HParser* parser = init_tiff_parser();
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }

    // Clean up
    h_delete_parser(parser);
    return 0;
}