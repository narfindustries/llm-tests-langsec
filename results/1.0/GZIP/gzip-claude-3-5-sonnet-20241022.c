#include <hammer/hammer.h>
#include <stdio.h>

HParser* init_gzip_parser() {
    // Constants and flags
    HParser* id1 = h_ch(0x1f);
    HParser* id2 = h_ch(0x8b);
    HParser* cm = h_ch(0x08);  // Compression method (DEFLATE)
    
    // Flags byte parser
    HParser* flags = h_bits(8, false);
    
    // MTIME (4 bytes)
    HParser* mtime = h_uint32();
    
    // Extra flags and OS
    HParser* xfl = h_uint8();
    HParser* os = h_uint8();
    
    // Optional fields based on flags
    HParser* extra_length = h_uint16();
    HParser* extra_field = h_length_value(extra_length);
    HParser* filename = h_many_till(h_not_in("\0", 1), h_ch(0x00));
    HParser* comment = h_many_till(h_not_in("\0", 1), h_ch(0x00));
    HParser* crc16 = h_uint16();
    
    // Compressed data and footer
    HParser* compressed_data = h_many(h_uint8());
    HParser* crc32 = h_uint32();
    HParser* isize = h_uint32();
    
    // Sequence to check flags and include optional fields
    HParser* optional_fields = h_sequence(
        h_action(flags, act_get_flags),
        h_optional(extra_field),
        h_optional(filename),
        h_optional(comment),
        h_optional(crc16),
        NULL
    );
    
    // Complete GZIP format
    return h_sequence(
        id1,
        id2,
        cm,
        optional_fields,
        mtime,
        xfl,
        os,
        compressed_data,
        crc32,
        isize,
        NULL
    );
}

typedef struct {
    uint8_t ftext : 1;
    uint8_t fhcrc : 1;
    uint8_t fextra : 1;
    uint8_t fname : 1;
    uint8_t fcomment : 1;
    uint8_t reserved : 3;
} GzipFlags;

HParsedToken* act_get_flags(const HParseResult* p) {
    uint8_t flags = H_CAST_UINT(p->ast);
    GzipFlags* gflags = H_ALLOC(GzipFlags);
    
    gflags->ftext = (flags >> 0) & 1;
    gflags->fhcrc = (flags >> 1) & 1;
    gflags->fextra = (flags >> 2) & 1;
    gflags->fname = (flags >> 3) & 1;
    gflags->fcomment = (flags >> 4) & 1;
    gflags->reserved = (flags >> 5) & 7;
    
    return H_MAKE_UINT(flags);
}

int main() {
    HParser* parser = init_gzip_parser();
    
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }
    
    return 0;
}