#include "hammer.h"
#include <stdio.h>

static const uint8_t ZIP_LOCAL_FILE_HEADER_SIGNATURE[] = {0x50, 0x4b, 0x03, 0x04};

HParser *init_zip_parser() {
    // File name field
    HParser *file_name = h_many1(h_ch_range(0x20, 0x7E));
    
    // Extra field
    HParser *extra_field = h_many(h_uint8());
    
    // File data
    HParser *file_data = h_many(h_uint8());
    
    // Version needed
    HParser *version = h_uint16();
    
    // General purpose bit flag
    HParser *flags = h_uint16();
    
    // Compression method
    HParser *compression = h_uint16();
    
    // Last mod time and date
    HParser *mod_time = h_uint16();
    HParser *mod_date = h_uint16();
    
    // CRC-32
    HParser *crc32 = h_uint32();
    
    // Compressed and uncompressed sizes
    HParser *comp_size = h_uint32();
    HParser *uncomp_size = h_uint32();
    
    // File name length and extra field length
    HParser *fname_length = h_uint16();
    HParser *extra_length = h_uint16();
    
    // Local file header signature
    HParser *signature = h_token(ZIP_LOCAL_FILE_HEADER_SIGNATURE, sizeof(ZIP_LOCAL_FILE_HEADER_SIGNATURE));
    
    // Combine all fields in sequence
    HParser *local_file_header = h_sequence(
        signature,
        version,
        flags,
        compression,
        mod_time,
        mod_date,
        crc32,
        comp_size,
        uncomp_size,
        fname_length,
        extra_length,
        file_name,
        extra_field,
        file_data,
        NULL
    );
    
    // Allow multiple local file headers in sequence
    return h_many1(local_file_header);
}