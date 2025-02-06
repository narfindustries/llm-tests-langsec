#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Parser for Local File Header
HParser* create_local_file_header_parser() {
    return h_sequence(
        h_token((uint8_t*)"\x50\x4b\x03\x04", 4),  // signature
        h_uint16(),                                 // version needed
        h_uint16(),                                 // general purpose flag
        h_uint16(),                                 // compression method
        h_uint16(),                                 // last mod time
        h_uint16(),                                 // last mod date
        h_uint32(),                                 // crc-32
        h_uint32(),                                 // compressed size
        h_uint32(),                                 // uncompressed size
        h_length_value(h_uint16(), h_uint8()),     // filename
        h_length_value(h_uint16(), h_uint8()),     // extra field
        NULL
    );
}

// Parser for Central Directory Header
HParser* create_central_directory_parser() {
    return h_sequence(
        h_token((uint8_t*)"\x50\x4b\x01\x02", 4),  // signature
        h_uint16(),                                 // version made by
        h_uint16(),                                 // version needed
        h_uint16(),                                 // general purpose flag
        h_uint16(),                                 // compression method
        h_uint16(),                                 // last mod time
        h_uint16(),                                 // last mod date
        h_uint32(),                                 // crc-32
        h_uint32(),                                 // compressed size
        h_uint32(),                                 // uncompressed size
        h_length_value(h_uint16(), h_uint8()),     // filename length + filename
        h_length_value(h_uint16(), h_uint8()),     // extra field length + extra field
        h_length_value(h_uint16(), h_uint8()),     // file comment length + comment
        h_uint16(),                                 // disk number start
        h_uint16(),                                 // internal attributes
        h_uint32(),                                 // external attributes
        h_uint32(),                                 // relative offset
        NULL
    );
}

// Parser for End of Central Directory
HParser* create_end_of_central_directory_parser() {
    return h_sequence(
        h_token((uint8_t*)"\x50\x4b\x05\x06", 4),  // signature
        h_uint16(),                                 // disk number
        h_uint16(),                                 // disk with central directory
        h_uint16(),                                 // entries on this disk
        h_uint16(),                                 // total entries
        h_uint32(),                                 // central directory size
        h_uint32(),                                 // central directory offset
        h_length_value(h_uint16(), h_uint8()),     // comment length + comment
        NULL
    );
}

// Parser for Data Descriptor (optional)
HParser* create_data_descriptor_parser() {
    return h_sequence(
        h_token((uint8_t*)"\x50\x4b\x07\x08", 4),  // signature
        h_uint32(),                                 // crc-32
        h_uint32(),                                 // compressed size
        h_uint32(),                                 // uncompressed size
        NULL
    );
}

// Parser for ZIP64 End of Central Directory Record
HParser* create_zip64_end_of_central_directory_parser() {
    return h_sequence(
        h_token((uint8_t*)"\x50\x4b\x06\x06", 4),  // signature
        h_uint64(),                                 // size of record
        h_uint16(),                                 // version made by
        h_uint16(),                                 // version needed
        h_uint32(),                                 // disk number
        h_uint32(),                                 // disk with central directory
        h_uint64(),                                 // entries on this disk
        h_uint64(),                                 // total entries
        h_uint64(),                                 // central directory size
        h_uint64(),                                 // central directory offset
        NULL
    );
}

// Parser for ZIP64 End of Central Directory Locator
HParser* create_zip64_end_of_central_directory_locator_parser() {
    return h_sequence(
        h_token((uint8_t*)"\x50\x4b\x06\x07", 4),  // signature
        h_uint32(),                                 // disk with zip64 end
        h_uint64(),                                 // offset of zip64 end
        h_uint32(),                                 // total number of disks
        NULL
    );
}

// Main ZIP parser combining all structures
HParser* create_zip_parser() {
    return h_many1(h_choice(
        create_local_file_header_parser(),
        create_central_directory_parser(),
        create_end_of_central_directory_parser(),
        create_data_descriptor_parser(),
        create_zip64_end_of_central_directory_parser(),
        create_zip64_end_of_central_directory_locator_parser(),
        NULL
    ));
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    // Get file size
    fseek(fp, 0, SEEK_END);
    long file_size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    // Read file into buffer
    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(fp);
        return 1;
    }
    
    size_t bytes_read = fread(buffer, 1, file_size, fp);
    if (bytes_read != file_size) {
        perror("Failed to read file");
        free(buffer);
        fclose(fp);
        return 1;
    }

    // Initialize parser
    HParser *zip_parser = create_zip_parser();
    
    // Parse input
    HParseResult *result = h_parse(zip_parser, buffer, file_size);
    
    if (result) {
        printf("Successfully parsed ZIP file\n");
    } else {
        printf("Failed to parse ZIP file\n");
    }

    // Cleanup
    h_parse_result_free(result);
    free(buffer);
    fclose(fp);
    
    return 0;
}