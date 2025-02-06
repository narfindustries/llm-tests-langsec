#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define constants for signatures
#define LOCAL_FILE_HEADER_SIG 0x04034b50
#define CENTRAL_DIR_HEADER_SIG 0x02014b50
#define END_OF_CENTRAL_DIR_SIG 0x06054b50

// Helper function to parse variable length data
HParser* parse_var_length_data() {
    return h_length_value(h_int16(), h_uint8());
}

// Function to parse a ZIP local file header
HParser* local_file_header() {
    return h_sequence(
        h_int32(),  // Signature
        h_int16(),  // Version needed to extract
        h_int16(),  // General purpose bit flag
        h_int16(),  // Compression method
        h_int16(),  // Last mod file time
        h_int16(),  // Last mod file date
        h_int32(),  // CRC-32
        h_int32(),  // Compressed size
        h_int32(),  // Uncompressed size
        parse_var_length_data(),  // File name
        parse_var_length_data(),  // Extra field
        NULL
    );
}

// Function to parse a ZIP central directory file header
HParser* central_directory_file_header() {
    return h_sequence(
        h_int32(),  // Signature
        h_int16(),  // Version made by
        h_int16(),  // Version needed to extract
        h_int16(),  // General purpose bit flag
        h_int16(),  // Compression method
        h_int16(),  // Last mod file time
        h_int16(),  // Last mod file date
        h_int32(),  // CRC-32
        h_int32(),  // Compressed size
        h_int32(),  // Uncompressed size
        parse_var_length_data(),  // File name
        parse_var_length_data(),  // Extra field
        parse_var_length_data(),  // File comment
        h_int16(),  // Disk number start
        h_int16(),  // Internal file attributes
        h_int32(),  // External file attributes
        h_int32(),  // Relative offset of local header
        NULL
    );
}

// Function to parse a ZIP end of central directory record
HParser* end_of_central_directory_record() {
    return h_sequence(
        h_int32(),  // Signature
        h_int16(),  // Number of this disk
        h_int16(),  // Disk where central directory starts
        h_int16(),  // Number of central directory records on this disk
        h_int16(),  // Total number of central directory records
        h_int32(),  // Size of central directory
        h_int32(),  // Offset of start of central directory
        parse_var_length_data(),  // ZIP file comment
        NULL
    );
}

// Main function
int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("File opening failed");
        return EXIT_FAILURE;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *data = malloc(fsize);
    fread(data, 1, fsize, fp);
    fclose(fp);

    HParser *zip_parser = h_sequence(
        h_many(local_file_header()),
        h_many(central_directory_file_header()),
        end_of_central_directory_record(),
        NULL
    );

    HParseResult *result = h_parse(zip_parser, data, fsize);
    if (result) {
        printf("ZIP file parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Failed to parse ZIP file.\n");
    }

    h_parse_result_free(result);
    h_free_parser(zip_parser);
    free(data);

    return 0;
}