#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the NITF header structure
typedef struct {
    uint32_t file_length;
    char header_length[6];
    char version[2];
    char complexity_level[2];
    char system_type[8];
    char origin_station_id[10];
    char file_date_time[14];
    char file_title[80];
    char file_security[50];
    char file_copy_number[5];
    char file_number_of_copies[5];
    char file_classification[1];
    char file_control_number[20];
    char file_release_instructions[2];
    char file_declassification_type[2];
    char file_declassification_date[8];
    char file_declassification_exemption[4];
    char file_downgrade[2];
    char file_downgrade_date[8];
    char file_classification_system[2];
    char file_classification_code[11];
    char file_handling_restrictions[2];
    char file_control_handling[25];
    char file_release_authority[40];
    char file_security_source_date[8];
    char file_security_control_number[15];
} NITFHeader;

// Define the NITF image segment header structure
typedef struct {
    char image_segment_id[10];
    char image_date_time[14];
    char image_title[80];
    char image_security[50];
    char image_encryption[1];
    char image_compression[2];
    char image_band_count[1];
    char image_bands[3];
    char image_pixel_value_type[3];
    char image_abpp[2];
    char image_pixel_justification[1];
    char image_coordinate_system[1];
    char image_geographic_location[60];
    char image_number_of_columns[8];
    char image_number_of_rows[8];
    char image_compression_rate[4];
    char image_compression_date[8];
    char image_compression_algorithm[2];
    char image_compression_control_number[15];
} NITFImageSegmentHeader;

// Define the NITF file structure
typedef struct {
    NITFHeader header;
    NITFImageSegmentHeader image_segment_header;
} NITFFile;

// Define the Hammer parser for the NITF header
HParser *nitf_header_parser() {
    return h_sequence(
        h_bits(32, NULL), // file_length
        h_length_value(h_uint8(), h_bytes(6, NULL)), // header_length
        h_bytes(2, NULL), // version
        h_bytes(2, NULL), // complexity_level
        h_bytes(8, NULL), // system_type
        h_bytes(10, NULL), // origin_station_id
        h_bytes(14, NULL), // file_date_time
        h_bytes(80, NULL), // file_title
        h_bytes(50, NULL), // file_security
        h_bytes(5, NULL), // file_copy_number
        h_bytes(5, NULL), // file_number_of_copies
        h_bytes(1, NULL), // file_classification
        h_bytes(20, NULL), // file_control_number
        h_bytes(2, NULL), // file_release_instructions
        h_bytes(2, NULL), // file_declassification_type
        h_bytes(8, NULL), // file_declassification_date
        h_bytes(4, NULL), // file_declassification_exemption
        h_bytes(2, NULL), // file_downgrade
        h_bytes(8, NULL), // file_downgrade_date
        h_bytes(2, NULL), // file_classification_system
        h_bytes(11, NULL), // file_classification_code
        h_bytes(2, NULL), // file_handling_restrictions
        h_bytes(25, NULL), // file_control_handling
        h_bytes(40, NULL), // file_release_authority
        h_bytes(8, NULL), // file_security_source_date
        h_bytes(15, NULL), // file_security_control_number
        NULL
    );
}

// Define the Hammer parser for the NITF image segment header
HParser *nitf_image_segment_header_parser() {
    return h_sequence(
        h_bytes(10, NULL), // image_segment_id
        h_bytes(14, NULL), // image_date_time
        h_bytes(80, NULL), // image_title
        h_bytes(50, NULL), // image_security
        h_bytes(1, NULL), // image_encryption
        h_bytes(2, NULL), // image_compression
        h_bytes(1, NULL), // image_band_count
        h_bytes(3, NULL), // image_bands
        h_bytes(3, NULL), // image_pixel_value_type
        h_bytes(2, NULL), // image_abpp
        h_bytes(1, NULL), // image_pixel_justification
        h_bytes(1, NULL), // image_coordinate_system
        h_bytes(60, NULL), // image_geographic_location
        h_bytes(8, NULL), // image_number_of_columns
        h_bytes(8, NULL), // image_number_of_rows
        h_bytes(4, NULL), // image_compression_rate
        h_bytes(8, NULL), // image_compression_date
        h_bytes(2, NULL), // image_compression_algorithm
        h_bytes(15, NULL), // image_compression_control_number
        NULL
    );
}

// Define the Hammer parser for the NITF file
HParser *nitf_file_parser() {
    return h_sequence(
        nitf_header_parser(),
        nitf_image_segment_header_parser(),
        NULL
    );
}

int main() {
    // Initialize the Hammer parser
    HParser *parser = nitf_file_parser();

    // Open the NITF file
    FILE *file = fopen("input.nitf", "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    // Read the file into a buffer
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    uint8_t *buffer = (uint8_t *)malloc(file_size);
    fread(buffer, 1, file_size, file);
    fclose(file);

    // Parse the NITF file
    HParseResult *result = h_parse(parser, buffer, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse NITF file\n");
        free(buffer);
        return 1;
    }

    // Access the parsed data
    NITFFile *nitf_file = (NITFFile *)result->ast;

    // Print some parsed data
    printf("File Title: %.*s\n", 80, nitf_file->header.file_title);
    printf("Image Title: %.*s\n", 80, nitf_file->image_segment_header.image_title);

    // Clean up
    h_parse_result_free(result);
    free(buffer);

    return 0;
}