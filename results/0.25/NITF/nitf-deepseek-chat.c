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
    char system_type[4];
    char origin_station_id[10];
    char file_date_time[14];
    char file_title[80];
    char file_security_classification[1];
    char file_copy_number[5];
    char file_number_of_copies[5];
    char file_classification_system[2];
    char file_declassification_type[2];
    char file_declassification_date[8];
    char file_declassification_exemption[4];
    char file_downgrade[1];
    char file_downgrade_date[8];
    char file_classification_text[43];
    char file_classification_authority_type[1];
    char file_classification_authority[40];
    char file_classification_reason[1];
    char file_security_source_date[8];
    char file_security_control_number[15];
} NITFHeader;

// Define the Hammer parser for the NITF header
HParser *nitf_header_parser() {
    return h_sequence(
        h_bits(32, NULL), // file_length
        h_bits(48, NULL), // header_length
        h_bits(16, NULL), // version
        h_bits(16, NULL), // complexity_level
        h_bits(32, NULL), // system_type
        h_bits(80, NULL), // origin_station_id
        h_bits(112, NULL), // file_date_time
        h_bits(640, NULL), // file_title
        h_bits(8, NULL), // file_security_classification
        h_bits(40, NULL), // file_copy_number
        h_bits(40, NULL), // file_number_of_copies
        h_bits(16, NULL), // file_classification_system
        h_bits(16, NULL), // file_declassification_type
        h_bits(64, NULL), // file_declassification_date
        h_bits(32, NULL), // file_declassification_exemption
        h_bits(8, NULL), // file_downgrade
        h_bits(64, NULL), // file_downgrade_date
        h_bits(344, NULL), // file_classification_text
        h_bits(8, NULL), // file_classification_authority_type
        h_bits(320, NULL), // file_classification_authority
        h_bits(8, NULL), // file_classification_reason
        h_bits(64, NULL), // file_security_source_date
        h_bits(120, NULL), // file_security_control_number
        NULL
    );
}

// Main function to parse the NITF file
int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <nitf_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = nitf_header_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (!result) {
        fprintf(stderr, "Failed to parse NITF file\n");
        free(buffer);
        return 1;
    }

    NITFHeader *header = (NITFHeader *)result->ast;
    printf("File Length: %u\n", header->file_length);
    printf("Header Length: %.6s\n", header->header_length);
    printf("Version: %.2s\n", header->version);
    printf("Complexity Level: %.2s\n", header->complexity_level);
    printf("System Type: %.4s\n", header->system_type);
    printf("Origin Station ID: %.10s\n", header->origin_station_id);
    printf("File Date Time: %.14s\n", header->file_date_time);
    printf("File Title: %.80s\n", header->file_title);
    printf("File Security Classification: %.1s\n", header->file_security_classification);
    printf("File Copy Number: %.5s\n", header->file_copy_number);
    printf("File Number of Copies: %.5s\n", header->file_number_of_copies);
    printf("File Classification System: %.2s\n", header->file_classification_system);
    printf("File Declassification Type: %.2s\n", header->file_declassification_type);
    printf("File Declassification Date: %.8s\n", header->file_declassification_date);
    printf("File Declassification Exemption: %.4s\n", header->file_declassification_exemption);
    printf("File Downgrade: %.1s\n", header->file_downgrade);
    printf("File Downgrade Date: %.8s\n", header->file_downgrade_date);
    printf("File Classification Text: %.43s\n", header->file_classification_text);
    printf("File Classification Authority Type: %.1s\n", header->file_classification_authority_type);
    printf("File Classification Authority: %.40s\n", header->file_classification_authority);
    printf("File Classification Reason: %.1s\n", header->file_classification_reason);
    printf("File Security Source Date: %.8s\n", header->file_security_source_date);
    printf("File Security Control Number: %.15s\n", header->file_security_control_number);

    h_parse_result_free(result);
    free(buffer);

    return 0;
}