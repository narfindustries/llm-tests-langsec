#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define LOCAL_FILE_HEADER_SIGNATURE 0x04034b50
#define CENTRAL_DIRECTORY_HEADER_SIGNATURE 0x02014b50
#define DATA_DESCRIPTOR_SIGNATURE 0x08074b50
#define END_OF_CENTRAL_DIRECTORY_SIGNATURE 0x06054b50

typedef struct {
    uint32_t signature;
    uint16_t version_needed_to_extract;
    uint16_t general_purpose_bit_flag;
    uint16_t compression_method;
    uint16_t last_mod_time;
    uint16_t last_mod_date;
    uint32_t crc_32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
    uint16_t filename_length;
    uint16_t extra_field_length;
} local_file_header_t;

typedef struct {
    uint32_t signature;
    uint16_t version_made_by;
    uint16_t version_needed_to_extract;
    uint16_t general_purpose_bit_flag;
    uint16_t compression_method;
    uint16_t last_mod_time;
    uint16_t last_mod_date;
    uint32_t crc_32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
    uint16_t filename_length;
    uint16_t extra_field_length;
    uint16_t file_comment_length;
    uint16_t disk_number_start;
    uint16_t internal_attributes;
    uint32_t external_attributes;
    uint32_t local_header_offset;
} central_directory_header_t;

typedef struct {
    uint32_t signature;
    uint32_t crc_32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
} data_descriptor_t;

typedef struct {
    uint32_t signature;
    uint16_t number_of_this_disk;
    uint16_t number_of_the_disk_with_the_start_of_the_central_directory;
    uint16_t total_number_of_entries_in_the_central_directory_on_this_disk;
    uint16_t total_number_of_entries_in_the_central_directory;
    uint32_t size_of_the_central_directory;
    uint32_t offset_of_start_of_central_directory_with_respect_to_the_starting_disk_number;
    uint16_t zipfile_comment_length;
} end_of_central_directory_record_t;

typedef struct {
    int status;
    char* message;
} hammer_result_t;

typedef struct {
    void* parser;
} hammer_parser_t;

hammer_parser_t* local_file_header_parser() {
    hammer_parser_t* parser = malloc(sizeof(hammer_parser_t));
    parser->parser = h_sequence(
        h_uint32(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint32(),
        h_uint32(),
        h_uint32(),
        h_uint16(),
        h_uint16()
    );
    return parser;
}

hammer_parser_t* central_directory_header_parser() {
    hammer_parser_t* parser = malloc(sizeof(hammer_parser_t));
    parser->parser = h_sequence(
        h_uint32(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint32(),
        h_uint32(),
        h_uint32(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint32(),
        h_uint32()
    );
    return parser;
}

hammer_parser_t* data_descriptor_parser() {
    hammer_parser_t* parser = malloc(sizeof(hammer_parser_t));
    parser->parser = h_sequence(
        h_uint32(),
        h_uint32(),
        h_uint32(),
        h_uint32()
    );
    return parser;
}

hammer_parser_t* end_of_central_directory_record_parser() {
    hammer_parser_t* parser = malloc(sizeof(hammer_parser_t));
    parser->parser = h_sequence(
        h_uint32(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint32(),
        h_uint32(),
        h_uint16()
    );
    return parser;
}

hammer_parser_t* hammer_choice(hammer_parser_t* parser1, hammer_parser_t* parser2, hammer_parser_t* parser3, hammer_parser_t* parser4) {
    hammer_parser_t* parser = malloc(sizeof(hammer_parser_t));
    parser->parser = h_choice(parser1->parser, parser2->parser, parser3->parser, parser4->parser);
    return parser;
}

hammer_result_t hammer_parse(hammer_parser_t* parser, const uint8_t* input, size_t length) {
    hammer_result_t result;
    result.status = h_parse(parser->parser, input, length);
    if (result.status != 0) {
        result.message = "Error parsing file";
    } else {
        result.message = "Parsed successfully";
    }
    return result;
}

char* hammer_error_message(int status) {
    return "Error parsing file";
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_length = ftell(file);
    rewind(file);

    uint8_t* input = malloc(file_length);
    fread(input, 1, file_length, file);

    hammer_parser_t* parser = hammer_choice(
        local_file_header_parser(),
        central_directory_header_parser(),
        data_descriptor_parser(),
        end_of_central_directory_record_parser()
    );

    hammer_result_t result = hammer_parse(parser, input, file_length);
    if (result.status == 0) {
        printf("Parsed successfully\n");
    } else {
        printf("Error parsing file: %s\n", hammer_error_message(result.status));
    }

    fclose(file);
    free(input);
    return 0;
}