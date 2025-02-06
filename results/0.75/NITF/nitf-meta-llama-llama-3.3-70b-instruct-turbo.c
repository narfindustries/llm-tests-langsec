#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define the NITF file header structure
typedef struct {
    char fhver[6];
    char file_title[25];
    char classification[2];
    char classification_text[50];
    char originator[25];
    char originator_contact[50];
    char date_time[14];
    char security_control_information[2];
    char releasing_instructions[50];
    char file_security_metadata[50];
} nitf_file_header_t;

// Define the NITF image segment structure
typedef struct {
    char image_identifier[10];
    char image_compression[4];
    char image_pixel_format[4];
    int image_rows;
    int image_columns;
    char *image_data;
} nitf_image_segment_t;

// Define the NITF graphic segment structure
typedef struct {
    char graphic_identifier[10];
    char graphic_type[4];
    int graphic_x;
    int graphic_y;
} nitf_graphic_segment_t;

// Define the NITF text segment structure
typedef struct {
    char text_identifier[10];
    char text_type[4];
    char *text_content;
} nitf_text_segment_t;

// Define the NITF data extension segment structure
typedef struct {
    char data_extension_identifier[10];
    char data_extension_type[4];
    char *data_extension_content;
} nitf_data_extension_segment_t;

// Define the NITF file structure
typedef struct {
    nitf_file_header_t file_header;
    nitf_image_segment_t image_segment;
    nitf_graphic_segment_t *graphic_segment;
    nitf_text_segment_t *text_segment;
    nitf_data_extension_segment_t *data_extension_segment;
} nitf_file_t;

// Define the parser for the NITF file header
void nitf_file_header_parser(HParser *parser) {
    h_bytes(parser, "NITF", 4);
    h_bytes(parser, "02.10", 6);
    h_bytes(parser, "", 25);
    h_bytes(parser, "", 2);
    h_bytes(parser, "", 50);
    h_bytes(parser, "", 25);
    h_bytes(parser, "", 50);
    h_bytes(parser, "", 14);
    h_bytes(parser, "", 2);
    h_bytes(parser, "", 50);
    h_bytes(parser, "", 50);
}

// Define the parser for the NITF image segment
void nitf_image_segment_parser(HParser *parser) {
    h_bytes(parser, "", 10);
    h_bytes(parser, "", 4);
    h_bytes(parser, "", 4);
    h_int32(parser);
    h_int32(parser);
    h_bytes(parser, "", 0);
}

// Define the parser for the NITF graphic segment
void nitf_graphic_segment_parser(HParser *parser) {
    h_bytes(parser, "", 10);
    h_bytes(parser, "", 4);
    h_int32(parser);
    h_int32(parser);
}

// Define the parser for the NITF text segment
void nitf_text_segment_parser(HParser *parser) {
    h_bytes(parser, "", 10);
    h_bytes(parser, "", 4);
    h_bytes(parser, "", 0);
}

// Define the parser for the NITF data extension segment
void nitf_data_extension_segment_parser(HParser *parser) {
    h_bytes(parser, "", 10);
    h_bytes(parser, "", 4);
    h_bytes(parser, "", 0);
}

// Define the parser for the NITF file
void nitf_file_parser(HParser *parser) {
    HParser *sequence = h_sequence_init();
    h_sequence_add(sequence, nitf_file_header_parser);
    h_sequence_add(sequence, nitf_image_segment_parser);
    h_sequence_add(sequence, h_optional(nitf_graphic_segment_parser));
    h_sequence_add(sequence, h_optional(nitf_text_segment_parser));
    h_sequence_add(sequence, h_optional(nitf_data_extension_segment_parser));
    h_add(parser, sequence);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file %s\n", argv[1]);
        return 1;
    }

    HParser *parser = h_parser_init();
    nitf_file_parser(parser);
    HParserResult *result = h_parse_file(parser, file);
    if (!result) {
        printf("Error parsing file %s\n", argv[1]);
        return 1;
    }

    nitf_file_t *nitf_file = (nitf_file_t *)result->data;

    printf("File Header:\n");
    printf("  Version: %s\n", nitf_file->file_header.fhver);
    printf("  Title: %s\n", nitf_file->file_header.file_title);
    printf("  Classification: %s\n", nitf_file->file_header.classification);
    printf("  Classification Text: %s\n", nitf_file->file_header.classification_text);
    printf("  Originator: %s\n", nitf_file->file_header.originator);
    printf("  Originator Contact: %s\n", nitf_file->file_header.originator_contact);
    printf("  Date and Time: %s\n", nitf_file->file_header.date_time);
    printf("  Security Control Information: %s\n", nitf_file->file_header.security_control_information);
    printf("  Releasing Instructions: %s\n", nitf_file->file_header.releasing_instructions);
    printf("  File Security Metadata: %s\n", nitf_file->file_header.file_security_metadata);

    printf("Image Segment:\n");
    printf("  Identifier: %s\n", nitf_file->image_segment.image_identifier);
    printf("  Compression: %s\n", nitf_file->image_segment.image_compression);
    printf("  Pixel Format: %s\n", nitf_file->image_segment.image_pixel_format);
    printf("  Rows: %d\n", nitf_file->image_segment.image_rows);
    printf("  Columns: %d\n", nitf_file->image_segment.image_columns);
    printf("  Data: %s\n", nitf_file->image_segment.image_data);

    if (nitf_file->graphic_segment) {
        printf("Graphic Segment:\n");
        printf("  Identifier: %s\n", nitf_file->graphic_segment->graphic_identifier);
        printf("  Type: %s\n", nitf_file->graphic_segment->graphic_type);
        printf("  X: %d\n", nitf_file->graphic_segment->graphic_x);
        printf("  Y: %d\n", nitf_file->graphic_segment->graphic_y);
    }

    if (nitf_file->text_segment) {
        printf("Text Segment:\n");
        printf("  Identifier: %s\n", nitf_file->text_segment->text_identifier);
        printf("  Type: %s\n", nitf_file->text_segment->text_type);
        printf("  Content: %s\n", nitf_file->text_segment->text_content);
    }

    if (nitf_file->data_extension_segment) {
        printf("Data Extension Segment:\n");
        printf("  Identifier: %s\n", nitf_file->data_extension_segment->data_extension_identifier);
        printf("  Type: %s\n", nitf_file->data_extension_segment->data_extension_type);
        printf("  Content: %s\n", nitf_file->data_extension_segment->data_extension_content);
    }

    return 0;
}