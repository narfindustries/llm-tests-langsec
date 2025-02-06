#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NITF_FILE_HEADER "NITF"
#define NITF_FILE_TRAILER "NIET"

typedef struct {
    char file_header[4];
    char file_format_version[2];
    char system_type[2];
    char file_security_classification;
    char file_title[20];
    char file_security_classification_system[2];
    char file_copy_number[2];
    char file_date_and_time[6];
    char* file_header_extension;
} nitf_file_header_t;

typedef struct {
    char image_identifier[25];
    char image_security_classification;
    char image_security_classification_system[2];
    char image_compression_type[2];
    char image_pixel_type[2];
    int image_pixel_size_x;
    int image_pixel_size_y;
    char* image_data;
} nitf_image_segment_t;

typedef struct {
    char graphics_identifier[25];
    char graphics_security_classification;
    char graphics_security_classification_system[2];
    char graphics_type[2];
    char* graphics_data;
} nitf_graphics_segment_t;

typedef struct {
    char text_identifier[25];
    char text_security_classification;
    char text_security_classification_system[2];
    char* text_data;
} nitf_text_segment_t;

typedef struct {
    nitf_file_header_t file_header;
    nitf_image_segment_t* image_segments;
    int image_segment_count;
    nitf_graphics_segment_t* graphics_segments;
    int graphics_segment_count;
    nitf_text_segment_t* text_segments;
    int text_segment_count;
} nitf_t;

void* nitf_file_header_parser(void* input) {
    nitf_file_header_t* file_header = malloc(sizeof(nitf_file_header_t));
    char* input_ptr = (char*)input;
    if (strncmp(input_ptr, NITF_FILE_HEADER, 4) != 0) {
        free(file_header);
        return NULL;
    }
    input_ptr += 4;
    strncpy(file_header->file_format_version, input_ptr, 2);
    input_ptr += 2;
    strncpy(file_header->system_type, input_ptr, 2);
    input_ptr += 2;
    file_header->file_security_classification = *(char*)input_ptr;
    input_ptr += 1;
    strncpy(file_header->file_title, input_ptr, 20);
    input_ptr += 20;
    strncpy(file_header->file_security_classification_system, input_ptr, 2);
    input_ptr += 2;
    strncpy(file_header->file_copy_number, input_ptr, 2);
    input_ptr += 2;
    strncpy(file_header->file_date_and_time, input_ptr, 6);
    input_ptr += 6;
    file_header->file_header_extension = NULL;
    return file_header;
}

void* nitf_image_segment_parser(void* input) {
    nitf_image_segment_t* image_segment = malloc(sizeof(nitf_image_segment_t));
    char* input_ptr = (char*)input;
    strncpy(image_segment->image_identifier, input_ptr, 25);
    input_ptr += 25;
    image_segment->image_security_classification = *(char*)input_ptr;
    input_ptr += 1;
    strncpy(image_segment->image_security_classification_system, input_ptr, 2);
    input_ptr += 2;
    strncpy(image_segment->image_compression_type, input_ptr, 2);
    input_ptr += 2;
    strncpy(image_segment->image_pixel_type, input_ptr, 2);
    input_ptr += 2;
    image_segment->image_pixel_size_x = *(int*)input_ptr;
    input_ptr += 4;
    image_segment->image_pixel_size_y = *(int*)input_ptr;
    input_ptr += 4;
    image_segment->image_data = NULL;
    return image_segment;
}

void* nitf_graphics_segment_parser(void* input) {
    nitf_graphics_segment_t* graphics_segment = malloc(sizeof(nitf_graphics_segment_t));
    char* input_ptr = (char*)input;
    strncpy(graphics_segment->graphics_identifier, input_ptr, 25);
    input_ptr += 25;
    graphics_segment->graphics_security_classification = *(char*)input_ptr;
    input_ptr += 1;
    strncpy(graphics_segment->graphics_security_classification_system, input_ptr, 2);
    input_ptr += 2;
    strncpy(graphics_segment->graphics_type, input_ptr, 2);
    input_ptr += 2;
    graphics_segment->graphics_data = NULL;
    return graphics_segment;
}

void* nitf_text_segment_parser(void* input) {
    nitf_text_segment_t* text_segment = malloc(sizeof(nitf_text_segment_t));
    char* input_ptr = (char*)input;
    strncpy(text_segment->text_identifier, input_ptr, 25);
    input_ptr += 25;
    text_segment->text_security_classification = *(char*)input_ptr;
    input_ptr += 1;
    strncpy(text_segment->text_security_classification_system, input_ptr, 2);
    input_ptr += 2;
    text_segment->text_data = NULL;
    return text_segment;
}

void* nitf_parser(void* input) {
    nitf_t* nitf = malloc(sizeof(nitf_t));
    nitf->file_header = *(nitf_file_header_t*)nitf_file_header_parser(input);
    char* input_ptr = (char*)input;
    input_ptr += sizeof(nitf_file_header_t);
    nitf->image_segments = NULL;
    nitf->image_segment_count = 0;
    nitf->graphics_segments = NULL;
    nitf->graphics_segment_count = 0;
    nitf->text_segments = NULL;
    nitf->text_segment_count = 0;
    while (1) {
        if (strncmp(input_ptr, NITF_FILE_TRAILER, 4) == 0) {
            break;
        }
        if (strncmp(input_ptr, "IMAGE", 5) == 0) {
            nitf_image_segment_t* image_segment = nitf_image_segment_parser(input_ptr + 5);
            nitf->image_segments = realloc(nitf->image_segments, (nitf->image_segment_count + 1) * sizeof(nitf_image_segment_t));
            nitf->image_segments[nitf->image_segment_count++] = *image_segment;
            free(image_segment);
            input_ptr += sizeof(nitf_image_segment_t) + 5;
        } else if (strncmp(input_ptr, "GRAPHICS", 8) == 0) {
            nitf_graphics_segment_t* graphics_segment = nitf_graphics_segment_parser(input_ptr + 8);
            nitf->graphics_segments = realloc(nitf->graphics_segments, (nitf->graphics_segment_count + 1) * sizeof(nitf_graphics_segment_t));
            nitf->graphics_segments[nitf->graphics_segment_count++] = *graphics_segment;
            free(graphics_segment);
            input_ptr += sizeof(nitf_graphics_segment_t) + 8;
        } else if (strncmp(input_ptr, "TEXT", 4) == 0) {
            nitf_text_segment_t* text_segment = nitf_text_segment_parser(input_ptr + 4);
            nitf->text_segments = realloc(nitf->text_segments, (nitf->text_segment_count + 1) * sizeof(nitf_text_segment_t));
            nitf->text_segments[nitf->text_segment_count++] = *text_segment;
            free(text_segment);
            input_ptr += sizeof(nitf_text_segment_t) + 4;
        }
    }
    return nitf;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* input_file = fopen(argv[1], "rb");
    if (!input_file) {
        printf("Error opening input file\n");
        return 1;
    }

    fseek(input_file, 0, SEEK_END);
    long file_size = ftell(input_file);
    rewind(input_file);

    char* input_data = malloc(file_size);
    if (!input_data) {
        printf("Error allocating memory\n");
        fclose(input_file);
        return 1;
    }

    fread(input_data, 1, file_size, input_file);
    fclose(input_file);

    nitf_t* nitf = nitf_parser(input_data);

    printf("File Header: %s\n", nitf->file_header.file_header);
    printf("File Format Version: %s\n", nitf->file_header.file_format_version);
    printf("System Type: %s\n", nitf->file_header.system_type);
    printf("File Security Classification: %c\n", nitf->file_header.file_security_classification);
    printf("File Title: %s\n", nitf->file_header.file_title);
    printf("File Security Classification System: %s\n", nitf->file_header.file_security_classification_system);
    printf("File Copy Number: %s\n", nitf->file_header.file_copy_number);
    printf("File Date and Time: %s\n", nitf->file_header.file_date_and_time);

    for (int i = 0; i < nitf->image_segment_count; i++) {
        nitf_image_segment_t* image_segment = &nitf->image_segments[i];
        printf("Image Identifier: %s\n", image_segment->image_identifier);
        printf("Image Security Classification: %c\n", image_segment->image_security_classification);
        printf("Image Security Classification System: %s\n", image_segment->image_security_classification_system);
        printf("Image Compression Type: %s\n", image_segment->image_compression_type);
        printf("Image Pixel Type: %s\n", image_segment->image_pixel_type);
        printf("Image Pixel Size X: %d\n", image_segment->image_pixel_size_x);
        printf("Image Pixel Size Y: %d\n", image_segment->image_pixel_size_y);
    }

    for (int i = 0; i < nitf->graphics_segment_count; i++) {
        nitf_graphics_segment_t* graphics_segment = &nitf->graphics_segments[i];
        printf("Graphics Identifier: %s\n", graphics_segment->graphics_identifier);
        printf("Graphics Security Classification: %c\n", graphics_segment->graphics_security_classification);
        printf("Graphics Security Classification System: %s\n", graphics_segment->graphics_security_classification_system);
        printf("Graphics Type: %s\n", graphics_segment->graphics_type);
    }

    for (int i = 0; i < nitf->text_segment_count; i++) {
        nitf_text_segment_t* text_segment = &nitf->text_segments[i];
        printf("Text Identifier: %s\n", text_segment->text_identifier);
        printf("Text Security Classification: %c\n", text_segment->text_security_classification);
        printf("Text Security Classification System: %s\n", text_segment->text_security_classification_system);
    }

    free(input_data);
    free(nitf->image_segments);
    free(nitf->graphics_segments);
    free(nitf->text_segments);
    free(nitf);
    return 0;
}