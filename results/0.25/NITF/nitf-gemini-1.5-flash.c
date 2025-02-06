#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>

// Define NITF structures (simplified example - needs expansion for full spec)
typedef struct {
    char version[10];
    char user_id[50];
    // ... other fields ...
} NITF_FileHeader;

typedef struct {
    int32_t num_images;
    int32_t image_width;
    int32_t image_height;
    // ... other fields ...
} NITF_ImageSegmentHeader;

// ... other NITF structures ...


// Hammer parsers (simplified example - needs expansion for full spec)
static hm_parser_t* parse_string(size_t len) {
    return hm_string_n(len);
}

static hm_parser_t* parse_int32_be() {
    return hm_int32_be(); 
}

static hm_parser_t* parse_NITF_FileHeader() {
    return hm_sequence(
        hm_map(parse_string, sizeof(((NITF_FileHeader*)0)->version)),
        hm_map(parse_string, sizeof(((NITF_FileHeader*)0)->user_id)),
        // ... other parsers ...
        NULL
    );
}

static hm_parser_t* parse_NITF_ImageSegmentHeader() {
    return hm_sequence(
        hm_map(parse_int32_be, &((NITF_ImageSegmentHeader*)0)->num_images),
        hm_map(parse_int32_be, &((NITF_ImageSegmentHeader*)0)->image_width),
        hm_map(parse_int32_be, &((NITF_ImageSegmentHeader*)0)->image_height),
        // ... other parsers ...
        NULL
    );
}

// ... other Hammer parsers ...


int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <nitf_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char* fileContent = (char*)malloc(fileSize);
    fread(fileContent, 1, fileSize, fp);
    fclose(fp);

    hm_parser_t* nitf_parser = hm_sequence(
        parse_NITF_FileHeader(),
        parse_NITF_ImageSegmentHeader(),
        // ... other parsers ...
        NULL
    );

    hm_result_t result = hm_parse(nitf_parser, fileContent, fileSize);

    if (result.success) {
        // Access parsed data
        NITF_FileHeader* fileHeader = (NITF_FileHeader*)result.value;
        NITF_ImageSegmentHeader* imageHeader = (NITF_ImageSegmentHeader*)((char*)result.value + sizeof(NITF_FileHeader));
        printf("File Header Version: %s\n", fileHeader->version);
        printf("File Header User ID: %s\n", fileHeader->user_id);
        printf("Image Segment Header Num Images: %d\n", imageHeader->num_images);
        // ... access other fields ...
        hm_free(result.value);
    } else {
        fprintf(stderr, "Parsing failed at offset %zu: %s\n", result.offset, result.error);
    }

    free(fileContent);
    return 0;
}
