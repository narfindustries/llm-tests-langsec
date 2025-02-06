#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <hammer/hammer.h>
#include <hammer/allocator.h>

typedef struct {
    uint8_t magic[16];
    uint16_t page_size;
    uint32_t write_version;
    uint32_t read_version;
    uint32_t reserved_space;
    uint32_t max_page_count;
    uint32_t text_encoding;
} sqlite_header;

typedef struct {
    uint32_t page_number;
    uint8_t page_type;
    uint16_t free_bytes;
    uint16_t checksum;
} sqlite_page_header;

HParser sqlite_header_parser(HAllocator* allocator) {
    return h_seq(allocator,
                 h_string(allocator, "SQLite format 3\000"),
                 h_uint16(allocator),
                 h_uint32(allocator),
                 h_uint32(allocator),
                 h_uint32(allocator),
                 h_uint32(allocator),
                 h_uint32(allocator),
                 h_map(allocator, h_result(allocator), h_alloc(allocator, sizeof(sqlite_header)), (void*)memcpy));
}

HParser sqlite_page_header_parser(HAllocator* allocator) {
    return h_seq(allocator,
                 h_uint32(allocator),
                 h_uint8(allocator),
                 h_uint16(allocator),
                 h_uint16(allocator),
                 h_map(allocator, h_result(allocator), h_alloc(allocator, sizeof(sqlite_page_header)), (void*)memcpy));
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <sqlite_db_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *fileContent = (uint8_t*)malloc(fileSize);
    fread(fileContent, 1, fileSize, fp);
    fclose(fp);

    HAllocator* allocator = h_allocator_new();
    HParseResult result = h_parse(allocator, sqlite_header_parser(allocator), fileContent, fileSize);

    if (result.success) {
        sqlite_header *header = (sqlite_header*)result.value;
        printf("SQLite Header:\n");
        printf("  Page Size: %u\n", header->page_size);
        h_free(allocator, header);
    } else {
        fprintf(stderr, "Error parsing SQLite header: %s\n", result.error);
    }

    h_allocator_free(allocator);
    free(fileContent);
    return 0;
}
