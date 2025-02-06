#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <hammer/hammer.h>

// Define structures for SQLite file format elements
typedef struct {
    uint8_t magic[16];
    uint32_t pageSize;
    uint32_t writeVersion;
    uint32_t readOnly;
    uint32_t reservedSpace;
    uint32_t maxPageCount;
    uint32_t changeCount;
    uint32_t format;
    uint32_t textEncoding;
    uint32_t userVersion;
    uint32_t unused1;
    uint32_t unused2;
    uint32_t sqlVersion;
    uint32_t schemaVersion;
    uint32_t integrityCheck;
} sqlite3_header;

typedef struct {
    uint8_t pageType;
    uint32_t pageNumber;
    uint16_t freeBytes;
    uint16_t cellPointer;
    uint16_t cellCount;
    uint32_t rightmostChild;
} sqlite3_page_header;

// Helper function to convert a byte array to a string (for magic number)
char* byteArrayToString(const uint8_t* arr, size_t len) {
    char* str = (char*)malloc(len + 1);
    if (str == NULL) return NULL;
    memcpy(str, arr, len);
    str[len] = '\0';
    return str;
}

//Free the string allocated by byteArrayToString
void free_byteArrayToString(void* ptr){
    free(ptr);
}

// Hammer parser combinators for SQLite file format
static hm_parser sqlite3_header_parser() {
    return hm_map(
        hm_seq(
            hm_bytes(16, (hm_pred_f)byteArrayToString, (hm_free_f)free_byteArrayToString), 
            hm_uint32,
            hm_uint32,
            hm_uint32,
            hm_uint32,
            hm_uint32,
            hm_uint32,
            hm_uint32,
            hm_uint32,
            hm_uint32,
            hm_uint32,
            hm_uint32,
            hm_uint32,
            hm_uint32,
            hm_uint32
        ),
        (hm_map_f)sqlite3_header
    );
}

static hm_parser sqlite3_page_header_parser() {
    return hm_map(
        hm_seq(
            hm_uint8,
            hm_uint32,
            hm_uint16,
            hm_uint16,
            hm_uint16,
            hm_uint32
        ),
        (hm_map_f)sqlite3_page_header
    );
}


int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <sqlite3_database_file>\n", argv[0]);
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

    uint8_t *buffer = (uint8_t *)malloc(fileSize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fileSize, fp);
    fclose(fp);

    hm_input input = hm_input_new(buffer, fileSize);
    hm_result result = hm_parse(sqlite3_header_parser(), input);

    if (result.status == HM_SUCCESS) {
        sqlite3_header *header = (sqlite3_header *)result.value;
        printf("SQLite Header:\n");
        printf("  pageSize: %u\n", header->pageSize);
        // Print other header fields...
        free(header);
    } else {
        fprintf(stderr, "Error parsing SQLite header: %s\n", result.error);
    }

    free(buffer);
    return 0;
}

