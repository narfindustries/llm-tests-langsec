#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define PAGE_SIZE_MIN 512
#define PAGE_SIZE_MAX 65536

typedef enum {
    PAGETYPE_PURGABLE = 0,
    PAGETYPE_ROOT = 1,
    PAGETYPE_INTERNAL = 2,
    PAGETYPE_LEAF = 3,
    PAGETYPE_OVERFLOW = 4,
    PAGETYPE_FREE = 5
} page_type;

typedef enum {
    CELLTYPE_INTEGER = 0,
    CELLTYPE_FLOAT = 1,
    CELLTYPE_TEXT = 2,
    CELLTYPE_BLOB = 3,
    CELLTYPE_NULL = 4,
    CELLTYPE_RESERVED = 5
} cell_type;

typedef struct {
    uint8_t magic[16];
    uint16_t page_size;
    uint8_t write_version;
    uint8_t read_version;
    uint8_t reserved;
    uint8_t max_embedded_payload_fraction;
    uint8_t min_embedded_payload_fraction;
    uint8_t leaf_payload_fraction;
    uint32_t file_change_counter;
    uint32_t database_size;
    uint32_t reserved_space;
    uint32_t version_valid_for;
    uint32_t version_number;
    uint32_t sqlite_version_number;
} db_header;

typedef struct {
    page_type page_type;
    uint16_t first_freeblock;
    uint16_t cell_count;
    uint16_t fragmented_free_bytes;
    uint32_t right_child;
} page_header;

typedef struct {
    uint64_t payload_length;
    uint64_t header_length;
    uint8_t num_header_bytes;
    cell_type type;
    uint8_t reserved;
    uint8_t deleted;
} cell_header;

typedef struct {
    page_header header;
    cell_header cell_header;
    uint8_t* payload;
} cell;

typedef struct {
    page_header header;
    uint32_t left_child;
    uint8_t* key;
} internal_node_cell;

typedef struct {
    page_header header;
    uint32_t rowid;
    uint8_t* payload;
} leaf_node_cell;

typedef struct {
    uint32_t next_overflow;
    uint8_t* data;
} overflow_page;

typedef struct {
    uint32_t next_free;
    uint8_t* data;
} free_page;

int db_header_parser(const uint8_t* input, size_t* input_size) {
    if (*input_size < sizeof(db_header)) {
        return -1;
    }

    db_header* header = (db_header*)input;
    *input_size -= sizeof(db_header);
    return 0;
}

int page_header_parser(const uint8_t* input, size_t* input_size) {
    if (*input_size < sizeof(page_header)) {
        return -1;
    }

    page_header* header = (page_header*)input;
    *input_size -= sizeof(page_header);
    return 0;
}

int cell_header_parser(const uint8_t* input, size_t* input_size) {
    if (*input_size < sizeof(cell_header)) {
        return -1;
    }

    cell_header* header = (cell_header*)input;
    *input_size -= sizeof(cell_header);
    return 0;
}

int cell_parser(const uint8_t* input, size_t* input_size) {
    if (page_header_parser(input, input_size) != 0) {
        return -1;
    }

    if (cell_header_parser(input, input_size) != 0) {
        return -1;
    }

    return 0;
}

int internal_node_cell_parser(const uint8_t* input, size_t* input_size) {
    if (page_header_parser(input, input_size) != 0) {
        return -1;
    }

    if (*input_size < sizeof(uint32_t)) {
        return -1;
    }

    uint32_t* left_child = (uint32_t*)input;
    *input_size -= sizeof(uint32_t);

    if (*input_size < sizeof(uint8_t*)) {
        return -1;
    }

    uint8_t** key = (uint8_t**)input;
    *input_size -= sizeof(uint8_t*);

    return 0;
}

int leaf_node_cell_parser(const uint8_t* input, size_t* input_size) {
    if (page_header_parser(input, input_size) != 0) {
        return -1;
    }

    if (*input_size < sizeof(uint32_t)) {
        return -1;
    }

    uint32_t* rowid = (uint32_t*)input;
    *input_size -= sizeof(uint32_t);

    if (*input_size < sizeof(uint8_t*)) {
        return -1;
    }

    uint8_t** payload = (uint8_t**)input;
    *input_size -= sizeof(uint8_t*);

    return 0;
}

int overflow_page_parser(const uint8_t* input, size_t* input_size) {
    if (*input_size < sizeof(uint32_t)) {
        return -1;
    }

    uint32_t* next_overflow = (uint32_t*)input;
    *input_size -= sizeof(uint32_t);

    if (*input_size < sizeof(uint8_t*)) {
        return -1;
    }

    uint8_t** data = (uint8_t**)input;
    *input_size -= sizeof(uint8_t*);

    return 0;
}

int free_page_parser(const uint8_t* input, size_t* input_size) {
    if (*input_size < sizeof(uint32_t)) {
        return -1;
    }

    uint32_t* next_free = (uint32_t*)input;
    *input_size -= sizeof(uint32_t);

    if (*input_size < sizeof(uint8_t*)) {
        return -1;
    }

    uint8_t** data = (uint8_t**)input;
    *input_size -= sizeof(uint8_t*);

    return 0;
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

    db_header header;
    if (fread(&header, sizeof(header), 1, file) != 1) {
        printf("Error reading header\n");
        return 1;
    }

    if (header.page_size < PAGE_SIZE_MIN || header.page_size > PAGE_SIZE_MAX) {
        printf("Invalid page size: %u\n", header.page_size);
        return 1;
    }

    size_t input_size;
    uint8_t* input;

    while (!feof(file)) {
        page_header page;
        if (fread(&page, sizeof(page), 1, file) != 1) {
            break;
        }

        input = (uint8_t*)&page;
        input_size = sizeof(page);

        if (page_header_parser(input, &input_size) != 0) {
            printf("Error parsing page header\n");
            return 1;
        }

        printf("Page header parsed successfully\n");

        switch (page.page_type) {
            case PAGETYPE_ROOT:
            case PAGETYPE_INTERNAL:
                {
                    internal_node_cell cell;
                    if (fread(&cell, sizeof(cell), 1, file) != 1) {
                        printf("Error reading internal node cell\n");
                        return 1;
                    }

                    input = (uint8_t*)&cell;
                    input_size = sizeof(cell);

                    if (internal_node_cell_parser(input, &input_size) != 0) {
                        printf("Error parsing internal node cell\n");
                        return 1;
                    }

                    printf("Internal node cell parsed successfully\n");
                }
                break;
            case PAGETYPE_LEAF:
                {
                    leaf_node_cell cell;
                    if (fread(&cell, sizeof(cell), 1, file) != 1) {
                        printf("Error reading leaf node cell\n");
                        return 1;
                    }

                    input = (uint8_t*)&cell;
                    input_size = sizeof(cell);

                    if (leaf_node_cell_parser(input, &input_size) != 0) {
                        printf("Error parsing leaf node cell\n");
                        return 1;
                    }

                    printf("Leaf node cell parsed successfully\n");
                }
                break;
            case PAGETYPE_OVERFLOW:
                {
                    overflow_page page;
                    if (fread(&page, sizeof(page), 1, file) != 1) {
                        printf("Error reading overflow page\n");
                        return 1;
                    }

                    input = (uint8_t*)&page;
                    input_size = sizeof(page);

                    if (overflow_page_parser(input, &input_size) != 0) {
                        printf("Error parsing overflow page\n");
                        return 1;
                    }

                    printf("Overflow page parsed successfully\n");
                }
                break;
            case PAGETYPE_FREE:
                {
                    free_page page;
                    if (fread(&page, sizeof(page), 1, file) != 1) {
                        printf("Error reading free page\n");
                        return 1;
                    }

                    input = (uint8_t*)&page;
                    input_size = sizeof(page);

                    if (free_page_parser(input, &input_size) != 0) {
                        printf("Error parsing free page\n");
                        return 1;
                    }

                    printf("Free page parsed successfully\n");
                }
                break;
            default:
                printf("Unknown page type: %u\n", page.page_type);
                return 1;
        }
    }

    fclose(file);
    return 0;
}