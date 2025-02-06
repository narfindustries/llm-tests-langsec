#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define PAGE_SIZE_MIN 512
#define PAGE_SIZE_MAX 65536

typedef struct {
    char magic[16];
    uint16_t page_size;
    uint8_t file_format_write_version;
    uint8_t file_format_read_version;
    uint8_t reserved_space;
    uint8_t max_embedded_payload_fraction;
    uint8_t min_embedded_payload_fraction;
    uint8_t leaf_payload_fraction;
    uint32_t file_change_counter;
    uint32_t size_of_db_file_in_pages;
    uint32_t first_freelist_trunk_page;
    uint32_t number_of_freelist_pages;
    uint32_t schema_cookie;
    uint32_t schema_format_number;
    uint32_t default_page_cache_size;
    uint32_t largest_root_btree_page;
    uint32_t database_text_encoding;
    uint32_t user_version;
    uint32_t incremental_vacuum_mode;
    uint32_t application_id;
    uint8_t reserved_for_expansion[20];
} sqlite_header_t;

typedef enum {
    PAGE_TYPE_LEAF_INDEX = 0,
    PAGE_TYPE_INTERNAL_INDEX = 1,
    PAGE_TYPE_LEAF_TABLE = 2,
    PAGE_TYPE_INTERNAL_TABLE = 5,
    PAGE_TYPE_FREELIST_TRUNK = 10
} page_type_t;

typedef struct {
    page_type_t page_type;
    uint16_t first_freeblock;
    uint16_t cell_content_area;
    uint16_t number_of_cells;
} page_header_t;

typedef struct {
    uint32_t payload_length;
    uint8_t* payload;
    uint64_t rowid;
    uint32_t overflow_page;
} btree_cell_t;

typedef struct {
    uint32_t page_number;
    uint32_t right_child_page;
    btree_cell_t* cell_array;
    uint16_t number_of_cells;
} btree_page_t;

typedef struct {
    uint32_t trunk_page_number;
    uint16_t number_of_entries;
    uint32_t* freelist_entry_array;
} freelist_trunk_page_t;

typedef struct {
    int status;
    size_t offset;
} hammer_result_t;

hammer_result_t hammer_parse(void* parser, const uint8_t* data, size_t size);

int hammer_str(const char* str, const uint8_t** data, size_t* size) {
    size_t len = strlen(str);
    if (*size < len) return -1;
    memcpy((void*)*data, str, len);
    *size -= len;
    *data += len;
    return 0;
}

int hammer_uint8(const uint8_t** data, size_t* size, uint8_t* value) {
    if (*size < 1) return -1;
    *value = **data;
    (*data)++;
    (*size)--;
    return 0;
}

int hammer_uint16_be(const uint8_t** data, size_t* size, uint16_t* value) {
    if (*size < 2) return -1;
    *value = ((*data)[0] << 8) | (*data)[1];
    *data += 2;
    *size -= 2;
    return 0;
}

int hammer_uint32_be(const uint8_t** data, size_t* size, uint32_t* value) {
    if (*size < 4) return -1;
    *value = ((*data)[0] << 24) | ((*data)[1] << 16) | ((*data)[2] << 8) | (*data)[3];
    *data += 4;
    *size -= 4;
    return 0;
}

int hammer_bytes(const uint8_t** data, size_t* size, size_t length, uint8_t** value) {
    if (*size < length) return -1;
    *value = malloc(length);
    memcpy(*value, *data, length);
    *data += length;
    *size -= length;
    return 0;
}

int hammer_varint(const uint8_t** data, size_t* size, uint64_t* value) {
    uint64_t result = 0;
    int shift = 0;
    while (1) {
        if (*size < 1) return -1;
        uint8_t byte = **data;
        *data += 1;
        *size -= 1;
        result |= (uint64_t)(byte & 0x7F) << shift;
        if ((byte & 0x80) == 0) break;
        shift += 7;
    }
    *value = result;
    return 0;
}

typedef int (*hammer_parser_t)(const uint8_t**, size_t*, void*);

int hammer_seq(const uint8_t** data, size_t* size, hammer_parser_t* parsers, void** values, size_t length) {
    for (size_t i = 0; i < length; i++) {
        if (parsers[i](data, size, values[i]) != 0) return -1;
    }
    return 0;
}

int sqlite_header_parser(const uint8_t** data, size_t* size, sqlite_header_t* header) {
    hammer_parser_t parsers[] = {
        (hammer_parser_t)hammer_str,
        (hammer_parser_t)hammer_uint16_be,
        (hammer_parser_t)hammer_uint8,
        (hammer_parser_t)hammer_uint8,
        (hammer_parser_t)hammer_uint8,
        (hammer_parser_t)hammer_uint8,
        (hammer_parser_t)hammer_uint8,
        (hammer_parser_t)hammer_uint8,
        (hammer_parser_t)hammer_uint32_be,
        (hammer_parser_t)hammer_uint32_be,
        (hammer_parser_t)hammer_uint32_be,
        (hammer_parser_t)hammer_uint32_be,
        (hammer_parser_t)hammer_uint32_be,
        (hammer_parser_t)hammer_uint32_be,
        (hammer_parser_t)hammer_uint32_be,
        (hammer_parser_t)hammer_uint32_be,
        (hammer_parser_t)hammer_uint32_be,
        (hammer_parser_t)hammer_uint32_be,
        (hammer_parser_t)hammer_uint32_be,
        (hammer_parser_t)hammer_uint32_be,
        (hammer_parser_t)hammer_uint32_be,
        (hammer_parser_t)hammer_bytes
    };
    void* values[] = {
        (void*)header->magic,
        (void*)&header->page_size,
        (void*)&header->file_format_write_version,
        (void*)&header->file_format_read_version,
        (void*)&header->reserved_space,
        (void*)&header->max_embedded_payload_fraction,
        (void*)&header->min_embedded_payload_fraction,
        (void*)&header->leaf_payload_fraction,
        (void*)&header->file_change_counter,
        (void*)&header->size_of_db_file_in_pages,
        (void*)&header->first_freelist_trunk_page,
        (void*)&header->number_of_freelist_pages,
        (void*)&header->schema_cookie,
        (void*)&header->schema_format_number,
        (void*)&header->default_page_cache_size,
        (void*)&header->largest_root_btree_page,
        (void*)&header->database_text_encoding,
        (void*)&header->user_version,
        (void*)&header->incremental_vacuum_mode,
        (void*)&header->application_id,
        (void*)header->reserved_for_expansion
    };
    return hammer_seq(data, size, parsers, values, sizeof(parsers)/sizeof(parsers[0]));
}

int page_header_parser(const uint8_t** data, size_t* size, page_header_t* header) {
    hammer_parser_t parsers[] = {
        (hammer_parser_t)hammer_uint8,
        (hammer_parser_t)hammer_uint16_be,
        (hammer_parser_t)hammer_uint16_be,
        (hammer_parser_t)hammer_uint16_be
    };
    void* values[] = {
        (void*)&header->page_type,
        (void*)&header->first_freeblock,
        (void*)&header->cell_content_area,
        (void*)&header->number_of_cells
    };
    return hammer_seq(data, size, parsers, values, sizeof(parsers)/sizeof(parsers[0]));
}

int btree_cell_parser(const uint8_t** data, size_t* size, btree_cell_t* cell) {
    hammer_parser_t parsers[] = {
        (hammer_parser_t)hammer_varint,
        (hammer_parser_t)hammer_bytes,
        (hammer_parser_t)hammer_varint,
        (hammer_parser_t)hammer_uint32_be
    };
    void* values[] = {
        (void*)&cell->payload_length,
        (void*)&cell->payload,
        (void*)&cell->rowid,
        (void*)&cell->overflow_page
    };
    return hammer_seq(data, size, parsers, values, sizeof(parsers)/sizeof(parsers[0]));
}

int btree_page_parser(const uint8_t** data, size_t* size, btree_page_t* page) {
    hammer_parser_t parsers[] = {
        (hammer_parser_t)hammer_uint32_be,
        (hammer_parser_t)hammer_uint32_be,
        (hammer_parser_t)hammer_bytes,
        (hammer_parser_t)hammer_uint16_be
    };
    void* values[] = {
        (void*)&page->page_number,
        (void*)&page->right_child_page,
        (void*)&page->cell_array,
        (void*)&page->number_of_cells
    };
    return hammer_seq(data, size, parsers, values, sizeof(parsers)/sizeof(parsers[0]));
}

int freelist_trunk_page_parser(const uint8_t** data, size_t* size, freelist_trunk_page_t* page) {
    hammer_parser_t parsers[] = {
        (hammer_parser_t)hammer_uint32_be,
        (hammer_parser_t)hammer_uint16_be,
        (hammer_parser_t)hammer_bytes
    };
    void* values[] = {
        (void*)&page->trunk_page_number,
        (void*)&page->number_of_entries,
        (void*)&page->freelist_entry_array
    };
    return hammer_seq(data, size, parsers, values, sizeof(parsers)/sizeof(parsers[0]));
}

int sqlite_parser(const uint8_t** data, size_t* size, void* result) {
    hammer_parser_t parsers[] = {
        (hammer_parser_t)sqlite_header_parser,
        (hammer_parser_t)page_header_parser,
        (hammer_parser_t)btree_page_parser,
        (hammer_parser_t)freelist_trunk_page_parser
    };
    void* values[] = {
        (void*)result,
        (void*)result,
        (void*)result,
        (void*)result
    };
    return hammer_seq(data, size, parsers, values, sizeof(parsers)/sizeof(parsers[0]));
}

hammer_result_t hammer_parse(void* parser, const uint8_t* data, size_t size) {
    hammer_result_t result;
    result.status = 0;
    result.offset = 0;
    const uint8_t* data_ptr = data;
    size_t size_ptr = size;
    if (sqlite_parser(&data_ptr, &size_ptr, parser) != 0) {
        result.status = -1;
        result.offset = data_ptr - data;
    }
    return result;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    rewind(file);

    uint8_t* data = malloc(file_size);
    if (!data) {
        printf("Error allocating memory\n");
        return 1;
    }

    size_t read_size = fread(data, 1, file_size, file);
    if (read_size != file_size) {
        printf("Error reading file\n");
        return 1;
    }

    fclose(file);

    sqlite_header_t header;
    hammer_result_t result = hammer_parse(&header, data, file_size);

    if (result.status == 0) {
        printf("Parse successful\n");
    } else {
        printf("Parse failed at offset %zu\n", result.offset);
    }

    free(data);
    return 0;
}