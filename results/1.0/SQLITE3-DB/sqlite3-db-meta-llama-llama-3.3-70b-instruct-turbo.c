#include <hammer/hammer.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define page_size_min 512
#define page_size_max 65536

typedef enum {
    zero,
    one,
    two
} file_format_version;

typedef enum {
    zero_bytes,
    sixty_four_bytes,
    one_hundred_twenty_eight_bytes,
    two_hundred_fifty_six_bytes,
    five_hundred_twelve_bytes,
    one_kilobyte,
    two_kilobytes,
    four_kilobytes,
    eight_kilobytes,
    sixteen_kilobytes,
    thirty_two_kilobytes,
    sixty_four_kilobytes
} payload_fraction;

typedef enum {
    minsize,
    minsizemask,
    leftchild,
    rightchild,
    frag
} cell_mask;

typedef enum {
    table_leaf,
    table_interior,
    index_leaf,
    index_interior
} page_type;

typedef struct {
    uint8_t magic[16];
    uint16_t page_size;
    uint8_t file_format_write_version;
    uint8_t file_format_read_version;
    uint8_t reserved;
    uint8_t payload_fraction_max_embed;
    uint8_t payload_fraction_min_page;
    uint32_t leaf_pointer_map_lrealm;
    uint32_t file_change_counter;
    uint32_t database_size_pages;
    uint32_t first_freelist_trunk_page;
    uint32_t total_freelist_pages;
    uint32_t schema_cookie;
    uint32_t schema_format_number;
    uint32_t default_page_cache_size;
    uint32_t legacy_file_format;
    uint32_t legacy_page_cache_size;
    uint32_t version_valid_number;
    uint32_t sqlite_version_number;
    uint32_t application_id;
} sqlite_database_header;

typedef struct {
    uint8_t page_type;
    uint16_t first_freeblock;
    uint16_t cell_count;
    uint16_t cell_start_offset;
    uint16_t cell_start_size;
    uint16_t free_fragmented_bytes;
    uint16_t right_child_page_number;
    uint32_t page_number;
} btree_page_header;

typedef struct {
    uint32_t right_child_page_number;
    uint32_t page_number;
} pointer_map_page_header;

typedef struct {
    uint16_t payload_length;
    uint16_t row_id;
} cell_header;

typedef struct {
    cell_header header;
    uint8_t* payload;
} cell;

typedef struct {
    uint16_t start_of_content_area;
    uint16_t fragment_bytes;
} page_footer;

typedef struct {
    page_footer footer;
    cell* cells;
} btree_page;

typedef struct {
    pointer_map_page_header header;
} pointer_map_page;

void* sqlite_database_header_rule() {
    void* rule = hammer_sequence(
        hammer_literal((uint8_t*)"SQLite format 3\0", 16),
        hammer_concat(
            hammer_uint16le(),
            hammer_choice(
                hammer_literal((uint8_t*)"\x01", 1),
                hammer_literal((uint8_t*)"\x02", 1)
            ),
            hammer_literal((uint8_t*)"\x00", 1),
            hammer_uint8(),
            hammer_uint8(),
            hammer_uint32le(),
            hammer_uint32le(),
            hammer_uint32le(),
            hammer_uint32le(),
            hammer_uint32le(),
            hammer_uint32le(),
            hammer_uint32le(),
            hammer_uint32le(),
            hammer_uint32le(),
            hammer_uint32le(),
            hammer_uint32le(),
            hammer_uint32le()
        )
    );
    return rule;
}

void* btree_page_header_rule() {
    void* rule = hammer_sequence(
        hammer_uint8(),
        hammer_uint16le(),
        hammer_uint16le(),
        hammer_uint16le(),
        hammer_uint16le(),
        hammer_uint16le(),
        hammer_uint16le(),
        hammer_uint32le()
    );
    return rule;
}

void* pointer_map_page_header_rule() {
    void* rule = hammer_sequence(
        hammer_uint32le(),
        hammer_uint32le()
    );
    return rule;
}

void* cell_header_rule() {
    void* rule = hammer_sequence(
        hammer_uint16le(),
        hammer_uint16le()
    );
    return rule;
}

void* cell_rule() {
    void* rule = hammer_sequence(
        cell_header_rule(),
        hammer_bytes_infinite()
    );
    return rule;
}

void* page_footer_rule() {
    void* rule = hammer_sequence(
        hammer_uint16le(),
        hammer_uint16le()
    );
    return rule;
}

void* btree_page_rule() {
    void* rule = hammer_sequence(
        btree_page_header_rule(),
        hammer_repeated(cell_rule()),
        page_footer_rule()
    );
    return rule;
}

void* pointer_map_page_rule() {
    void* rule = hammer_sequence(
        pointer_map_page_header_rule(),
        hammer_repeated(hammer_bytes(5))
    );
    return rule;
}

void* sqlite_page_rule() {
    void* rule = hammer_choice(
        btree_page_rule(),
        pointer_map_page_rule()
    );
    return rule;
}

void* sqlite_file_rule() {
    void* rule = hammer_sequence(
        sqlite_database_header_rule(),
        hammer_repeated(sqlite_page_rule())
    );
    return rule;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    char* filename = argv[1];
    FILE* file = fopen(filename, "rb");
    if (!file) {
        printf("Failed to open file %s\n", filename);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    fread(buffer, file_size, 1, file);
    fclose(file);

    void* result = hammer_parse(sqlite_file_rule(), buffer, file_size);
    if (!result) {
        printf("Failed to parse file %s\n", filename);
        return 1;
    }

    printf("Parsed file %s successfully\n", filename);

    free(buffer);
    return 0;
}