#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char magic[16];
    uint16_t page_size;
    uint8_t write_version;
    uint8_t read_version;
    uint8_t reserved;
    uint8_t max_embedded_payload_fraction;
    uint8_t min_embedded_payload_fraction;
    uint8_t leaf_payload_fraction;
    uint32_t file_change_counter;
} file_header_t;

typedef struct {
    uint8_t page_type;
    uint16_t first_freeblock;
    uint16_t cell_offset;
    uint16_t num_cells;
    uint8_t fragmented_free_bytes;
    uint32_t right_child;
} page_header_t;

typedef struct {
    uint8_t record_format;
    uint8_t num_columns;
    uint8_t* column_types;
    uint8_t** column_data;
} cell_t;

typedef struct {
    uint32_t next_overflow;
} overflow_page_t;

typedef struct {
    uint32_t child_page;
    uint8_t* key;
} btree_page_t;

parser(file_header_p) {
    char magic[16];
    h_bytes(magic, 16);
    uint16_t page_size = h_uint16();
    uint8_t write_version = h_uint8();
    uint8_t read_version = h_uint8();
    uint8_t reserved = h_uint8();
    uint8_t max_embedded_payload_fraction = h_uint8();
    uint8_t min_embedded_payload_fraction = h_uint8();
    uint8_t leaf_payload_fraction = h_uint8();
    uint32_t file_change_counter = h_uint32();

    file_header_t* header = malloc(sizeof(file_header_t));
    memcpy(header->magic, magic, 16);
    header->page_size = page_size;
    header->write_version = write_version;
    header->read_version = read_version;
    header->reserved = reserved;
    header->max_embedded_payload_fraction = max_embedded_payload_fraction;
    header->min_embedded_payload_fraction = min_embedded_payload_fraction;
    header->leaf_payload_fraction = leaf_payload_fraction;
    header->file_change_counter = file_change_counter;

    return header;
}

parser(page_header_p) {
    uint8_t page_type = h_uint8();
    uint16_t first_freeblock = h_uint16();
    uint16_t cell_offset = h_uint16();
    uint16_t num_cells = h_uint16();
    uint8_t fragmented_free_bytes = h_uint8();
    uint32_t right_child = h_uint32();

    page_header_t* header = malloc(sizeof(page_header_t));
    header->page_type = page_type;
    header->first_freeblock = first_freeblock;
    header->cell_offset = cell_offset;
    header->num_cells = num_cells;
    header->fragmented_free_bytes = fragmented_free_bytes;
    header->right_child = right_child;

    return header;
}

parser(cell_p) {
    uint8_t record_format = h_uint8();
    uint8_t num_columns = h_uint8();
    uint8_t* column_types = malloc(num_columns * sizeof(uint8_t));
    for (int i = 0; i < num_columns; i++) {
        column_types[i] = h_uint8();
    }
    uint8_t** column_data = malloc(num_columns * sizeof(uint8_t*));
    for (int i = 0; i < num_columns; i++) {
        column_data[i] = malloc(h_varint() * sizeof(uint8_t));
        h_bytes(column_data[i], h_varint());
    }

    cell_t* cell = malloc(sizeof(cell_t));
    cell->record_format = record_format;
    cell->num_columns = num_columns;
    cell->column_types = column_types;
    cell->column_data = column_data;

    return cell;
}

parser(overflow_page_p) {
    uint32_t next_overflow = h_uint32();

    overflow_page_t* page = malloc(sizeof(overflow_page_t));
    page->next_overflow = next_overflow;

    return page;
}

parser(btree_page_p) {
    uint32_t child_page = h_uint32();
    uint8_t* key = malloc(h_varint() * sizeof(uint8_t));
    h_bytes(key, h_varint());

    btree_page_t* page = malloc(sizeof(btree_page_t));
    page->child_page = child_page;
    page->key = key;

    return page;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file\n");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t length = ftell(file);
    rewind(file);

    uint8_t* input = malloc(length);
    fread(input, 1, length, file);

    HParser* parser = h_new_parser();
    h_add_parser(parser, &file_header_p);
    h_add_parser(parser, &page_header_p);
    h_add_parser(parser, &cell_p);
    h_add_parser(parser, &overflow_page_p);
    h_add_parser(parser, &btree_page_p);

    HParseResult* result = h_parse(parser, input, length);

    file_header_t* header = (file_header_t*)result;
    printf("File Header:\n");
    printf("  Magic: %s\n", header->magic);
    printf("  Page Size: %d\n", header->page_size);
    printf("  Write Version: %d\n", header->write_version);
    printf("  Read Version: %d\n", header->read_version);
    printf("  Reserved: %d\n", header->reserved);
    printf("  Max Embedded Payload Fraction: %d\n", header->max_embedded_payload_fraction);
    printf("  Min Embedded Payload Fraction: %d\n", header->min_embedded_payload_fraction);
    printf("  Leaf Payload Fraction: %d\n", header->leaf_payload_fraction);
    printf("  File Change Counter: %d\n", header->file_change_counter);

    page_header_t* page_header = (page_header_t*)result;
    printf("Page Header:\n");
    printf("  Page Type: %d\n", page_header->page_type);
    printf("  First Freeblock: %d\n", page_header->first_freeblock);
    printf("  Cell Offset: %d\n", page_header->cell_offset);
    printf("  Num Cells: %d\n", page_header->num_cells);
    printf("  Fragmented Free Bytes: %d\n", page_header->fragmented_free_bytes);
    printf("  Right Child: %d\n", page_header->right_child);

    cell_t* cell = (cell_t*)result;
    printf("Cell:\n");
    printf("  Record Format: %d\n", cell->record_format);
    printf("  Num Columns: %d\n", cell->num_columns);
    printf("  Column Types: ");
    for (int i = 0; i < cell->num_columns; i++) {
        printf("%d ", cell->column_types[i]);
    }
    printf("\n");
    printf("  Column Data: ");
    for (int i = 0; i < cell->num_columns; i++) {
        printf("%s ", cell->column_data[i]);
    }
    printf("\n");

    overflow_page_t* overflow_page = (overflow_page_t*)result;
    printf("Overflow Page:\n");
    printf("  Next Overflow: %d\n", overflow_page->next_overflow);

    btree_page_t* btree_page = (btree_page_t*)result;
    printf("B-Tree Page:\n");
    printf("  Child Page: %d\n", btree_page->child_page);
    printf("  Key: %s\n", btree_page->key);

    fclose(file);
    return 0;
}