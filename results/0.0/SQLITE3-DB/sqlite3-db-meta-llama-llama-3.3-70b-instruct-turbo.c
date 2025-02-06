#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define PAGE_SIZE 4096

typedef struct {
    uint32_t magic;
    uint8_t file_format_write_version;
    uint8_t file_format_read_version;
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
} page_header_t;

typedef struct {
    uint16_t payload_length;
    uint8_t header_length;
    uint8_t num_columns;
    uint8_t* column_data;
} cell_t;

typedef struct {
    uint32_t page_number;
    uint32_t database_page;
    uint32_t checksum;
} master_journal_page_t;

typedef struct {
    uint32_t magic;
    uint32_t version;
    uint32_t master_journal_page_count;
    uint32_t master_journal_page;
    uint32_t master_journal_checksum;
} master_journal_header_t;

typedef struct {
    uint32_t magic;
    uint32_t file_format_version;
    uint32_t page_size;
    uint32_t checkpoint_sequence;
    uint32_t salt1;
    uint32_t salt2;
    uint32_t checksum_initial_value;
} wal_header_t;

typedef struct {
    uint32_t page_number;
    uint8_t before_image;
    uint8_t number_of_pages;
    uint32_t salt;
    uint32_t checksum;
} wal_frame_header_t;

typedef struct {
    uint8_t* data;
    size_t size;
    size_t pos;
} hammer_context_t;

void hammer_init(hammer_context_t* ctx, uint8_t* data, size_t size) {
    ctx->data = data;
    ctx->size = size;
    ctx->pos = 0;
}

void hammer_uint32_be(hammer_context_t* ctx, uint32_t* value) {
    if (ctx->pos + 4 > ctx->size) {
        printf("Error: end of data reached\n");
        exit(1);
    }
    *value = (ctx->data[ctx->pos] << 24) | (ctx->data[ctx->pos + 1] << 16) | (ctx->data[ctx->pos + 2] << 8) | ctx->data[ctx->pos + 3];
    ctx->pos += 4;
}

void hammer_uint16_be(hammer_context_t* ctx, uint16_t* value) {
    if (ctx->pos + 2 > ctx->size) {
        printf("Error: end of data reached\n");
        exit(1);
    }
    *value = (ctx->data[ctx->pos] << 8) | ctx->data[ctx->pos + 1];
    ctx->pos += 2;
}

void hammer_uint8(hammer_context_t* ctx, uint8_t* value) {
    if (ctx->pos + 1 > ctx->size) {
        printf("Error: end of data reached\n");
        exit(1);
    }
    *value = ctx->data[ctx->pos];
    ctx->pos += 1;
}

void hammer_bytes(hammer_context_t* ctx, uint8_t* data, size_t size) {
    if (ctx->pos + size > ctx->size) {
        printf("Error: end of data reached\n");
        exit(1);
    }
    memcpy(data, ctx->data + ctx->pos, size);
    ctx->pos += size;
}

void file_header_parser(hammer_context_t* ctx, file_header_t* file_header) {
    hammer_uint32_be(ctx, &file_header->magic);
    hammer_uint8(ctx, &file_header->file_format_write_version);
    hammer_uint8(ctx, &file_header->file_format_read_version);
    hammer_uint8(ctx, &file_header->reserved);
    hammer_uint8(ctx, &file_header->max_embedded_payload_fraction);
    hammer_uint8(ctx, &file_header->min_embedded_payload_fraction);
    hammer_uint8(ctx, &file_header->leaf_payload_fraction);
    hammer_uint32_be(ctx, &file_header->file_change_counter);
}

void page_header_parser(hammer_context_t* ctx, page_header_t* page_header) {
    hammer_uint8(ctx, &page_header->page_type);
    hammer_uint16_be(ctx, &page_header->first_freeblock);
    hammer_uint16_be(ctx, &page_header->cell_offset);
    hammer_uint16_be(ctx, &page_header->num_cells);
}

void cell_parser(hammer_context_t* ctx, cell_t* cell) {
    hammer_uint16_be(ctx, &cell->payload_length);
    hammer_uint8(ctx, &cell->header_length);
    hammer_uint8(ctx, &cell->num_columns);
    cell->column_data = malloc(cell->payload_length);
    hammer_bytes(ctx, cell->column_data, cell->payload_length);
}

void master_journal_page_parser(hammer_context_t* ctx, master_journal_page_t* master_journal_page) {
    hammer_uint32_be(ctx, &master_journal_page->page_number);
    hammer_uint32_be(ctx, &master_journal_page->database_page);
    hammer_uint32_be(ctx, &master_journal_page->checksum);
}

void master_journal_header_parser(hammer_context_t* ctx, master_journal_header_t* master_journal_header) {
    hammer_uint32_be(ctx, &master_journal_header->magic);
    hammer_uint32_be(ctx, &master_journal_header->version);
    hammer_uint32_be(ctx, &master_journal_header->master_journal_page_count);
    hammer_uint32_be(ctx, &master_journal_header->master_journal_page);
    hammer_uint32_be(ctx, &master_journal_header->master_journal_checksum);
}

void wal_header_parser(hammer_context_t* ctx, wal_header_t* wal_header) {
    hammer_uint32_be(ctx, &wal_header->magic);
    hammer_uint32_be(ctx, &wal_header->file_format_version);
    hammer_uint32_be(ctx, &wal_header->page_size);
    hammer_uint32_be(ctx, &wal_header->checkpoint_sequence);
    hammer_uint32_be(ctx, &wal_header->salt1);
    hammer_uint32_be(ctx, &wal_header->salt2);
    hammer_uint32_be(ctx, &wal_header->checksum_initial_value);
}

void wal_frame_header_parser(hammer_context_t* ctx, wal_frame_header_t* wal_frame_header) {
    hammer_uint32_be(ctx, &wal_frame_header->page_number);
    hammer_uint8(ctx, &wal_frame_header->before_image);
    hammer_uint8(ctx, &wal_frame_header->number_of_pages);
    hammer_uint32_be(ctx, &wal_frame_header->salt);
    hammer_uint32_be(ctx, &wal_frame_header->checksum);
}

int main(int argc, char* argv[]) {
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
    long file_size = ftell(file);
    rewind(file);

    uint8_t* data = malloc(file_size);
    if (!data) {
        printf("Error allocating memory\n");
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    hammer_context_t ctx;
    hammer_init(&ctx, data, file_size);

    file_header_t file_header;
    file_header_parser(&ctx, &file_header);

    printf("File Header:\n");
    printf("  Magic: 0x%x\n", file_header.magic);
    printf("  File Format Write Version: %d\n", file_header.file_format_write_version);
    printf("  File Format Read Version: %d\n", file_header.file_format_read_version);
    printf("  Reserved: %d\n", file_header.reserved);
    printf("  Max Embedded Payload Fraction: %d\n", file_header.max_embedded_payload_fraction);
    printf("  Min Embedded Payload Fraction: %d\n", file_header.min_embedded_payload_fraction);
    printf("  Leaf Payload Fraction: %d\n", file_header.leaf_payload_fraction);
    printf("  File Change Counter: %d\n", file_header.file_change_counter);

    page_header_t page_header;
    page_header_parser(&ctx, &page_header);

    printf("Page Header:\n");
    printf("  Page Type: %d\n", page_header.page_type);
    printf("  First Freeblock: %d\n", page_header.first_freeblock);
    printf("  Cell Offset: %d\n", page_header.cell_offset);
    printf("  Number of Cells: %d\n", page_header.num_cells);

    cell_t cell;
    cell_parser(&ctx, &cell);

    printf("Cell:\n");
    printf("  Payload Length: %d\n", cell.payload_length);
    printf("  Header Length: %d\n", cell.header_length);
    printf("  Number of Columns: %d\n", cell.num_columns);
    printf("  Column Data: ");
    for (int i = 0; i < cell.payload_length; i++) {
        printf("%02x ", cell.column_data[i]);
    }
    printf("\n");

    master_journal_page_t master_journal_page;
    master_journal_page_parser(&ctx, &master_journal_page);

    printf("Master Journal Page:\n");
    printf("  Page Number: %d\n", master_journal_page.page_number);
    printf("  Database Page: %d\n", master_journal_page.database_page);
    printf("  Checksum: %d\n", master_journal_page.checksum);

    master_journal_header_t master_journal_header;
    master_journal_header_parser(&ctx, &master_journal_header);

    printf("Master Journal Header:\n");
    printf("  Magic: 0x%x\n", master_journal_header.magic);
    printf("  Version: %d\n", master_journal_header.version);
    printf("  Master Journal Page Count: %d\n", master_journal_header.master_journal_page_count);
    printf("  Master Journal Page: %d\n", master_journal_header.master_journal_page);
    printf("  Master Journal Checksum: %d\n", master_journal_header.master_journal_checksum);

    wal_header_t wal_header;
    wal_header_parser(&ctx, &wal_header);

    printf("WAL Header:\n");
    printf("  Magic: 0x%x\n", wal_header.magic);
    printf("  File Format Version: %d\n", wal_header.file_format_version);
    printf("  Page Size: %d\n", wal_header.page_size);
    printf("  Checkpoint Sequence: %d\n", wal_header.checkpoint_sequence);
    printf("  Salt1: %d\n", wal_header.salt1);
    printf("  Salt2: %d\n", wal_header.salt2);
    printf("  Checksum Initial Value: %d\n", wal_header.checksum_initial_value);

    wal_frame_header_t wal_frame_header;
    wal_frame_header_parser(&ctx, &wal_frame_header);

    printf("WAL Frame Header:\n");
    printf("  Page Number: %d\n", wal_frame_header.page_number);
    printf("  Before Image: %d\n", wal_frame_header.before_image);
    printf("  Number of Pages: %d\n", wal_frame_header.number_of_pages);
    printf("  Salt: %d\n", wal_frame_header.salt);
    printf("  Checksum: %d\n", wal_frame_header.checksum);

    free(data);
    free(cell.column_data);
    return 0;
}