#include <hammer/hammer.h>

// Define TIFF Header structure
typedef struct {
    uint16_t byte_order;
    uint16_t magic_number;
    uint32_t ifd_offset;
} tiff_header_t;

// Define TIFF IFD Entry structure
typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t value_offset;
} tiff_ifd_entry_t;

// Define TIFF Data structure
typedef struct {
    tiff_header_t header;
    uint16_t num_entries;
    tiff_ifd_entry_t *entries;
} tiff_data_t;

// Helper function to create a tiff_header parser
static HParser *create_tiff_header_parser(void) {
    return h_sequence(
        h_uint16(), // Byte Order
        h_uint16(), // Magic Number
        h_uint32()  // IFD Offset
    , NULL);
}

// Helper function to create a tiff_ifd_entry parser
static HParser *create_tiff_ifd_entry_parser(void) {
    return h_sequence(
        h_uint16(), // Tag
        h_uint16(), // Type
        h_uint32(), // Count
        h_uint32()  // Value Offset
    , NULL);
}

// Helper function to create a tiff_data parser
static HParser *create_tiff_data_parser(void) {
    HParser *header_parser = create_tiff_header_parser();
    HParser *ifd_entry_parser = create_tiff_ifd_entry_parser();

    HParser *num_entries_parser = h_uint16();
    HParser *entries_parser = h_repeat(ifd_entry_parser, num_entries_parser);

    return h_sequence(
        header_parser,
        num_entries_parser,
        entries_parser
    , NULL);
}

// Main function to parse TIFF data
tiff_data_t *parse_tiff_data(const uint8_t *data, size_t length) {
    HParser *tiff_parser = create_tiff_data_parser();
    HParseResult *result = h_parse(tiff_parser, data, length);

    if (!result) {
        return NULL;
    }

    const HParsedToken *token = result->ast;
    tiff_data_t *tiff_data = malloc(sizeof(tiff_data_t));

    tiff_data->header.byte_order = token->seq->elements[0]->uint;
    tiff_data->header.magic_number = token->seq->elements[1]->uint;
    tiff_data->header.ifd_offset = token->seq->elements[2]->uint;
    tiff_data->num_entries = token->seq->elements[3]->uint;

    tiff_data->entries = malloc(tiff_data->num_entries * sizeof(tiff_ifd_entry_t));
    for (uint16_t i = 0; i < tiff_data->num_entries; i++) {
        const HParsedToken *entry_token = token->seq->elements[4]->seq->elements[i];
        tiff_data->entries[i].tag = entry_token->seq->elements[0]->uint;
        tiff_data->entries[i].type = entry_token->seq->elements[1]->uint;
        tiff_data->entries[i].count = entry_token->seq->elements[2]->uint;
        tiff_data->entries[i].value_offset = entry_token->seq->elements[3]->uint;
    }

    h_parse_result_free(result);
    return tiff_data;
}

// Function to free TIFF data
void free_tiff_data(tiff_data_t *tiff_data) {
    if (tiff_data) {
        free(tiff_data->entries);
        free(tiff_data);
    }
}