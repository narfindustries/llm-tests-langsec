#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Forward declarations
static HParser* tiff_parser(void);
static HParser* ifd_entry_parser(void);
static HParser* header_parser(void);

// Type parsers
static HParser* byte_order_parser(void) {
    return h_choice(h_token((uint8_t*)"II", 2), 
                    h_token((uint8_t*)"MM", 2), 
                    NULL);
}

static HParser* version_parser(void) {
    return h_int_range(h_uint16(), 42, 42);
}

static HParser* offset_parser(void) {
    return h_uint32();
}

// Field type parsers
static HParser* byte_parser(void) {
    return h_uint8();
}

static HParser* ascii_parser(void) {
    return h_many1(h_not_in((uint8_t*)"\0", 1));
}

static HParser* short_parser(void) {
    return h_uint16();
}

static HParser* long_parser(void) {
    return h_uint32();
}

static HParser* rational_parser(void) {
    return h_sequence(long_parser(), long_parser(), NULL);
}

static HParser* sbyte_parser(void) {
    return h_int8();
}

static HParser* undefined_parser(void) {
    return h_uint8();
}

static HParser* sshort_parser(void) {
    return h_int16();
}

static HParser* slong_parser(void) {
    return h_int32();
}

static HParser* srational_parser(void) {
    return h_sequence(slong_parser(), slong_parser(), NULL);
}

static HParser* float_parser(void) {
    return h_bits(32, false);  // Using h_bits for float32
}

static HParser* double_parser(void) {
    return h_bits(64, false);  // Using h_bits for float64
}

// Tag value parsers
static HParser* tag_parser(void) {
    return h_uint16();
}

static HParser* type_parser(void) {
    return h_uint16();
}

static HParser* count_parser(void) {
    return h_uint32();
}

static HParser* value_offset_parser(void) {
    return h_uint32();
}

// IFD Entry parser
static HParser* ifd_entry_parser(void) {
    return h_sequence(
        tag_parser(),
        type_parser(),
        count_parser(),
        value_offset_parser(),
        NULL
    );
}

// IFD parser
static HParser* ifd_parser(void) {
    return h_sequence(
        h_uint16(), // Number of directory entries
        h_many1(ifd_entry_parser()),
        h_uint32(), // Offset to next IFD
        NULL
    );
}

// Header parser
static HParser* header_parser(void) {
    return h_sequence(
        byte_order_parser(),
        version_parser(),
        offset_parser(),
        NULL
    );
}

// Complete TIFF parser
static HParser* tiff_parser(void) {
    return h_sequence(
        header_parser(),
        h_many1(ifd_parser()),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    // Get file size
    fseek(fp, 0, SEEK_END);
    long file_size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    // Read file into buffer
    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(fp);
        return 1;
    }

    if (fread(buffer, 1, file_size, fp) != file_size) {
        perror("Failed to read file");
        free(buffer);
        fclose(fp);
        return 1;
    }

    // Parse TIFF
    HParseResult *result = h_parse(tiff_parser(), buffer, file_size);
    
    if (result) {
        printf("Successfully parsed TIFF file\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse TIFF file\n");
    }

    free(buffer);
    fclose(fp);
    return 0;
}