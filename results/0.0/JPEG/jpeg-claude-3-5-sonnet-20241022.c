#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Forward declarations
HParser* jpeg_parser;
HParser* sof_parser;
HParser* dht_parser;
HParser* dqt_parser;
HParser* sos_parser;
HParser* app_parser;
HParser* com_parser;
HParser* dri_parser;
HParser* restart_parser;
HParser* scan_data_parser;

// Marker definitions
#define SOI 0xFFD8
#define EOI 0xFFD9
#define SOF0 0xFFC0
#define SOF1 0xFFC1
#define SOF2 0xFFC2
#define DHT 0xFFC4
#define DQT 0xFFDB
#define SOS 0xFFDA
#define APP0 0xFFE0
#define COM 0xFFFE
#define DRI 0xFFDD

// Helper parsers
static HParser* uint8_parser() {
    return h_uint8();
}

static HParser* uint16_parser() {
    return h_uint16();
}

static HParser* marker_parser(uint16_t marker) {
    return h_int_range(h_uint16(), marker, marker);
}

// Component parser
static HParser* component_parser() {
    return h_sequence(
        uint8_parser(),  // Component ID
        uint8_parser(),  // Sampling factors
        uint8_parser(),  // Quantization table number
        NULL
    );
}

// SOF parser
static HParser* init_sof_parser() {
    return h_sequence(
        h_choice(
            marker_parser(SOF0),
            marker_parser(SOF1),
            marker_parser(SOF2),
            NULL
        ),
        uint16_parser(),  // Length
        uint8_parser(),   // Precision
        uint16_parser(),  // Height
        uint16_parser(),  // Width
        uint8_parser(),   // Number of components
        h_repeat_n(component_parser(), 3),  // Components
        NULL
    );
}

// DHT parser
static HParser* init_dht_parser() {
    return h_sequence(
        marker_parser(DHT),
        uint16_parser(),  // Length
        uint8_parser(),   // Table class and ID
        h_repeat_n(uint8_parser(), 16),  // Number of codes
        h_many(uint8_parser()),  // Huffman values
        NULL
    );
}

// DQT parser
static HParser* init_dqt_parser() {
    return h_sequence(
        marker_parser(DQT),
        uint16_parser(),  // Length
        uint8_parser(),   // Precision and table ID
        h_repeat_n(uint8_parser(), 64),  // Quantization values
        NULL
    );
}

// SOS parser
static HParser* init_sos_parser() {
    return h_sequence(
        marker_parser(SOS),
        uint16_parser(),  // Length
        uint8_parser(),   // Number of components
        h_many1(h_sequence(
            uint8_parser(),  // Component ID
            uint8_parser(),  // DC/AC table selectors
            NULL
        )),
        uint8_parser(),  // Start of spectral selection
        uint8_parser(),  // End of spectral selection
        uint8_parser(),  // Successive approximation
        NULL
    );
}

// APP parser
static HParser* init_app_parser() {
    return h_sequence(
        h_int_range(h_uint16(), APP0, APP0 + 15),
        uint16_parser(),  // Length
        h_many(uint8_parser()),  // Application data
        NULL
    );
}

// COM parser
static HParser* init_com_parser() {
    return h_sequence(
        marker_parser(COM),
        uint16_parser(),  // Length
        h_many(uint8_parser()),  // Comment data
        NULL
    );
}

// DRI parser
static HParser* init_dri_parser() {
    return h_sequence(
        marker_parser(DRI),
        uint16_parser(),  // Length
        uint16_parser(),  // Restart interval
        NULL
    );
}

// Restart marker parser
static HParser* init_restart_parser() {
    return h_int_range(h_uint16(), 0xFFD0, 0xFFD7);
}

// Scan data parser
static HParser* init_scan_data_parser() {
    return h_many1(uint8_parser());
}

// Main JPEG parser
static HParser* init_jpeg_parser() {
    return h_sequence(
        marker_parser(SOI),
        h_many(h_choice(
            sof_parser,
            dht_parser,
            dqt_parser,
            sos_parser,
            app_parser,
            com_parser,
            dri_parser,
            restart_parser,
            scan_data_parser,
            NULL
        )),
        marker_parser(EOI),
        NULL
    );
}

// Initialize all parsers
static void init_parsers() {
    sof_parser = init_sof_parser();
    dht_parser = init_dht_parser();
    dqt_parser = init_dqt_parser();
    sos_parser = init_sos_parser();
    app_parser = init_app_parser();
    com_parser = init_com_parser();
    dri_parser = init_dri_parser();
    restart_parser = init_restart_parser();
    scan_data_parser = init_scan_data_parser();
    jpeg_parser = init_jpeg_parser();
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <jpeg_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    // Get file size
    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    // Read file into buffer
    uint8_t *buffer = malloc(size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(fp);
        return 1;
    }
    
    if (fread(buffer, 1, size, fp) != size) {
        perror("Failed to read file");
        free(buffer);
        fclose(fp);
        return 1;
    }

    // Initialize parsers
    init_parsers();

    // Parse JPEG
    HParseResult *result = h_parse(jpeg_parser, buffer, size);
    if (!result) {
        fprintf(stderr, "Failed to parse JPEG file\n");
        free(buffer);
        fclose(fp);
        return 1;
    }

    // Cleanup
    h_parse_result_free(result);
    free(buffer);
    fclose(fp);
    return 0;
}