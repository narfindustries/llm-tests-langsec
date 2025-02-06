#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Forward declarations
HParser* jpeg_parser;

// Marker parsers
static HParser* soi_marker;
static HParser* eoi_marker;
static HParser* app0_marker;
static HParser* dqt_marker;
static HParser* sof_marker;
static HParser* dht_marker;
static HParser* sos_marker;
static HParser* rst_marker;
static HParser* com_marker;
static HParser* appn_marker;

// Helper parsers
static HParser* jpeg_segment;
static HParser* entropy_coded_data;

void init_parsers() {
    // Basic markers
    soi_marker = h_sequence(h_ch(0xFF), h_ch(0xD8), NULL);
    eoi_marker = h_sequence(h_ch(0xFF), h_ch(0xD9), NULL);
    
    // APP0 (JFIF) segment
    HParser* jfif_identifier = h_sequence(
        h_token((const uint8_t*)"JFIF\0", 5),
        h_uint8(), // Major version
        h_uint8(), // Minor version
        h_uint8(), // Units
        h_uint16(), // X density
        h_uint16(), // Y density
        h_uint8(), // Thumbnail width
        h_uint8(), // Thumbnail height
        NULL
    );
    
    app0_marker = h_sequence(
        h_ch(0xFF),
        h_ch(0xE0),
        h_length_value(h_uint16(), jfif_identifier),
        NULL
    );
    
    // Quantization table
    HParser* dqt_data = h_sequence(
        h_uint8(), // Precision and table ID
        h_repeat_n(h_uint8(), 64), // Table elements
        NULL
    );
    
    dqt_marker = h_sequence(
        h_ch(0xFF),
        h_ch(0xDB),
        h_length_value(h_uint16(), dqt_data),
        NULL
    );
    
    // Component info for SOF
    HParser* component_info = h_sequence(
        h_uint8(), // Component ID
        h_uint8(), // Sampling factors
        h_uint8(), // Quantization table ID
        NULL
    );
    
    // Start of Frame
    HParser* sof_data = h_sequence(
        h_uint8(), // Precision
        h_uint16(), // Height
        h_uint16(), // Width
        h_uint8(), // Number of components
        h_repeat_n(component_info, 3), // Component info
        NULL
    );
    
    sof_marker = h_sequence(
        h_ch(0xFF),
        h_ch(0xC0),
        h_length_value(h_uint16(), sof_data),
        NULL
    );
    
    // Huffman table
    HParser* dht_data = h_sequence(
        h_uint8(), // Table class and ID
        h_repeat_n(h_uint8(), 16), // Number of codes
        h_many(h_uint8()), // Table values
        NULL
    );
    
    dht_marker = h_sequence(
        h_ch(0xFF),
        h_ch(0xC4),
        h_length_value(h_uint16(), dht_data),
        NULL
    );
    
    // Scan component info
    HParser* scan_component = h_sequence(
        h_uint8(), // Component ID
        h_uint8(), // DC/AC table selector
        NULL
    );
    
    // Start of Scan
    HParser* sos_data = h_sequence(
        h_uint8(), // Number of components
        h_repeat_n(scan_component, 3),
        h_uint8(), // Start of spectral selection
        h_uint8(), // End of spectral selection
        h_uint8(), // Successive approximation
        NULL
    );
    
    sos_marker = h_sequence(
        h_ch(0xFF),
        h_ch(0xDA),
        h_length_value(h_uint16(), sos_data),
        NULL
    );
    
    // Restart markers
    rst_marker = h_sequence(
        h_ch(0xFF),
        h_ch_range(0xD0, 0xD7),
        NULL
    );
    
    // Comment marker
    com_marker = h_sequence(
        h_ch(0xFF),
        h_ch(0xFE),
        h_length_value(h_uint16(), h_many(h_uint8())),
        NULL
    );
    
    // Generic APPn markers
    appn_marker = h_sequence(
        h_ch(0xFF),
        h_ch_range(0xE0, 0xEF),
        h_length_value(h_uint16(), h_many(h_uint8())),
        NULL
    );
    
    // Entropy coded data (simplified)
    entropy_coded_data = h_many(h_uint8());
    
    // Segment parser
    jpeg_segment = h_choice(
        app0_marker,
        dqt_marker,
        sof_marker,
        dht_marker,
        sos_marker,
        rst_marker,
        com_marker,
        appn_marker,
        NULL
    );
    
    // Complete JPEG parser
    jpeg_parser = h_sequence(
        soi_marker,
        h_many(jpeg_segment),
        entropy_coded_data,
        eoi_marker,
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <jpeg_file>\n", argv[0]);
        return 1;
    }
    
    FILE* fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }
    
    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    
    uint8_t* buffer = malloc(size);
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
    
    init_parsers();
    
    HParseResult* result = h_parse(jpeg_parser, buffer, size);
    if (result) {
        printf("Successfully parsed JPEG file\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse JPEG file\n");
    }
    
    free(buffer);
    fclose(fp);
    return 0;
}