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
HParser* dri_parser;
HParser* com_parser;
HParser* entropy_data_parser;

// Marker definitions
static const uint8_t SOI_MARKER[] = {0xFF, 0xD8};
static const uint8_t EOI_MARKER[] = {0xFF, 0xD9};
static const uint8_t SOF0_MARKER[] = {0xFF, 0xC0};
static const uint8_t SOF1_MARKER[] = {0xFF, 0xC1};
static const uint8_t SOF2_MARKER[] = {0xFF, 0xC2};
static const uint8_t SOF3_MARKER[] = {0xFF, 0xC3};
static const uint8_t DHT_MARKER[] = {0xFF, 0xC4};
static const uint8_t DQT_MARKER[] = {0xFF, 0xDB};
static const uint8_t SOS_MARKER[] = {0xFF, 0xDA};
static const uint8_t APP0_MARKER[] = {0xFF, 0xE0};
static const uint8_t DRI_MARKER[] = {0xFF, 0xDD};
static const uint8_t COM_MARKER[] = {0xFF, 0xFE};

// Helper parsers
static HParser* marker(const uint8_t* marker_bytes) {
    return h_token(marker_bytes, 2);
}

static HParser* length_field() {
    return h_uint16();
}

// SOF parser
static HParser* init_sof_parser() {
    return h_sequence(
        h_choice(
            marker(SOF0_MARKER),
            marker(SOF1_MARKER),
            marker(SOF2_MARKER),
            marker(SOF3_MARKER),
            NULL
        ),
        length_field(),
        h_uint8(),
        h_uint16(),
        h_uint16(),
        h_uint8(),
        h_many(h_sequence(
            h_uint8(),
            h_uint8(),
            h_uint8(),
            NULL
        )),
        NULL
    );
}

// DHT parser
static HParser* init_dht_parser() {
    return h_sequence(
        marker(DHT_MARKER),
        length_field(),
        h_uint8(),
        h_repeat_n(h_uint8(), 16),
        h_many(h_uint8()),
        NULL
    );
}

// DQT parser
static HParser* init_dqt_parser() {
    return h_sequence(
        marker(DQT_MARKER),
        length_field(),
        h_uint8(),
        h_many(h_uint8()),
        NULL
    );
}

// SOS parser
static HParser* init_sos_parser() {
    return h_sequence(
        marker(SOS_MARKER),
        length_field(),
        h_uint8(),
        h_many(h_sequence(
            h_uint8(),
            h_uint8(),
            NULL
        )),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        NULL
    );
}

// APP parser
static HParser* init_app_parser() {
    return h_sequence(
        marker(APP0_MARKER),
        length_field(),
        h_many(h_uint8()),
        NULL
    );
}

// DRI parser
static HParser* init_dri_parser() {
    return h_sequence(
        marker(DRI_MARKER),
        length_field(),
        h_uint16(),
        NULL
    );
}

// COM parser
static HParser* init_com_parser() {
    return h_sequence(
        marker(COM_MARKER),
        length_field(),
        h_many(h_uint8()),
        NULL
    );
}

// Entropy data parser
static HParser* init_entropy_data_parser() {
    return h_many(h_uint8());
}

// Main JPEG parser
static HParser* init_jpeg_parser() {
    return h_sequence(
        marker(SOI_MARKER),
        h_many(h_choice(
            sof_parser,
            dht_parser,
            dqt_parser,
            sos_parser,
            app_parser,
            dri_parser,
            com_parser,
            entropy_data_parser,
            NULL
        )),
        marker(EOI_MARKER),
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
    dri_parser = init_dri_parser();
    com_parser = init_com_parser();
    entropy_data_parser = init_entropy_data_parser();
    jpeg_parser = init_jpeg_parser();
}

int main(int argc, char** argv) {
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
    if (!result) {
        fprintf(stderr, "Failed to parse JPEG\n");
        free(buffer);
        fclose(fp);
        return 1;
    }

    h_parse_result_free(result);
    free(buffer);
    fclose(fp);
    return 0;
}