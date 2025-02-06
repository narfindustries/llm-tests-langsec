#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define SOI 0xD8FF
#define APP0 0xE0FF
#define APP1 0xE1FF
#define DQT 0xDBFF
#define SOF0 0xC0FF
#define DHT 0xC4FF
#define SOS 0xDAFF
#define EOI 0xD9FF
#define DRI 0xDDFF
#define COM 0xFEFF
#define APP2 0xE2FF
#define APP3 0xE3FF
#define APP4 0xE4FF
#define APP5 0xE5FF
#define APP6 0xE6FF
#define APP7 0xE7FF
#define APP8 0xE8FF
#define APP9 0xE9FF
#define APP10 0xEAFF
#define APP11 0xEBFF
#define APP12 0xECFF
#define APP13 0xEDFF
#define APP14 0xEEFF
#define APP15 0xEFFF

typedef struct {
    uint16_t length;
    uint8_t identifier[5];
    uint16_t version;
    uint8_t units;
    uint16_t xdensity;
    uint16_t ydensity;
    uint8_t thumb_width;
    uint8_t thumb_height;
} app0_t;

typedef struct {
    uint16_t length;
    uint8_t identifier[0];
} app1_t;

typedef struct {
    uint16_t length;
    uint8_t table_number;
    uint8_t precision;
    uint8_t table_data[0];
} dqt_t;

typedef struct {
    uint16_t length;
    uint8_t precision;
    uint16_t image_height;
    uint16_t image_width;
    uint8_t number_of_components;
    uint8_t component_identifier;
    uint8_t horizontal_sampling_factor;
    uint8_t vertical_sampling_factor;
    uint8_t quantization_table_number;
} sof0_t;

typedef struct {
    uint16_t length;
    uint8_t table_class;
    uint8_t table_number;
    uint8_t table_data[0];
} dht_t;

typedef struct {
    uint16_t length;
    uint8_t number_of_components;
    uint8_t component_identifier;
    uint8_t dc_entropy_coding;
    uint8_t ac_entropy_coding;
} sos_t;

typedef struct {
    uint16_t length;
    uint16_t restart_interval;
} dri_t;

typedef struct {
    uint16_t length;
    uint8_t comment[0];
} com_t;

typedef struct {
    uint16_t length;
    uint8_t identifier[0];
} app2_t;

typedef struct {
    uint16_t length;
    uint8_t identifier[0];
} app3_t;

typedef struct {
    uint16_t length;
    uint8_t identifier[0];
} app4_t;

typedef struct {
    uint16_t length;
    uint8_t identifier[0];
} app5_t;

typedef struct {
    uint16_t length;
    uint8_t identifier[0];
} app6_t;

typedef struct {
    uint16_t length;
    uint8_t identifier[0];
} app7_t;

typedef struct {
    uint16_t length;
    uint8_t identifier[0];
} app8_t;

typedef struct {
    uint16_t length;
    uint8_t identifier[0];
} app9_t;

typedef struct {
    uint16_t length;
    uint8_t identifier[0];
} app10_t;

typedef struct {
    uint16_t length;
    uint8_t identifier[0];
} app11_t;

typedef struct {
    uint16_t length;
    uint8_t identifier[0];
} app12_t;

typedef struct {
    uint16_t length;
    uint8_t identifier[0];
} app13_t;

typedef struct {
    uint16_t length;
    uint8_t identifier[0];
} app14_t;

typedef struct {
    uint16_t length;
    uint8_t identifier[0];
} app15_t;

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(file_size);
    if (!data) {
        printf("Error allocating memory\n");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(data, 1, file_size, file);
    if (bytes_read != file_size) {
        printf("Error reading file %s\n", argv[1]);
        free(data);
        fclose(file);
        return 1;
    }

    fclose(file);

    HParser *soi_parser = h_word_u16be(SOI);
    HParser *app0_parser = h_struct(
        app0_t,
        h_word_u16be(APP0),
        h_word_u16be(),
        h_bytes(5),
        h_word_u16be(),
        h_byte(),
        h_word_u16be(),
        h_word_u16be(),
        h_byte(),
        h_byte()
    );

    HParser *app1_parser = h_struct(
        app1_t,
        h_word_u16be(APP1),
        h_word_u16be(),
        h_bytes_until(h_any())
    );

    HParser *dqt_parser = h_struct(
        dqt_t,
        h_word_u16be(DQT),
        h_word_u16be(),
        h_byte(),
        h_byte(),
        h_bytes_until(h_any())
    );

    HParser *sof0_parser = h_struct(
        sof0_t,
        h_word_u16be(SOF0),
        h_word_u16be(),
        h_byte(),
        h_word_u16be(),
        h_word_u16be(),
        h_byte(),
        h_byte(),
        h_byte(),
        h_byte(),
        h_byte()
    );

    HParser *dht_parser = h_struct(
        dht_t,
        h_word_u16be(DHT),
        h_word_u16be(),
        h_byte(),
        h_byte(),
        h_bytes_until(h_any())
    );

    HParser *sos_parser = h_struct(
        sos_t,
        h_word_u16be(SOS),
        h_word_u16be(),
        h_byte(),
        h_byte(),
        h_byte(),
        h_byte()
    );

    HParser *eoi_parser = h_word_u16be(EOI);

    HParser *dri_parser = h_struct(
        dri_t,
        h_word_u16be(DRI),
        h_word_u16be(),
        h_word_u16be()
    );

    HParser *com_parser = h_struct(
        com_t,
        h_word_u16be(COM),
        h_word_u16be(),
        h_bytes_until(h_any())
    );

    HParser *app2_parser = h_struct(
        app2_t,
        h_word_u16be(APP2),
        h_word_u16be(),
        h_bytes_until(h_any())
    );

    HParser *app3_parser = h_struct(
        app3_t,
        h_word_u16be(APP3),
        h_word_u16be(),
        h_bytes_until(h_any())
    );

    HParser *app4_parser = h_struct(
        app4_t,
        h_word_u16be(APP4),
        h_word_u16be(),
        h_bytes_until(h_any())
    );

    HParser *app5_parser = h_struct(
        app5_t,
        h_word_u16be(APP5),
        h_word_u16be(),
        h_bytes_until(h_any())
    );

    HParser *app6_parser = h_struct(
        app6_t,
        h_word_u16be(APP6),
        h_word_u16be(),
        h_bytes_until(h_any())
    );

    HParser *app7_parser = h_struct(
        app7_t,
        h_word_u16be(APP7),
        h_word_u16be(),
        h_bytes_until(h_any())
    );

    HParser *app8_parser = h_struct(
        app8_t,
        h_word_u16be(APP8),
        h_word_u16be(),
        h_bytes_until(h_any())
    );

    HParser *app9_parser = h_struct(
        app9_t,
        h_word_u16be(APP9),
        h_word_u16be(),
        h_bytes_until(h_any())
    );

    HParser *app10_parser = h_struct(
        app10_t,
        h_word_u16be(APP10),
        h_word_u16be(),
        h_bytes_until(h_any())
    );

    HParser *app11_parser = h_struct(
        app11_t,
        h_word_u16be(APP11),
        h_word_u16be(),
        h_bytes_until(h_any())
    );

    HParser *app12_parser = h_struct(
        app12_t,
        h_word_u16be(APP12),
        h_word_u16be(),
        h_bytes_until(h_any())
    );

    HParser *app13_parser = h_struct(
        app13_t,
        h_word_u16be(APP13),
        h_word_u16be(),
        h_bytes_until(h_any())
    );

    HParser *app14_parser = h_struct(
        app14_t,
        h_word_u16be(APP14),
        h_word_u16be(),
        h_bytes_until(h_any())
    );

    HParser *app15_parser = h_struct(
        app15_t,
        h_word_u16be(APP15),
        h_word_u16be(),
        h_bytes_until(h_any())
    );

    HParser *jpeg_parser = h_choice(
        soi_parser,
        app0_parser,
        app1_parser,
        dqt_parser,
        sof0_parser,
        dht_parser,
        sos_parser,
        eoi_parser,
        dri_parser,
        com_parser,
        app2_parser,
        app3_parser,
        app4_parser,
        app5_parser,
        app6_parser,
        app7_parser,
        app8_parser,
        app9_parser,
        app10_parser,
        app11_parser,
        app12_parser,
        app13_parser,
        app14_parser,
        app15_parser
    );

    HParseResult *result = h_parse(jpeg_parser, data, file_size);
    if (result->status == H_OK) {
        printf("Parsed JPEG file successfully\n");
    } else {
        printf("Error parsing JPEG file\n");
    }

    free(data);
    return 0;
}