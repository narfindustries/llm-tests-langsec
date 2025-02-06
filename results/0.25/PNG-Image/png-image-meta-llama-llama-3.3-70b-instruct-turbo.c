#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define CHUNK_LENGTH 4
#define CHUNK_CRC 4

typedef enum {
    COLOR_TYPE_GRAYSCALE = 0,
    COLOR_TYPE_RGB = 2,
    COLOR_TYPE_PLTE = 3,
    COLOR_TYPE_GRAYSCALE_ALPHA = 4,
    COLOR_TYPE_RGBA = 6
} color_type_t;

typedef enum {
    COMPRESSION_METHOD_DEFLATE = 0
} compression_method_t;

typedef enum {
    FILTER_METHOD_ADAPTIVE = 0
} filter_method_t;

typedef enum {
    INTERLACE_METHOD_NONE = 0,
    INTERLACE_METHOD_ADAM7 = 1
} interlace_method_t;

typedef struct {
    uint32_t width;
    uint32_t height;
    uint8_t bit_depth;
    color_type_t color_type;
    compression_method_t compression_method;
    filter_method_t filter_method;
    interlace_method_t interlace_method;
} ihdr_chunk_t;

typedef struct {
    uint8_t red;
    uint8_t green;
    uint8_t blue;
} plte_entry_t;

typedef struct {
    plte_entry_t* entries;
    uint32_t length;
} plte_chunk_t;

typedef struct {
    uint8_t* data;
    uint32_t length;
} idat_chunk_t;

typedef struct {
    uint8_t* data;
    uint32_t length;
} chunk_t;

typedef struct {
    ihdr_chunk_t ihdr;
    plte_chunk_t* plte;
    idat_chunk_t* idat;
    chunk_t** ancillary_chunks;
    uint32_t num_ancillary_chunks;
} png_image_t;

HParser* png_magic_number_parser() {
    static uint8_t magic_number[] = {0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A};
    return h_bytes(magic_number, 8);
}

HParser* uint32_parser() {
    return h_uint32_be();
}

HParser* uint8_parser() {
    return h_uint8();
}

HParser* ihdr_chunk_parser() {
    return h_struct(
        h_uint32_be(),
        h_uint32_be(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        h_uint8()
    );
}

HParser* plte_chunk_parser() {
    return h_struct(
        h_repeat_n(
            h_struct(
                h_uint8(),
                h_uint8(),
                h_uint8()
            ),
            1,
            256
        )
    );
}

HParser* idat_chunk_parser() {
    return h_bytes(1, 1 << 31);
}

HParser* chunk_parser() {
    return h_struct(
        h_bytes(4, 4),
        h_uint32_be(),
        h_bytes(1, 1 << 31),
        h_uint32_be()
    );
}

HParser* png_image_parser() {
    return h_struct(
        png_magic_number_parser(),
        ihdr_chunk_parser(),
        h_optional(plte_chunk_parser()),
        idat_chunk_parser(),
        h_repeat_n(chunk_parser(), 0, 1 << 31)
    );
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
    long file_size = ftell(file);
    rewind(file);

    uint8_t* data = malloc(file_size);
    if (!data) {
        printf("Error allocating memory\n");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(data, 1, file_size, file);
    if (bytes_read != file_size) {
        printf("Error reading file\n");
        free(data);
        fclose(file);
        return 1;
    }

    fclose(file);

    HParser* parser = png_image_parser();
    HParseResult* result = h_parse(parser, data, file_size);

    if (h_result_is_ok(result)) {
        png_image_t* image = h_result_value(result);
        printf("Width: %u\n", image->ihdr.width);
        printf("Height: %u\n", image->ihdr.height);
        printf("Bit depth: %u\n", image->ihdr.bit_depth);
        printf("Color type: %u\n", image->ihdr.color_type);
        printf("Compression method: %u\n", image->ihdr.compression_method);
        printf("Filter method: %u\n", image->ihdr.filter_method);
        printf("Interlace method: %u\n", image->ihdr.interlace_method);
        if (image->plte) {
            printf("PLTE chunk:\n");
            for (uint32_t i = 0; i < image->plte->length; i++) {
                printf("  Entry %u: (%u, %u, %u)\n", i, image->plte->entries[i].red, image->plte->entries[i].green, image->plte->entries[i].blue);
            }
        }
        printf("IDAT chunk:\n");
        printf("  Length: %u\n", image->idat->length);
        printf("Ancillary chunks:\n");
        for (uint32_t i = 0; i < image->num_ancillary_chunks; i++) {
            printf("  Chunk %u:\n", i);
            printf("    Type: %s\n", image->ancillary_chunks[i]->data);
            printf("    Length: %u\n", image->ancillary_chunks[i]->length);
        }
    } else {
        printf("Error parsing PNG image\n");
    }

    free(data);
    return 0;
}