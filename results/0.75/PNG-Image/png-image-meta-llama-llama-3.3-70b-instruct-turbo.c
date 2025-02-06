#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#define UTF8(s) s, (sizeof(s) - 1)

typedef struct {
    uint32_t length;
    char type[4];
    uint8_t* data;
    uint32_t crc;
} chunk_t;

typedef struct {
    uint32_t width;
    uint32_t height;
    uint8_t bit_depth;
    uint8_t color_type;
    uint8_t compression_method;
    uint8_t filter_method;
    uint8_t interlace_method;
} ihdr_t;

typedef struct {
    uint8_t* palette;
    uint32_t length;
} plte_t;

typedef struct {
    uint8_t* data;
    uint32_t length;
} idat_t;

typedef struct {
    uint8_t intend;
} iend_t;

typedef struct {
    uint8_t* chromaticity;
    uint32_t length;
} chrm_t;

typedef struct {
    uint32_t gamma;
} gama_t;

typedef struct {
    uint8_t* significant_bits;
    uint32_t length;
} sbit_t;

typedef struct {
    uint8_t* srgb;
    uint32_t length;
} srgb_t;

typedef struct {
    uint8_t* iccp;
    uint32_t length;
} iccp_t;

typedef struct {
    uint8_t* text;
    uint32_t length;
    uint8_t* key;
    uint32_t key_length;
} text_t;

typedef struct {
    uint8_t* compressed_text;
    uint32_t length;
    uint8_t* key;
    uint32_t key_length;
} ztxt_t;

typedef struct {
    uint8_t* international_text;
    uint32_t length;
    uint8_t* key;
    uint32_t key_length;
    uint8_t* langtag;
    uint32_t langtag_length;
} itxt_t;

typedef struct {
    uint8_t* background;
    uint32_t length;
} bkgd_t;

typedef struct {
    uint8_t* histogram;
    uint32_t length;
} hist_t;

typedef struct {
    uint8_t* transparency;
    uint32_t length;
} trns_t;

typedef struct {
    uint32_t phys_units;
    uint32_t phys_ppu_x;
    uint32_t phys_ppu_y;
} phys_t;

typedef struct {
    int32_t scale_numerator;
    int32_t scale_denominator;
} scal_t;

typedef struct {
    uint8_t* valid;
    uint32_t length;
} vird_t;

uint32_t hammer_uint32_be(uint8_t* input, int* len) {
    uint32_t result = (input[0] << 24) | (input[1] << 16) | (input[2] << 8) | input[3];
    *len -= 4;
    return result;
}

uint8_t hammer_uint8(uint8_t* input, int* len) {
    uint8_t result = input[0];
    *len -= 1;
    return result;
}

char* hammer_string(uint8_t* input, int length, int* len) {
    char* result = malloc(length + 1);
    memcpy(result, input, length);
    result[length] = '\0';
    *len -= length;
    return result;
}

uint8_t* hammer_bytes(uint8_t* input, int length, int* len) {
    uint8_t* result = malloc(length);
    memcpy(result, input, length);
    *len -= length;
    return result;
}

int hammer_remaining(uint8_t* input, int* len) {
    return *len;
}

uint32_t hammer_uint32_be_input(uint8_t* input, int* len) {
    return hammer_uint32_be(input, len);
}

void* parse_chunk(uint8_t* input, void** output, int* len) {
    chunk_t* chunk = malloc(sizeof(chunk_t));
    *output = chunk;

    chunk->length = hammer_uint32_be(input, len);

    chunk->type[0] = input[0];
    chunk->type[1] = input[1];
    chunk->type[2] = input[2];
    chunk->type[3] = input[3];
    *len -= 4;

    chunk->data = hammer_bytes(input, chunk->length, len);

    chunk->crc = hammer_uint32_be(input, len);

    return NULL;
}

void* parse_ihdr(uint8_t* input, void** output, int* len) {
    ihdr_t* ihdr = malloc(sizeof(ihdr_t));
    *output = ihdr;

    ihdr->width = hammer_uint32_be(input, len);

    ihdr->height = hammer_uint32_be(input, len);

    ihdr->bit_depth = hammer_uint8(input, len);

    ihdr->color_type = hammer_uint8(input, len);

    ihdr->compression_method = hammer_uint8(input, len);

    ihdr->filter_method = hammer_uint8(input, len);

    ihdr->interlace_method = hammer_uint8(input, len);

    return NULL;
}

void* parse_plte(uint8_t* input, void** output, int* len) {
    plte_t* plte = malloc(sizeof(plte_t));
    *output = plte;

    plte->length = hammer_remaining(input, len);

    plte->palette = hammer_bytes(input, plte->length, len);

    return NULL;
}

void* parse_idat(uint8_t* input, void** output, int* len) {
    idat_t* idat = malloc(sizeof(idat_t));
    *output = idat;

    idat->length = hammer_remaining(input, len);

    idat->data = hammer_bytes(input, idat->length, len);

    return NULL;
}

void* parse_iend(uint8_t* input, void** output, int* len) {
    iend_t* iend = malloc(sizeof(iend_t));
    *output = iend;

    return NULL;
}

void* parse_chrm(uint8_t* input, void** output, int* len) {
    chrm_t* chrm = malloc(sizeof(chrm_t));
    *output = chrm;

    chrm->length = hammer_remaining(input, len);

    chrm->chromaticity = hammer_bytes(input, chrm->length, len);

    return NULL;
}

void* parse_gama(uint8_t* input, void** output, int* len) {
    gama_t* gama = malloc(sizeof(gama_t));
    *output = gama;

    gama->gamma = hammer_uint32_be(input, len);

    return NULL;
}

void* parse_sbit(uint8_t* input, void** output, int* len) {
    sbit_t* sbit = malloc(sizeof(sbit_t));
    *output = sbit;

    sbit->length = hammer_remaining(input, len);

    sbit->significant_bits = hammer_bytes(input, sbit->length, len);

    return NULL;
}

void* parse_srgb(uint8_t* input, void** output, int* len) {
    srgb_t* srgb = malloc(sizeof(srgb_t));
    *output = srgb;

    srgb->length = hammer_remaining(input, len);

    srgb->srgb = hammer_bytes(input, srgb->length, len);

    return NULL;
}

void* parse_iccp(uint8_t* input, void** output, int* len) {
    iccp_t* iccp = malloc(sizeof(iccp_t));
    *output = iccp;

    iccp->length = hammer_remaining(input, len);

    iccp->iccp = hammer_bytes(input, iccp->length, len);

    return NULL;
}

void* parse_text(uint8_t* input, void** output, int* len) {
    text_t* text = malloc(sizeof(text_t));
    *output = text;

    text->key_length = hammer_remaining(input, len);

    text->key = hammer_bytes(input, text->key_length, len);

    text->length = hammer_remaining(input, len);

    text->text = hammer_bytes(input, text->length, len);

    return NULL;
}

void* parse_ztxt(uint8_t* input, void** output, int* len) {
    ztxt_t* ztxt = malloc(sizeof(ztxt_t));
    *output = ztxt;

    ztxt->key_length = hammer_remaining(input, len);

    ztxt->key = hammer_bytes(input, ztxt->key_length, len);

    ztxt->length = hammer_remaining(input, len);

    ztxt->compressed_text = hammer_bytes(input, ztxt->length, len);

    return NULL;
}

void* parse_itxt(uint8_t* input, void** output, int* len) {
    itxt_t* itxt = malloc(sizeof(itxt_t));
    *output = itxt;

    itxt->key_length = hammer_remaining(input, len);

    itxt->key = hammer_bytes(input, itxt->key_length, len);

    itxt->langtag_length = hammer_remaining(input, len);

    itxt->langtag = hammer_bytes(input, itxt->langtag_length, len);

    itxt->length = hammer_remaining(input, len);

    itxt->international_text = hammer_bytes(input, itxt->length, len);

    return NULL;
}

void* parse_bkgd(uint8_t* input, void** output, int* len) {
    bkgd_t* bkgd = malloc(sizeof(bkgd_t));
    *output = bkgd;

    bkgd->length = hammer_remaining(input, len);

    bkgd->background = hammer_bytes(input, bkgd->length, len);

    return NULL;
}

void* parse_hist(uint8_t* input, void** output, int* len) {
    hist_t* hist = malloc(sizeof(hist_t));
    *output = hist;

    hist->length = hammer_remaining(input, len);

    hist->histogram = hammer_bytes(input, hist->length, len);

    return NULL;
}

void* parse_trns(uint8_t* input, void** output, int* len) {
    trns_t* trns = malloc(sizeof(trns_t));
    *output = trns;

    trns->length = hammer_remaining(input, len);

    trns->transparency = hammer_bytes(input, trns->length, len);

    return NULL;
}

void* parse_phys(uint8_t* input, void** output, int* len) {
    phys_t* phys = malloc(sizeof(phys_t));
    *output = phys;

    phys->phys_units = hammer_uint32_be(input, len);

    phys->phys_ppu_x = hammer_uint32_be(input, len);

    phys->phys_ppu_y = hammer_uint32_be(input, len);

    return NULL;
}

void* parse_scal(uint8_t* input, void** output, int* len) {
    scal_t* scal = malloc(sizeof(scal_t));
    *output = scal;

    scal->scale_numerator = (int32_t)hammer_uint32_be(input, len);

    scal->scale_denominator = (int32_t)hammer_uint32_be(input, len);

    return NULL;
}

void* parse_vird(uint8_t* input, void** output, int* len) {
    vird_t* vird = malloc(sizeof(vird_t));
    *output = vird;

    vird->length = hammer_remaining(input, len);

    vird->valid = hammer_bytes(input, vird->length, len);

    return NULL;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (file == NULL) {
        printf("Error opening file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* file_data = malloc(file_size);
    fread(file_data, 1, file_size, file);
    fclose(file);

    uint8_t* input = file_data;
    int len = file_size;

    uint8_t* signature = hammer_bytes(input, 8, &len);
    if (memcmp(signature, "\x89\x50\x4E\x47\x0D\x0A\x1A\x0A", 8) != 0) {
        printf("Invalid PNG signature\n");
        return 1;
    }

    while (len > 0) {
        chunk_t* chunk;
        parse_chunk(input, (void**)&chunk, &len);

        if (strcmp(chunk->type, "IHDR") == 0) {
            ihdr_t* ihdr;
            parse_ihdr(chunk->data, (void**)&ihdr, &chunk->length);
            printf("IHDR chunk:\n");
            printf("  width: %u\n", ihdr->width);
            printf("  height: %u\n", ihdr->height);
            printf("  bit depth: %u\n", ihdr->bit_depth);
            printf("  color type: %u\n", ihdr->color_type);
            printf("  compression method: %u\n", ihdr->compression_method);
            printf("  filter method: %u\n", ihdr->filter_method);
            printf("  interlace method: %u\n", ihdr->interlace_method);
        } else if (strcmp(chunk->type, "PLTE") == 0) {
            plte_t* plte;
            parse_plte(chunk->data, (void**)&plte, &chunk->length);
            printf("PLTE chunk:\n");
            printf("  palette: %p\n", plte->palette);
        } else if (strcmp(chunk->type, "IDAT") == 0) {
            idat_t* idat;
            parse_idat(chunk->data, (void**)&idat, &chunk->length);
            printf("IDAT chunk:\n");
            printf("  data: %p\n", idat->data);
        } else if (strcmp(chunk->type, "IEND") == 0) {
            iend_t* iend;
            parse_iend(chunk->data, (void**)&iend, &chunk->length);
            printf("IEND chunk:\n");
        } else if (strcmp(chunk->type, "cHRM") == 0) {
            chrm_t* chrm;
            parse_chrm(chunk->data, (void**)&chrm, &chunk->length);
            printf("cHRM chunk:\n");
            printf("  chromaticity: %p\n", chrm->chromaticity);
        } else if (strcmp(chunk->type, "gAMA") == 0) {
            gama_t* gama;
            parse_gama(chunk->data, (void**)&gama, &chunk->length);
            printf("gAMA chunk:\n");
            printf("  gamma: %u\n", gama->gamma);
        } else if (strcmp(chunk->type, "sBIT") == 0) {
            sbit_t* sbit;
            parse_sbit(chunk->data, (void**)&sbit, &chunk->length);
            printf("sBIT chunk:\n");
            printf("  significant bits: %p\n", sbit->significant_bits);
        } else if (strcmp(chunk->type, "sRGB") == 0) {
            srgb_t* srgb;
            parse_srgb(chunk->data, (void**)&srgb, &chunk->length);
            printf("sRGB chunk:\n");
            printf("  srgb: %p\n", srgb->srgb);
        } else if (strcmp(chunk->type, "iCCP") == 0) {
            iccp_t* iccp;
            parse_iccp(chunk->data, (void**)&iccp, &chunk->length);
            printf("iCCP chunk:\n");
            printf("  iccp: %p\n", iccp->iccp);
        } else if (strcmp(chunk->type, "tEXt") == 0) {
            text_t* text;
            parse_text(chunk->data, (void**)&text, &chunk->length);
            printf("tEXt chunk:\n");
            printf("  key: %p\n", text->key);
            printf("  text: %p\n", text->text);
        } else if (strcmp(chunk->type, "zTXt") == 0) {
            ztxt_t* ztxt;
            parse_ztxt(chunk->data, (void**)&ztxt, &chunk->length);
            printf("zTXt chunk:\n");
            printf("  key: %p\n", ztxt->key);
            printf("  compressed text: %p\n", ztxt->compressed_text);
        } else if (strcmp(chunk->type, "iTXt") == 0) {
            itxt_t* itxt;
            parse_itxt(chunk->data, (void**)&itxt, &chunk->length);
            printf("iTXt chunk:\n");
            printf("  key: %p\n", itxt->key);
            printf("  language tag: %p\n", itxt->langtag);
            printf("  international text: %p\n", itxt->international_text);
        } else if (strcmp(chunk->type, "bKGD") == 0) {
            bkgd_t* bkgd;
            parse_bkgd(chunk->data, (void**)&bkgd, &chunk->length);
            printf("bKGD chunk:\n");
            printf("  background: %p\n", bkgd->background);
        } else if (strcmp(chunk->type, "hIST") == 0) {
            hist_t* hist;
            parse_hist(chunk->data, (void**)&hist, &chunk->length);
            printf("hIST chunk:\n");
            printf("  histogram: %p\n", hist->histogram);
        } else if (strcmp(chunk->type, "tRNS") == 0) {
            trns_t* trns;
            parse_trns(chunk->data, (void**)&trns, &chunk->length);
            printf("tRNS chunk:\n");
            printf("  transparency: %p\n", trns->transparency);
        } else if (strcmp(chunk->type, "pHYs") == 0) {
            phys_t* phys;
            parse_phys(chunk->data, (void**)&phys, &chunk->length);
            printf("pHYs chunk:\n");
            printf("  phys units: %u\n", phys->phys_units);
            printf("  phys ppu x: %u\n", phys->phys_ppu_x);
            printf("  phys ppu y: %u\n", phys->phys_ppu_y);
        } else if (strcmp(chunk->type, "sCAL") == 0) {
            scal_t* scal;
            parse_scal(chunk->data, (void**)&scal, &chunk