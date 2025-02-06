#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define HAMMER_HEADER
#define HAMMER_TAIL

typedef enum {
    SOF0,
    SOF2,
    DHT,
    DAC,
    SOF6,
    SOF9,
    SOF10,
    SOF11,
    SOF13,
    RST0,
    RST1,
    RST2,
    RST3,
    RST4,
    RST5,
    RST6,
    RST7,
    DNL,
    DQT,
    DRI,
    DHP,
    SOI,
    EOI,
    SOS,
    APP0,
    APP1,
    APP2,
    APP3,
    APP4,
    APP5,
    APP6,
    APP7,
    APP8,
    APP9,
    APP10,
    APP11,
    APP12,
    APP13,
    APP14,
    APP15,
    EXP,
    JPG
} marker_type;

typedef struct {
    uint8_t marker;
    uint8_t code;
} marker_segment;

typedef struct {
    uint8_t precision;
    uint16_t y_sampling_factor;
    uint16_t x_sampling_factor;
    uint8_t num_components;
    uint8_t component_id;
    uint8_t h_sampling_factor;
    uint8_t v_sampling_factor;
    uint8_t tq;
} sof_segment;

typedef struct {
    uint8_t tc;
    uint8_t th;
    uint16_t l;
    uint8_t *v;
} dht_segment;

typedef struct {
    uint8_t pq;
    uint8_t tq;
    uint8_t *q;
} dqt_segment;

typedef struct {
    uint8_t ns;
    uint8_t component_id;
    uint8_t td;
    uint8_t ta;
    uint8_t ss;
    uint8_t se;
    uint8_t ah;
    uint8_t al;
} sos_segment;

typedef struct {
    uint8_t m;
    uint16_t l;
    uint8_t *data;
} app_segment;

typedef struct {
    uint8_t *data;
    size_t size;
} jpeg_file;

typedef struct {
    uint8_t *data;
    size_t size;
    size_t pos;
} hammer_context_t;

void hammer_rule_marker_segment(hammer_context_t *ctx, marker_segment *segment) {
    segment->marker = ctx->data[ctx->pos++];
    segment->code = ctx->data[ctx->pos++];
}

void hammer_rule_sof_segment(hammer_context_t *ctx, sof_segment *segment) {
    segment->precision = ctx->data[ctx->pos++];
    segment->y_sampling_factor = (ctx->data[ctx->pos++] << 8) | ctx->data[ctx->pos++];
    segment->x_sampling_factor = (ctx->data[ctx->pos++] << 8) | ctx->data[ctx->pos++];
    segment->num_components = ctx->data[ctx->pos++];
    for (int i = 0; i < segment->num_components; i++) {
        segment->component_id = ctx->data[ctx->pos++];
        segment->h_sampling_factor = ctx->data[ctx->pos++];
        segment->v_sampling_factor = ctx->data[ctx->pos++];
        segment->tq = ctx->data[ctx->pos++];
    }
}

void hammer_rule_dht_segment(hammer_context_t *ctx, dht_segment *segment) {
    segment->tc = ctx->data[ctx->pos++];
    segment->th = ctx->data[ctx->pos++];
    segment->l = (ctx->data[ctx->pos++] << 8) | ctx->data[ctx->pos++];
    segment->v = malloc(segment->l);
    for (int i = 0; i < segment->l; i++) {
        segment->v[i] = ctx->data[ctx->pos++];
    }
}

void hammer_rule_dqt_segment(hammer_context_t *ctx, dqt_segment *segment) {
    segment->pq = ctx->data[ctx->pos++];
    segment->tq = ctx->data[ctx->pos++];
    segment->q = malloc(64);
    for (int i = 0; i < 64; i++) {
        segment->q[i] = ctx->data[ctx->pos++];
    }
}

void hammer_rule_sos_segment(hammer_context_t *ctx, sos_segment *segment) {
    segment->ns = ctx->data[ctx->pos++];
    for (int i = 0; i < segment->ns; i++) {
        segment->component_id = ctx->data[ctx->pos++];
        segment->td = ctx->data[ctx->pos++];
        segment->ta = ctx->data[ctx->pos++];
    }
    segment->ss = ctx->data[ctx->pos++];
    segment->se = ctx->data[ctx->pos++];
    segment->ah = ctx->data[ctx->pos++];
    segment->al = ctx->data[ctx->pos++];
}

void hammer_rule_app_segment(hammer_context_t *ctx, app_segment *segment) {
    segment->m = ctx->data[ctx->pos++];
    segment->l = (ctx->data[ctx->pos++] << 8) | ctx->data[ctx->pos++];
    segment->data = malloc(segment->l);
    for (int i = 0; i < segment->l; i++) {
        segment->data[i] = ctx->data[ctx->pos++];
    }
}

void hammer_rule_jpeg_file(hammer_context_t *ctx, jpeg_file *file) {
    while (ctx->pos < ctx->size) {
        uint8_t marker;
        marker = ctx->data[ctx->pos++];
        switch (marker) {
            case SOF0:
                {
                    sof_segment segment;
                    hammer_rule_sof_segment(ctx, &segment);
                    break;
                }
            case DHT:
                {
                    dht_segment segment;
                    hammer_rule_dht_segment(ctx, &segment);
                    break;
                }
            case DQT:
                {
                    dqt_segment segment;
                    hammer_rule_dqt_segment(ctx, &segment);
                    break;
                }
            case SOS:
                {
                    sos_segment segment;
                    hammer_rule_sos_segment(ctx, &segment);
                    break;
                }
            case APP0:
            case APP1:
            case APP2:
            case APP3:
            case APP4:
            case APP5:
            case APP6:
            case APP7:
            case APP8:
            case APP9:
            case APP10:
            case APP11:
            case APP12:
            case APP13:
            case APP14:
            case APP15:
                {
                    app_segment segment;
                    hammer_rule_app_segment(ctx, &segment);
                    break;
                }
            default:
                break;
        }
    }
}

int main(int argc, char **argv) {
    if (argc != 2) {
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
    rewind(file);

    uint8_t *data = malloc(file_size);
    if (!data) {
        printf("Error allocating memory\n");
        return 1;
    }

    size_t bytes_read = fread(data, 1, file_size, file);
    if (bytes_read != file_size) {
        printf("Error reading file\n");
        return 1;
    }

    fclose(file);

    hammer_context_t ctx;
    ctx.data = data;
    ctx.size = file_size;
    ctx.pos = 0;

    jpeg_file file_data;
    file_data.data = data;
    file_data.size = file_size;

    hammer_rule_jpeg_file(&ctx, &file_data);

    free(data);
    return 0;
}