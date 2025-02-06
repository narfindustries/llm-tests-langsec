#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <arpa/inet.h>

typedef struct {
    uint32_t seconds;
    uint32_t fraction;
} ntp_timestamp_t;

typedef struct {
    uint8_t li_vn_mode;
    uint8_t stratum;
    int8_t poll;
    int8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t ref_id;
    ntp_timestamp_t ref_time;
    ntp_timestamp_t orig_time;
    ntp_timestamp_t recv_time;
    ntp_timestamp_t trans_time;
} ntp_packet_t;

uint8_t get_bits(uint8_t value, int start, int len) {
    return (value >> start) & ((1 << len) - 1);
}

hammer_parser_t* parse_li(void) {
    return hammer_map(hammer_uint8(NULL), &(hammer_map_func){.func = (void*)get_bits, .arg = (void*)(uintptr_t)0});
}

hammer_parser_t* parse_vn(void) {
    return hammer_map(hammer_uint8(NULL), &(hammer_map_func){.func = (void*)get_bits, .arg = (void*)(uintptr_t)2});
}

hammer_parser_t* parse_mode(void) {
    return hammer_map(hammer_uint8(NULL), &(hammer_map_func){.func = (void*)get_bits, .arg = (void*)(uintptr_t)5});
}

hammer_parser_t* parse_stratum(void) {
    return hammer_uint8(NULL);
}

hammer_parser_t* parse_poll(void) {
    return hammer_int8(NULL);
}

hammer_parser_t* parse_precision(void) {
    return hammer_int8(NULL);
}

hammer_parser_t* parse_root_delay(void) {
    return hammer_uint32(NULL);
}

hammer_parser_t* parse_root_dispersion(void) {
    return hammer_uint32(NULL);
}

hammer_parser_t* parse_ref_id(void) {
    return hammer_uint32(NULL);
}

hammer_parser_t* parse_ntp_timestamp(void) {
    return hammer_seq(NULL, hammer_uint32(NULL), hammer_uint32(NULL));
}

hammer_parser_t* parse_ntp_packet(void) {
    return hammer_seq(NULL, parse_li(), parse_vn(), parse_mode(), parse_stratum(),
                      parse_poll(), parse_precision(), parse_root_delay(),
                      parse_root_dispersion(), parse_ref_id(),
                      parse_ntp_timestamp(), parse_ntp_timestamp(),
                      parse_ntp_timestamp(), parse_ntp_timestamp());
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char *buffer = (char *)malloc(fsize);
    fread(buffer, 1, fsize, fp);
    fclose(fp);

    hammer_parser_t* parser = parse_ntp_packet();
    hammer_result_t result = hammer_parse(parser, buffer, fsize);

    if (result.success) {
        ntp_packet_t* packet = (ntp_packet_t*)result.value;
        // Add your printing logic here
    } else {
        fprintf(stderr, "Error parsing NTP packet: %s\n", result.error);
    }

    free(buffer);
    hammer_free(parser);

    return 0;
}
