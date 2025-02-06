#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h> //Added for memcpy

typedef enum {
  ICMP_ECHO_REPLY = 0,
  ICMP_DEST_UNREACH = 3,
  ICMP_REDIRECT = 5,
  ICMP_ECHO_REQUEST = 8,
  ICMP_TIME_EXCEEDED = 11,
  ICMP_PARAM_PROBLEM = 12,
} icmp_type_t;

typedef enum {
  ICMP_NET_UNREACH = 0,
  ICMP_HOST_UNREACH = 1,
  ICMP_PROT_UNREACH = 2,
  ICMP_PORT_UNREACH = 3,
} icmp_code_t;

typedef struct {
  uint8_t type;
  uint8_t code;
  uint16_t checksum;
  uint16_t identifier;
  uint16_t sequence_number;
  uint8_t data[1024]; //Fixed size data buffer. Adjust as needed.
} icmp_header_t;


hammer_parser icmp_parser() {
    return h_seq(
        h_uint8(),
        h_uint8(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_bytes(sizeof(((icmp_header_t*)0)->data), "data") //Parse data as bytes
    );
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

  uint8_t *buffer = (uint8_t *)malloc(fsize);
  if (buffer == NULL) {
    perror("Memory allocation failed");
    fclose(fp);
    return 1;
  }

  fread(buffer, 1, fsize, fp);
  fclose(fp);

  hammer_result result = hammer_parse(icmp_parser(), buffer, fsize);

  if (result.success) {
    icmp_header_t *icmp_header = (icmp_header_t *)malloc(sizeof(icmp_header_t));
    if(icmp_header == NULL){
        perror("Memory allocation failed");
        free(buffer);
        return 1;
    }
    memcpy(icmp_header, result.value, sizeof(icmp_header_t)); //Copy parsed data to icmp_header

    printf("ICMP Header:\n");
    printf("Type: %u\n", icmp_header->type);
    printf("Code: %u\n", icmp_header->code);
    printf("Checksum: %u\n", icmp_header->checksum);
    printf("Identifier: %u\n", icmp_header->identifier);
    printf("Sequence Number: %u\n", icmp_header->sequence_number);

    size_t data_len = result.length - sizeof(icmp_header_t);
    printf("Data Length: %zu\n", data_len);
    //Access and print data if needed:  printf("Data: %s\n", icmp_header->data);

    free(result.value);
    free(icmp_header);
  } else {
    fprintf(stderr, "Parsing failed at offset %zu: %s\n", result.offset, result.error);
  }

  free(buffer);
  return 0;
}
