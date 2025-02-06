#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef enum {
  READ_COILS = 0x01,
  READ_DISCRETE_INPUTS = 0x02,
  READ_HOLDING_REGISTERS = 0x03,
  READ_INPUT_REGISTERS = 0x04,
  WRITE_SINGLE_COIL = 0x05,
  WRITE_SINGLE_REGISTER = 0x06,
  WRITE_MULTIPLE_COILS = 0x0F,
  WRITE_MULTIPLE_REGISTERS = 0x10,
} ModbusFunctionCode;

typedef struct {
  uint8_t function_code;
  uint16_t starting_address;
  uint16_t quantity;
  uint8_t data[];
} ModbusPDU;

static HParser pdu_parser = NULL;

static HParser uint8_parser = h_uint8();
static HParser uint16_parser = h_uint16_be();

static HParser modbus_pdu_parser() {
    if (pdu_parser) return pdu_parser;
    HParser data_parser = h_bytes(0); 

    pdu_parser = h_seq(
        h_field("function_code", uint8_parser),
        h_field("starting_address", uint16_parser),
        h_field("quantity", uint16_parser),
        h_field("data", data_parser)
    );
    return pdu_parser;
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

  HParseResult result = h_parse(&modbus_pdu_parser(), buffer, fsize);

  if (result.status == H_SUCCESS) {
    ModbusPDU *pdu = (ModbusPDU *)result.value;
    printf("Function Code: 0x%02X\n", pdu->function_code);
    printf("Starting Address: 0x%04X\n", pdu->starting_address);
    printf("Quantity: %u\n", pdu->quantity);
    printf("Data: ");
    for (size_t i = 0; i < result.length - 5; i++) {
      printf("0x%02X ", pdu->data[i]);
    }
    printf("\n");
    free(pdu);
  } else {
    fprintf(stderr, "Parsing failed: %s\n", h_error(result.status));
  }

  free(buffer);
  return 0;
}
