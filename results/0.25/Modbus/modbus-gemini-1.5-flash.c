#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

// Define Modbus data types
typedef uint8_t  mb_uint8;
typedef uint16_t mb_uint16;
typedef uint32_t mb_uint32;

//Helper function to create a parser for a specific number of bytes
hammer_parser hammer_bytes_n(size_t n){
    return hammer_map(hammer_take(n),hammer_to_bytes);
}

// Modbus PDU parsers
static hammer_parser mb_slave_address = hammer_uint8;
static hammer_parser mb_function_code = hammer_uint8;
static hammer_parser mb_starting_address = hammer_uint16_be;
static hammer_parser mb_quantity = hammer_uint16_be;
static hammer_parser mb_data = hammer_bytes(0); 
static hammer_parser mb_exception_code = hammer_uint8;


// Function to parse a Modbus PDU based on function code
hammer_parser mb_pdu_parser(hammer_result func_code_result) {
    mb_uint8 function_code = func_code_result.value;
    switch (function_code) {
        case 0x01: // Read Coils
        case 0x02: // Read Discrete Inputs
            return hammer_seq(mb_starting_address, mb_quantity, hammer_bytes_n((mb_quantity.value + 7) / 8), hammer_end);
        case 0x03: // Read Holding Registers
        case 0x04: // Read Input Registers
            return hammer_seq(mb_starting_address, mb_quantity, hammer_bytes_n(mb_quantity.value * 2), hammer_end);
        case 0x05: // Write Single Coil
            return hammer_seq(mb_starting_address, hammer_uint16_be, hammer_end);
        case 0x06: // Write Single Register
            return hammer_seq(mb_starting_address, hammer_uint16_be, hammer_end);
        case 0x0F: // Write Multiple Coils
            return hammer_seq(mb_starting_address, mb_quantity, hammer_bytes_n((mb_quantity.value + 7) / 8), hammer_end);
        case 0x10: // Write Multiple Registers
            return hammer_seq(mb_starting_address, mb_quantity, hammer_bytes_n(mb_quantity.value * 2), hammer_end);
        case 0x17: // Read/Write Multiple Registers
            return hammer_seq(mb_starting_address, mb_quantity, hammer_bytes_n(mb_quantity.value * 2), hammer_end);
        case 0x18: // Read File Record
            return hammer_seq(mb_starting_address, mb_quantity, hammer_bytes_n(mb_quantity.value * 2), hammer_end);
        case 0x14: // Read Device Identification
            return hammer_seq(mb_starting_address, mb_quantity, hammer_bytes_n(mb_quantity.value), hammer_end);
        default: // Exception handling for unknown function codes
            return hammer_fail;
    }
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
    fread(buffer, 1, fsize, fp);
    fclose(fp);

    hammer_parser parser = hammer_seq(mb_slave_address, mb_function_code, hammer_bind(mb_pdu_parser,mb_function_code));
    hammer_result result = hammer_parse(parser, buffer, fsize);

    if (result.success) {
        printf("Parsing successful!\n");
        // Access parsed data here.  
    } else {
        fprintf(stderr, "Parsing failed at offset %zu: %s\n", result.offset, result.error);
    }

    free(buffer);
    return 0;
}

