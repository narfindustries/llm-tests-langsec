#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdint.h>

static HParser* modbus_parser = NULL;

// Function to represent Modbus Function Code
static HParser* parse_function_code() {
    return h_choice(
        h_literal_uint(0x01), // Read Coils
        h_literal_uint(0x02), // Read Discrete Inputs
        h_literal_uint(0x03), // Read Holding Registers
        h_literal_uint(0x04), // Read Input Registers
        h_literal_uint(0x05), // Write Single Coil
        h_literal_uint(0x06), // Write Single Register
        h_literal_uint(0x0F), // Write Multiple Coils
        h_literal_uint(0x10)  // Write Multiple Registers
    );
}

// Function to represent Modbus Address
static HParser* parse_address() {
    return h_range_uint(0, 0xFFFF);
}

// Function to represent Modbus Quantity
static HParser* parse_quantity() {
    return h_range_uint(1, 0x7D);
}

// Function to represent Modbus Data
static HParser* parse_data() {
    return h_repeat_n(h_uint8(), 1, 0xFF);
}

// Complete Modbus frame parser
static HParser* create_modbus_parser() {
    HParser* function_code = parse_function_code();
    HParser* address = parse_address();
    HParser* quantity = parse_quantity();
    HParser* data = parse_data();

    return h_sequence(
        function_code,   // Function Code
        address,         // Starting Address
        quantity,        // Quantity of Registers/Coils
        data,            // Optional Data
        NULL
    );
}

void initialize_modbus_parser() {
    modbus_parser = create_modbus_parser();
}

void cleanup_modbus_parser() {
    h_destroy_parser(modbus_parser);
}