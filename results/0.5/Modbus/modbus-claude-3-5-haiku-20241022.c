#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Modbus Function Code Parser
static HParser* modbus_function_code() {
    return h_choice(
        h_literal_uint(0x01),  // Read Coils
        h_literal_uint(0x02),  // Read Discrete Inputs
        h_literal_uint(0x03),  // Read Holding Registers
        h_literal_uint(0x04),  // Read Input Registers
        h_literal_uint(0x05),  // Write Single Coil
        h_literal_uint(0x06),  // Write Single Register
        h_literal_uint(0x0F),  // Write Multiple Coils
        h_literal_uint(0x10),  // Write Multiple Registers
        NULL
    );
}

// Modbus Address Parser
static HParser* modbus_address() {
    return h_uint16();
}

// Modbus Quantity Parser
static HParser* modbus_quantity() {
    return h_uint16();
}

// Modbus Data Value Parser
static HParser* modbus_data_value() {
    return h_uint16();
}

// Complete Modbus Request Parser
static HParser* modbus_request_parser() {
    return h_sequence(
        modbus_function_code(),   // Function Code
        modbus_address(),         // Starting Address
        modbus_quantity(),        // Quantity of Registers/Coils
        h_end_p(),
        NULL
    );
}

// Complete Modbus Response Parser
static HParser* modbus_response_parser() {
    return h_sequence(
        modbus_function_code(),   // Function Code
        h_uint8(),                // Byte Count
        h_repeat(modbus_data_value(), 1, 125),  // Data Values
        h_end_p(),
        NULL
    );
}

int main(int argc, char** argv) {
    // Initialize Hammer
    hammer_init();

    // Create parsers
    HParser* request_parser = modbus_request_parser();
    HParser* response_parser = modbus_response_parser();

    // Cleanup
    h_destroy_parser(request_parser);
    h_destroy_parser(response_parser);

    return 0;
}