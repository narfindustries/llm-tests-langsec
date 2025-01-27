#include <hammer/hammer.h>
#include <stdio.h>

HParser* init_modbus_parser(void) {
    // Function codes
    H_RULE(fn_read_coils, h_int_range(h_uint8(), 0x01, 0x01));
    H_RULE(fn_read_discrete_inputs, h_int_range(h_uint8(), 0x02, 0x02));
    H_RULE(fn_read_holding_registers, h_int_range(h_uint8(), 0x03, 0x03));
    H_RULE(fn_read_input_registers, h_int_range(h_uint8(), 0x04, 0x04));
    H_RULE(fn_write_single_coil, h_int_range(h_uint8(), 0x05, 0x05));
    H_RULE(fn_write_single_register, h_int_range(h_uint8(), 0x06, 0x06));
    H_RULE(fn_write_multiple_coils, h_int_range(h_uint8(), 0x0F, 0x0F));
    H_RULE(fn_write_multiple_registers, h_int_range(h_uint8(), 0x10, 0x10));

    // Address fields
    H_RULE(address, h_uint16());
    H_RULE(quantity, h_uint16());
    H_RULE(value, h_uint16());
    H_RULE(byte_count, h_uint8());
    H_RULE(coil_status, h_uint8());
    H_RULE(output_value, h_uint16());
    H_RULE(register_value, h_uint16());

    // Data fields
    H_RULE(coil_data, h_sequence(byte_count, h_many1(h_uint8())));
    H_RULE(register_data, h_sequence(byte_count, h_many1(h_uint16())));

    // Request message formats
    H_RULE(read_coils_req, h_sequence(fn_read_coils, address, quantity));
    H_RULE(read_discrete_inputs_req, h_sequence(fn_read_discrete_inputs, address, quantity));
    H_RULE(read_holding_registers_req, h_sequence(fn_read_holding_registers, address, quantity));
    H_RULE(read_input_registers_req, h_sequence(fn_read_input_registers, address, quantity));
    H_RULE(write_single_coil_req, h_sequence(fn_write_single_coil, address, output_value));
    H_RULE(write_single_register_req, h_sequence(fn_write_single_register, address, register_value));
    H_RULE(write_multiple_coils_req, h_sequence(fn_write_multiple_coils, address, quantity, coil_data));
    H_RULE(write_multiple_registers_req, h_sequence(fn_write_multiple_registers, address, quantity, register_data));

    // Response message formats
    H_RULE(read_coils_resp, h_sequence(fn_read_coils, coil_data));
    H_RULE(read_discrete_inputs_resp, h_sequence(fn_read_discrete_inputs, coil_data));
    H_RULE(read_holding_registers_resp, h_sequence(fn_read_holding_registers, register_data));
    H_RULE(read_input_registers_resp, h_sequence(fn_read_input_registers, register_data));
    H_RULE(write_single_coil_resp, h_sequence(fn_write_single_coil, address, output_value));
    H_RULE(write_single_register_resp, h_sequence(fn_write_single_register, address, register_value));
    H_RULE(write_multiple_coils_resp, h_sequence(fn_write_multiple_coils, address, quantity));
    H_RULE(write_multiple_registers_resp, h_sequence(fn_write_multiple_registers, address, quantity));

    // Main request and response rules
    H_RULE(modbus_request, h_choice(read_coils_req,
                                  read_discrete_inputs_req,
                                  read_holding_registers_req,
                                  read_input_registers_req,
                                  write_single_coil_req,
                                  write_single_register_req,
                                  write_multiple_coils_req,
                                  write_multiple_registers_req));

    H_RULE(modbus_response, h_choice(read_coils_resp,
                                   read_discrete_inputs_resp,
                                   read_holding_registers_resp,
                                   read_input_registers_resp,
                                   write_single_coil_resp,
                                   write_single_register_resp,
                                   write_multiple_coils_resp,
                                   write_multiple_registers_resp));

    // Top-level rule
    H_RULE(modbus_message, h_choice(modbus_request, modbus_response));

    return modbus_message;
}