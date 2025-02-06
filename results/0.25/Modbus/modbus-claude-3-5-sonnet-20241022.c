#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Parser declarations
HParser* mbap_transaction_id;
HParser* mbap_protocol_id;
HParser* mbap_length;
HParser* mbap_unit_id;
HParser* function_code;
HParser* starting_address;
HParser* quantity;
HParser* byte_count;
HParser* output_value;
HParser* register_value;
HParser* read_coils_request;
HParser* read_discrete_inputs_request;
HParser* read_holding_registers_request;
HParser* read_input_registers_request;
HParser* write_single_coil_request;
HParser* write_single_register_request;
HParser* read_coils_response;
HParser* read_discrete_inputs_response;
HParser* read_holding_registers_response;
HParser* read_input_registers_response;
HParser* write_single_coil_response;
HParser* write_single_register_response;
HParser* write_multiple_coils_request;
HParser* write_multiple_coils_response;
HParser* write_multiple_registers_request;
HParser* write_multiple_registers_response;
HParser* mask_write_register_request;
HParser* mask_write_register_response;
HParser* read_write_multiple_registers_request;
HParser* read_write_multiple_registers_response;
HParser* read_fifo_queue_request;
HParser* read_fifo_queue_response;
HParser* exception_response;
HParser* modbus_tcp_frame;
HParser* modbus_rtu_frame;

void init_parsers() {
    mbap_transaction_id = h_uint16();
    mbap_protocol_id = h_uint16();
    mbap_length = h_uint16();
    mbap_unit_id = h_uint8();
    function_code = h_uint8();
    starting_address = h_uint16();
    quantity = h_uint16();
    byte_count = h_uint8();
    output_value = h_uint16();
    register_value = h_uint16();

    read_coils_request = h_sequence(starting_address, quantity, NULL);
    read_discrete_inputs_request = h_sequence(starting_address, quantity, NULL);
    read_holding_registers_request = h_sequence(starting_address, quantity, NULL);
    read_input_registers_request = h_sequence(starting_address, quantity, NULL);
    write_single_coil_request = h_sequence(starting_address, output_value, NULL);
    write_single_register_request = h_sequence(starting_address, register_value, NULL);

    read_coils_response = h_sequence(byte_count, 
                                   h_length_value(byte_count, h_uint8()),
                                   NULL);
    
    read_discrete_inputs_response = h_sequence(byte_count,
                                             h_length_value(byte_count, h_uint8()),
                                             NULL);
    
    read_holding_registers_response = h_sequence(byte_count,
                                               h_length_value(byte_count, h_uint16()),
                                               NULL);
    
    read_input_registers_response = h_sequence(byte_count,
                                             h_length_value(byte_count, h_uint16()),
                                             NULL);
    
    write_single_coil_response = h_sequence(starting_address, output_value, NULL);
    write_single_register_response = h_sequence(starting_address, register_value, NULL);
    
    write_multiple_coils_request = h_sequence(starting_address,
                                            quantity,
                                            byte_count,
                                            h_length_value(byte_count, h_uint8()),
                                            NULL);
    
    write_multiple_coils_response = h_sequence(starting_address, quantity, NULL);
    
    write_multiple_registers_request = h_sequence(starting_address,
                                                quantity,
                                                byte_count,
                                                h_length_value(byte_count, h_uint16()),
                                                NULL);
    
    write_multiple_registers_response = h_sequence(starting_address, quantity, NULL);
    
    mask_write_register_request = h_sequence(starting_address,
                                           h_uint16(),
                                           h_uint16(),
                                           NULL);
    
    mask_write_register_response = mask_write_register_request;
    
    read_write_multiple_registers_request = h_sequence(starting_address,
                                                     quantity,
                                                     starting_address,
                                                     quantity,
                                                     byte_count,
                                                     h_length_value(byte_count, h_uint16()),
                                                     NULL);
    
    read_write_multiple_registers_response = h_sequence(byte_count,
                                                      h_length_value(byte_count, h_uint16()),
                                                      NULL);
    
    read_fifo_queue_request = starting_address;
    read_fifo_queue_response = h_sequence(h_uint16(),
                                        h_uint16(),
                                        h_many(h_uint16()),
                                        NULL);
    
    exception_response = h_sequence(h_uint8(),
                                  h_uint8(),
                                  NULL);
    
    HParser* pdu = h_choice(h_sequence(function_code,
                                     h_choice(read_coils_request,
                                            read_discrete_inputs_request,
                                            read_holding_registers_request,
                                            read_input_registers_request,
                                            write_single_coil_request,
                                            write_single_register_request,
                                            write_multiple_coils_request,
                                            write_multiple_registers_request,
                                            mask_write_register_request,
                                            read_write_multiple_registers_request,
                                            read_fifo_queue_request,
                                            NULL),
                                     NULL),
                           exception_response,
                           NULL);
    
    modbus_tcp_frame = h_sequence(mbap_transaction_id,
                                mbap_protocol_id,
                                mbap_length,
                                mbap_unit_id,
                                pdu,
                                NULL);
    
    modbus_rtu_frame = h_sequence(mbap_unit_id,
                                 pdu,
                                 h_uint16(),
                                 NULL);
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open input file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t* input = malloc(size);
    if (fread(input, 1, size, f) != size) {
        perror("Failed to read input file");
        fclose(f);
        free(input);
        return 1;
    }
    fclose(f);

    init_parsers();

    HParseResult* result = h_parse(modbus_tcp_frame, input, size);
    if (!result) {
        fprintf(stderr, "Failed to parse Modbus frame\n");
        free(input);
        return 1;
    }

    printf("Successfully parsed Modbus frame\n");

    h_parse_result_free(result);
    free(input);
    return 0;
}