modbus_protocol {
    modbus modbus;
}

modbus {
    u8 unit_identifier;
    u8 function_code;
    payload payload;
}

payload {
    switch (parent.function_code) {
        case 0x01, 0x02: {
            read_request_response read_request_response;
        }
        case 0x03, 0x04: {
            read_register_request_response read_register_request_response;
        }
        case 0x05: {
            write_single_coil write_single_coil;
        }
        case 0x06: {
            write_single_register write_single_register;
        }
        case 0x0F: {
            write_multiple_coils write_multiple_coils;
        }
        case 0x10: {
            write_multiple_registers write_multiple_registers;
        }
        default: {
            exception_response exception_response;
        }
    }
}

read_request_response {
    u16 start_address;
    u16 quantity;
    u8 byte_count;
    u8 coil_status[byte_count];
}

read_register_request_response {
    u16 start_address;
    u16 quantity;
    u8 byte_count;
    u16 register_values[byte_count / 2];
}

write_single_coil {
    u16 coil_address;
    u16 coil_value; // 0x0000 for OFF, 0xFF00 for ON
}

write_single_register {
    u16 register_address;
    u16 register_value;
}

write_multiple_coils {
    u16 start_address;
    u16 quantity_of_coils;
    u8 byte_count;
    u8 coil_values[byte_count];
}

write_multiple_registers {
    u16 start_address;
    u16 quantity_of_registers;
    u8 byte_count;
    u16 register_values[byte_count / 2];
}

exception_response {
    u8 function_code_with_msb_set;
    u8 exception_code;
}

modbus_tcp {
    u16 transaction_id;
    u16 protocol_id;
    u16 length;
    modbus modbus;
}