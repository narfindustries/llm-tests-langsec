type ModbusMessage = variant {
    read_coils_request: struct {
        slave_address: uint8,
        function_code: uint8 == 0x01,
        starting_address: uint16,
        quantity_of_coils: uint16 where quantity_of_coils >= 1 && quantity_of_coils <= 2000,
        crc: uint16
    },
    read_discrete_inputs_request: struct {
        slave_address: uint8,
        function_code: uint8 == 0x02,
        starting_address: uint16,
        quantity_of_inputs: uint16 where quantity_of_inputs >= 1 && quantity_of_inputs <= 2000,
        crc: uint16
    },
    read_holding_registers_request: struct {
        slave_address: uint8,
        function_code: uint8 == 0x03,
        starting_address: uint16,
        quantity_of_registers: uint16 where quantity_of_registers >= 1 && quantity_of_registers <= 125,
        crc: uint16
    },
    read_input_registers_request: struct {
        slave_address: uint8,
        function_code: uint8 == 0x04,
        starting_address: uint16,
        quantity_of_registers: uint16 where quantity_of_registers >= 1 && quantity_of_registers <= 125,
        crc: uint16
    },
    write_single_coil_request: struct {
        slave_address: uint8,
        function_code: uint8 == 0x05,
        coil_address: uint16,
        coil_value: uint16 where coil_value == 0x0000 || coil_value == 0xFF00,
        crc: uint16
    },
    write_single_register_request: struct {
        slave_address: uint8,
        function_code: uint8 == 0x06,
        register_address: uint16,
        register_value: uint16,
        crc: uint16
    },
    write_multiple_coils_request: struct {
        slave_address: uint8,
        function_code: uint8 == 0x0F,
        starting_address: uint16,
        quantity_of_coils: uint16 where quantity_of_coils >= 1 && quantity_of_coils <= 1968,
        byte_count: uint8,
        coil_values: list<uint8>,
        crc: uint16
    },
    write_multiple_registers_request: struct {
        slave_address: uint8,
        function_code: uint8 == 0x10,
        starting_address: uint16,
        quantity_of_registers: uint16 where quantity_of_registers >= 1 && quantity_of_registers <= 123,
        byte_count: uint8,
        register_values: list<uint16>,
        crc: uint16
    },
    mask_write_register_request: struct {
        slave_address: uint8,
        function_code: uint8 == 0x16,
        reference_address: uint16,
        and_mask: uint16,
        or_mask: uint16,
        crc: uint16
    },
    read_write_multiple_registers_request: struct {
        slave_address: uint8,
        function_code: uint8 == 0x17,
        read_starting_address: uint16,
        quantity_to_read: uint16 where quantity_to_read >= 1 && quantity_to_read <= 125,
        write_starting_address: uint16,
        quantity_to_write: uint16 where quantity_to_write >= 1 && quantity_to_write <= 123,
        write_byte_count: uint8,
        write_register_values: list<uint16>,
        crc: uint16
    },
    error_response: struct {
        slave_address: uint8,
        function_code: uint8,
        exception_code: uint8,
        crc: uint16
    }
}

type ModbusTCPMessage = {
    transaction_id: uint16,
    protocol_id: uint16,
    length: uint16,
    unit_id: uint8,
    message: ModbusMessage
}