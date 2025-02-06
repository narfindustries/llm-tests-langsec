def ModbusTCP = sequence {
    transaction_id: uint16
    protocol_id: uint16
    length: uint16
    unit_id: uint8
    function_code: uint8
    data: ModbusData(function_code)
}

def ModbusData(fc: uint8) = sequence {
    if fc in [0x01, 0x02, 0x03, 0x04] {
        starting_address: uint16
        quantity: uint16
    }
    if fc == 0x05 {
        output_address: uint16
        output_value: uint16
    }
    if fc == 0x06 {
        register_address: uint16
        register_value: uint16
    }
    if fc == 0x0F {
        starting_address: uint16
        quantity: uint16
        byte_count: uint8
        coil_values: byte[byte_count]
    }
    if fc == 0x10 {
        starting_address: uint16
        quantity: uint16
        byte_count: uint8
        register_values: byte[byte_count]
    }
    if fc == 0x14 {
        byte_count: uint8
        reference_type: uint8
        file_number: uint16
        record_number: uint16
        record_length: uint16
    }
    if fc == 0x15 {
        byte_count: uint8
        reference_type: uint8
        file_number: uint16
        record_number: uint16
        record_length: uint16
        record_data: byte[record_length * 2]
    }
    if fc == 0x16 {
        reference_address: uint16
        and_mask: uint16
        or_mask: uint16
    }
    if fc == 0x17 {
        read_starting_address: uint16
        read_quantity: uint16
        write_starting_address: uint16
        write_quantity: uint16
        write_byte_count: uint8
        write_registers: byte[write_byte_count]
    }
    if fc == 0x18 {
        fifo_pointer_address: uint16
    }
    if fc == 0x08 {
        sub_function: uint16
        diagnostic_data: DiagnosticData(sub_function)
    }
    if fc >= 0x80 {
        exception_code: uint8
    }
}

def DiagnosticData(sub_fn: uint16) = sequence {
    if sub_fn == 0x0000 {
        query_data: uint16
    }
    if sub_fn == 0x0001 {
        restart_type: uint16
    }
    if sub_fn > 0x0001 {
        data: uint16
    }
}

def ModbusResponse(fc: uint8) = sequence {
    if fc in [0x01, 0x02] {
        byte_count: uint8
        coil_status: byte[byte_count]
    }
    if fc in [0x03, 0x04] {
        byte_count: uint8
        register_values: byte[byte_count]
    }
    if fc in [0x05, 0x06] {
        output_address: uint16
        output_value: uint16
    }
    if fc in [0x0F, 0x10] {
        starting_address: uint16
        quantity: uint16
    }
    if fc in [0x14, 0x15] {
        byte_count: uint8
        record_data: byte[byte_count]
    }
    if fc == 0x16 {
        reference_address: uint16
        and_mask: uint16
        or_mask: uint16
    }
    if fc == 0x17 {
        byte_count: uint8
        read_registers: byte[byte_count]
    }
    if fc == 0x18 {
        byte_count: uint16
        fifo_count: uint16
        fifo_value_register: byte[fifo_count * 2]
    }
    if fc >= 0x80 {
        exception_code: uint8
    }
}