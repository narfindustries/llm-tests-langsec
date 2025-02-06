type ModbusADU = struct {
    server_address: u8,
    function_code: u8,
    payload: variant(function_code) {
        0x01 => ReadCoilsRequest {
            start_address: be_u16,
            quantity_of_coils: be_u16
        },
        0x02 => ReadDiscreteInputsRequest {
            start_address: be_u16,
            quantity_of_inputs: be_u16
        },
        0x03 => ReadHoldingRegistersRequest {
            start_address: be_u16,
            quantity_of_registers: be_u16
        },
        0x04 => ReadInputRegistersRequest {
            start_address: be_u16,
            quantity_of_registers: be_u16
        },
        0x05 => WriteSingleCoilRequest {
            coil_address: be_u16,
            coil_value: be_u16
        },
        0x06 => WriteSingleRegisterRequest {
            register_address: be_u16,
            register_value: be_u16
        },
        0x0F => WriteMultipleCoilsRequest {
            start_address: be_u16,
            quantity_of_coils: be_u16,
            byte_count: u8,
            coil_values: array(byte_count, u8)
        },
        0x10 => WriteMultipleRegistersRequest {
            start_address: be_u16,
            quantity_of_registers: be_u16,
            byte_count: u8,
            register_values: array(byte_count / 2, be_u16)
        },
        0x17 => ReadWriteMultipleRegistersRequest {
            read_start_address: be_u16,
            quantity_to_read: be_u16,
            write_start_address: be_u16,
            quantity_to_write: be_u16,
            byte_count: u8,
            write_register_values: array(byte_count / 2, be_u16)
        },
        _ => ErrorResponse {
            exception_code: u8
        }
    },
    crc: le_u16
}