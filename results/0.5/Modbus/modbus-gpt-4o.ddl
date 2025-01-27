@format little-endian
ModbusMessage {
    transaction_id: u16;
    protocol_id: u16;
    length: u16;
    unit_id: u8;
    function_code: u8;
    data: switch (function_code) {
        0x01 => ReadCoilsRequest,
        0x02 => ReadDiscreteInputsRequest,
        0x03 => ReadHoldingRegistersRequest,
        0x04 => ReadInputRegistersRequest,
        0x05 => WriteSingleCoilRequest,
        0x06 => WriteSingleRegisterRequest,
        0x0F => WriteMultipleCoilsRequest,
        0x10 => WriteMultipleRegistersRequest,
        _: RawData
    };
}

ReadCoilsRequest {
    starting_address: u16;
    quantity_of_coils: u16;
}

ReadDiscreteInputsRequest {
    starting_address: u16;
    quantity_of_inputs: u16;
}

ReadHoldingRegistersRequest {
    starting_address: u16;
    quantity_of_registers: u16;
}

ReadInputRegistersRequest {
    starting_address: u16;
    quantity_of_registers: u16;
}

WriteSingleCoilRequest {
    output_address: u16;
    output_value: u16;
}

WriteSingleRegisterRequest {
    register_address: u16;
    register_value: u16;
}

WriteMultipleCoilsRequest {
    starting_address: u16;
    quantity_of_outputs: u16;
    byte_count: u8;
    output_values: u8[byte_count];
}

WriteMultipleRegistersRequest {
    starting_address: u16;
    quantity_of_registers: u16;
    byte_count: u8;
    register_values: u8[byte_count];
}

RawData {
    bytes: u8[];
}