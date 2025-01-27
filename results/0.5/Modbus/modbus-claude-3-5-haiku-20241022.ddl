type modbus_frame = {
    transaction_id: uint16,
    protocol_id: uint16,
    length: uint16,
    unit_id: uint8,
    function_code: uint8,
    data: list<uint8>
}

enum function_codes {
    READ_COILS = 0x01,
    READ_DISCRETE_INPUTS = 0x02,
    READ_HOLDING_REGISTERS = 0x03,
    READ_INPUT_REGISTERS = 0x04,
    WRITE_SINGLE_COIL = 0x05,
    WRITE_SINGLE_REGISTER = 0x06,
    WRITE_MULTIPLE_COILS = 0x0F,
    WRITE_MULTIPLE_REGISTERS = 0x10
}

parser modbus_parser = {
    transaction_id: uint16,
    protocol_id: uint16,
    length: uint16,
    unit_id: uint8,
    function_code: function_codes,
    data: list<uint8> if length > 6 else []
}

bitfield modbus_error {
    exception_code: uint8 [0:7]
}

invariant valid_modbus_frame(frame: modbus_frame) = {
    frame.protocol_id == 0x0000,
    frame.length <= 256,
    frame.unit_id <= 247
}

transform modbus_request(request: modbus_frame) = {
    transaction_id: request.transaction_id,
    protocol_id: 0x0000,
    length: uint16(6 + len(request.data)),
    unit_id: request.unit_id,
    function_code: request.function_code,
    data: request.data
}