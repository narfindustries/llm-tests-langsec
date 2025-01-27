// Modbus Protocol Specification in Daedalus

struct ModbusPDU {
    function_code: uint8;
    data: switch (function_code) {
        1: ReadCoilsRequest {
            starting_address: uint16;
            quantity_of_coils: uint16;
        };
        2: ReadDiscreteInputsRequest {
            starting_address: uint16;
            quantity_of_inputs: uint16;
        };
        3: ReadHoldingRegistersRequest {
            starting_address: uint16;
            quantity_of_registers: uint16;
        };
        4: ReadInputRegistersRequest {
            starting_address: uint16;
            quantity_of_registers: uint16;
        };
        5: WriteSingleCoilRequest {
            output_address: uint16;
            output_value: uint16;
        };
        6: WriteSingleRegisterRequest {
            register_address: uint16;
            register_value: uint16;
        };
        15: WriteMultipleCoilsRequest {
            starting_address: uint16;
            quantity_of_outputs: uint16;
            byte_count: uint8;
            outputs_value: bytes[byte_count];
        };
        16: WriteMultipleRegistersRequest {
            starting_address: uint16;
            quantity_of_registers: uint16;
            byte_count: uint8;
            registers_value: bytes[byte_count];
        };
        default: UnknownFunction {
            data: bytes[..];
        };
    };
}

struct ModbusADU {
    transaction_id: uint16;
    protocol_id: uint16 = 0x0000;
    length: uint16;
    unit_id: uint8;
    pdu: ModbusPDU;
}

struct ModbusFrame {
    adu: ModbusADU;
    crc: uint16;
}