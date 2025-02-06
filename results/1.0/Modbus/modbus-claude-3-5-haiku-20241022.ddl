type MbapHeader {
    transaction_id: uint16;
    protocol_id: uint16 = 0x0000;
    length: uint16;
    unit_id: uint8;
}

enum FunctionCode : uint8 {
    ReadCoils = 0x01,
    ReadDiscreteInputs = 0x02,
    ReadHoldingRegisters = 0x03,
    ReadInputRegisters = 0x04,
    WriteSingleCoil = 0x05,
    WriteSingleRegister = 0x06,
    WriteMultipleCoils = 0x0F,
    WriteMultipleRegisters = 0x10
}

enum ExceptionCode : uint8 {
    IllegalFunction = 0x01,
    IllegalDataAddress = 0x02,
    IllegalDataValue = 0x03,
    SlaveDeviceFailure = 0x04
}

type ModbusRequest {
    function_code: FunctionCode;
    data: match function_code {
        FunctionCode.ReadCoils => {
            start_address: uint16;
            quantity: uint16 where quantity >= 1 && quantity <= 2000
        },
        FunctionCode.ReadDiscreteInputs => {
            start_address: uint16;
            quantity: uint16 where quantity >= 1 && quantity <= 2000
        },
        FunctionCode.ReadHoldingRegisters => {
            start_address: uint16;
            quantity: uint16 where quantity >= 1 && quantity <= 125
        },
        FunctionCode.ReadInputRegisters => {
            start_address: uint16;
            quantity: uint16 where quantity >= 1 && quantity <= 125
        },
        FunctionCode.WriteSingleCoil => {
            address: uint16;
            value: uint16 where value == 0x0000 || value == 0xFF00
        },
        FunctionCode.WriteSingleRegister => {
            address: uint16;
            value: uint16
        },
        FunctionCode.WriteMultipleCoils => {
            start_address: uint16;
            quantity: uint16 where quantity >= 1 && quantity <= 2000;
            byte_count: uint8;
            coil_values: uint8[byte_count]
        },
        FunctionCode.WriteMultipleRegisters => {
            start_address: uint16;
            quantity: uint16 where quantity >= 1 && quantity <= 123;
            byte_count: uint8;
            register_values: uint8[byte_count]
        }
    }
}

type ModbusResponse {
    is_error: bool;
    data: match is_error {
        true => {
            exception_code: ExceptionCode
        },
        false => {
            function_code: FunctionCode;
            response_data: uint8[]
        }
    }
}

type ModbusMessage {
    header: MbapHeader;
    request: ModbusRequest;
    response: ModbusResponse?
}