// Modbus Protocol Specification in Daedalus
// This specification defines the structure of Modbus TCP and RTU frames.

import Daedalus

// Modbus TCP Header
struct ModbusTCPHeader {
    transaction_id: uint16;
    protocol_id: uint16;
    length: uint16;
    unit_id: uint8;
}

// Modbus RTU Header
struct ModbusRTUHeader {
    address: uint8;
    function_code: uint8;
}

// Modbus PDU (Protocol Data Unit)
struct ModbusPDU {
    function_code: uint8;
    data: bytes;
}

// Modbus TCP Frame
struct ModbusTCPFrame {
    header: ModbusTCPHeader;
    pdu: ModbusPDU;
}

// Modbus RTU Frame
struct ModbusRTUFrame {
    header: ModbusRTUHeader;
    pdu: ModbusPDU;
    crc: uint16;
}

// Modbus Function Codes
enum ModbusFunctionCode : uint8 {
    ReadCoils = 0x01;
    ReadDiscreteInputs = 0x02;
    ReadHoldingRegisters = 0x03;
    ReadInputRegisters = 0x04;
    WriteSingleCoil = 0x05;
    WriteSingleRegister = 0x06;
    WriteMultipleCoils = 0x0F;
    WriteMultipleRegisters = 0x10;
}

// Modbus Exception Codes
enum ModbusExceptionCode : uint8 {
    IllegalFunction = 0x01;
    IllegalDataAddress = 0x02;
    IllegalDataValue = 0x03;
    ServerDeviceFailure = 0x04;
    Acknowledge = 0x05;
    ServerDeviceBusy = 0x06;
    MemoryParityError = 0x08;
    GatewayPathUnavailable = 0x0A;
    GatewayTargetDeviceFailedToRespond = 0x0B;
}

// Modbus Exception Response
struct ModbusExceptionResponse {
    function_code: uint8;
    exception_code: ModbusExceptionCode;
}

// Modbus Request Frame
union ModbusRequestFrame {
    ModbusTCPFrame tcp_frame;
    ModbusRTUFrame rtu_frame;
}

// Modbus Response Frame
union ModbusResponseFrame {
    ModbusTCPFrame tcp_frame;
    ModbusRTUFrame rtu_frame;
    ModbusExceptionResponse exception_response;
}

// Modbus Protocol
struct ModbusProtocol {
    request: ModbusRequestFrame;
    response: ModbusResponseFrame;
}