module Modbus;

type ModbusADU = struct {
    transactionId : uint16;
    protocolId    : uint16;
    length        : uint16;
    unitId        : uint8;
    pdu           : ModbusPDU;
};

type ModbusPDU = struct {
    functionCode  : uint8;
    data          : bytes(length - 1);
};

type ModbusFunctionCode = enum uint8 {
    ReadCoils = 0x01,
    ReadDiscreteInputs = 0x02,
    ReadHoldingRegisters = 0x03,
    ReadInputRegisters = 0x04,
    WriteSingleCoil = 0x05,
    WriteSingleRegister = 0x06,
    ReadExceptionStatus = 0x07,
    Diagnostics = 0x08,
    GetComEventCounter = 0x0B,
    GetComEventLog = 0x0C,
    WriteMultipleCoils = 0x0F,
    WriteMultipleRegisters = 0x10,
    ReportServerID = 0x11,
    ReadFileRecord = 0x14,
    WriteFileRecord = 0x15,
    MaskWriteRegister = 0x16,
    ReadWriteMultipleRegisters = 0x17,
    ReadFIFOQueue = 0x18,
    EncapsulatedInterfaceTransport = 0x2B
};

type ModbusRequest = struct {
    functionCode : ModbusFunctionCode;
    data         : bytes;
};

type ModbusResponse = struct {
    functionCode : ModbusFunctionCode;
    data         : bytes;
};

type ModbusExceptionResponse = struct {
    functionCode  : uint8;
    exceptionCode : ModbusExceptionCode;
};

type ModbusExceptionCode = enum uint8 {
    IllegalFunction = 0x01,
    IllegalDataAddress = 0x02,
    IllegalDataValue = 0x03,
    ServerDeviceFailure = 0x04,
    Acknowledge = 0x05,
    ServerDeviceBusy = 0x06,
    MemoryParityError = 0x08,
    GatewayPathUnavailable = 0x0A,
    GatewayTargetDeviceFailedToRespond = 0x0B
};