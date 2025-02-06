module Modbus;

import std.core::*;

type ModbusFunction = struct {
    functionCode : uint8;
    data         : switch (functionCode) {
        case 1  => ReadCoilsRequest;
        case 2  => ReadDiscreteInputsRequest;
        case 3  => ReadHoldingRegistersRequest;
        case 4  => ReadInputRegistersRequest;
        case 5  => WriteSingleCoilRequest;
        case 6  => WriteSingleRegisterRequest;
        case 15 => WriteMultipleCoilsRequest;
        case 16 => WriteMultipleRegistersRequest;
        default => array<uint8>;
    }
};

type ReadCoilsRequest = struct {
    startingAddress : uint16;
    quantityOfCoils : uint16;
};

type ReadDiscreteInputsRequest = struct {
    startingAddress : uint16;
    quantityOfInputs : uint16;
};

type ReadHoldingRegistersRequest = struct {
    startingAddress : uint16;
    quantityOfRegisters : uint16;
};

type ReadInputRegistersRequest = struct {
    startingAddress : uint16;
    quantityOfRegisters : uint16;
};

type WriteSingleCoilRequest = struct {
    outputAddress : uint16;
    outputValue   : uint16; // 0xFF00 for ON and 0x0000 for OFF
};

type WriteSingleRegisterRequest = struct {
    registerAddress : uint16;
    registerValue   : uint16;
};

type WriteMultipleCoilsRequest = struct {
    startingAddress : uint16;
    quantityOfOutputs : uint16;
    byteCount        : uint8;
    outputValues     : array<uint8>;
};

type WriteMultipleRegistersRequest = struct {
    startingAddress : uint16;
    quantityOfRegisters : uint16;
    byteCount           : uint8;
    registerValues      : array<uint8>;
};

type ModbusRTUFrame = struct {
    address  : uint8;
    function : ModbusFunction;
    crc      : uint16;
};

type ModbusTCPFrame = struct {
    transactionId : uint16;
    protocolId    : uint16 = 0x0000;
    length        : uint16;
    unitId        : uint8;
    function      : ModbusFunction;
};

type ModbusMessage = union {
    rtu  : ModbusRTUFrame;
    tcp  : ModbusTCPFrame;
};