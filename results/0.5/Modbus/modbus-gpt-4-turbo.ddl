type ModbusFrame = struct {
    transactionId : uint16;
    protocolId    : uint16 = 0; // Modbus protocol
    length        : uint16;     // Length includes Unit Identifier, Function Code and Data fields
    unitId        : uint8;
    functionCode  : uint8;
    data          : bytes(length - 2);
};

type ModbusTCPFrame = struct {
    mbapHeader : ModbusFrame;
    payload    : ModbusPDU;
};

type ModbusPDU = union {
    case 1   : ReadCoilsRequest;
    case 2   : ReadDiscreteInputsRequest;
    case 3   : ReadHoldingRegistersRequest;
    case 4   : ReadInputRegistersRequest;
    case 5   : WriteSingleCoilRequest;
    case 6   : WriteSingleRegisterRequest;
    case 15  : WriteMultipleCoilsRequest;
    case 16  : WriteMultipleRegistersRequest;
    default  : UnknownFunction;
} using ModbusTCPFrame.mbapHeader.functionCode;

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
    outputValue : uint16; // 0xFF00 for ON and 0x0000 for OFF
};

type WriteSingleRegisterRequest = struct {
    registerAddress : uint16;
    registerValue : uint16;
};

type WriteMultipleCoilsRequest = struct {
    startingAddress : uint16;
    quantityOfOutputs : uint16;
    byteCount : uint8;
    outputValue : bytes(byteCount);
};

type WriteMultipleRegistersRequest = struct {
    startingAddress : uint16;
    quantityOfRegisters : uint16;
    byteCount : uint8;
    registerValues : bytes(byteCount);
};

type UnknownFunction = struct {
    data : bytes(ModbusTCPFrame.mbapHeader.length - 2);
};