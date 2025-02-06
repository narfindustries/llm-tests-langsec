modbus = {
    header: {
        transactionID: uint16,
        protocolID: uint16,
        length: uint16,
        unitID: uint8
    },
    pdu: {
        functionCode: uint8,
        data: switch (functionCode) {
            case 1: readCoils,
            case 2: readDiscreteInputs,
            case 3: readHoldingRegisters,
            case 4: readInputRegisters,
            case 5: writeSingleCoil,
            case 6: writeSingleRegister,
            case 7: readExceptionStatus,
            case 8: diagnostics,
            case 11: getCommEventCounter,
            case 12: getCommEventLog,
            case 15: writeMultipleCoils,
            case 16: writeMultipleRegisters,
            case 17: reportServerID,
            case 20: readFileRecord,
            case 21: writeFileRecord,
            case 22: maskWriteRegister,
            case 23: readWriteMultipleRegisters,
            case 24: readFIFOQueue,
            case 43: encapsulatedInterfaceTransport,
            default: exceptionResponse
        }
    }
};

readCoils = {
    startingAddress: uint16,
    quantityOfCoils: uint16
};

readDiscreteInputs = {
    startingAddress: uint16,
    quantityOfInputs: uint16
};

readHoldingRegisters = {
    startingAddress: uint16,
    quantityOfRegisters: uint16
};

readInputRegisters = {
    startingAddress: uint16,
    quantityOfRegisters: uint16
};

writeSingleCoil = {
    outputAddress: uint16,
    outputValue: uint16
};

writeSingleRegister = {
    registerAddress: uint16,
    registerValue: uint16
};

readExceptionStatus = {};

diagnostics = {
    subFunctionCode: uint16,
    data: bytes
};

getCommEventCounter = {};

getCommEventLog = {};

writeMultipleCoils = {
    startingAddress: uint16,
    quantityOfCoils: uint16,
    byteCount: uint8,
    coilsValue: bytes
};

writeMultipleRegisters = {
    startingAddress: uint16,
    quantityOfRegisters: uint16,
    byteCount: uint8,
    registersValue: bytes
};

reportServerID = {};

readFileRecord = {
    referenceType: uint8,
    fileNumber: uint16,
    recordNumber: uint16,
    recordLength: uint16
};

writeFileRecord = {
    referenceType: uint8,
    fileNumber: uint16,
    recordNumber: uint16,
    recordLength: uint16,
    data: bytes
};

maskWriteRegister = {
    referenceAddress: uint16,
    andMask: uint16,
    orMask: uint16
};

readWriteMultipleRegisters = {
    readStartingAddress: uint16,
    quantityToRead: uint16,
    writeStartingAddress: uint16,
    quantityToWrite: uint16,
    writeByteCount: uint8,
    writeRegistersValue: bytes
};

readFIFOQueue = {
    fifoPointerAddress: uint16
};

encapsulatedInterfaceTransport = {
    meiType: uint8,
    data: bytes
};

exceptionResponse = {
    exceptionFunctionCode: uint8,
    exceptionCode: uint8
};