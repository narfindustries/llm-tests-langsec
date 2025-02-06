let FUNCTION_READ_COILS = 0x01
let FUNCTION_READ_DISCRETE_INPUTS = 0x02
let FUNCTION_READ_HOLDING_REGISTERS = 0x03
let FUNCTION_READ_INPUT_REGISTERS = 0x04
let FUNCTION_WRITE_SINGLE_COIL = 0x05
let FUNCTION_WRITE_SINGLE_REGISTER = 0x06
let FUNCTION_WRITE_MULTIPLE_COILS = 0x0F
let FUNCTION_WRITE_MULTIPLE_REGISTERS = 0x10
let FUNCTION_MASK_WRITE_REGISTER = 0x16
let FUNCTION_READ_WRITE_MULTIPLE_REGISTERS = 0x17
let FUNCTION_READ_FIFO_QUEUE = 0x18

def MBAPHeader = {
    transactionId : uint16,
    protocolId : uint16,
    length : uint16,
    unitId : uint8
}

def Function = {
    code : uint8
}

def ReadCoilsRequest = {
    startingAddress : uint16,
    quantityOfCoils : uint16
}

def ReadCoilsResponse = {
    byteCount : uint8,
    coilStatus : bytes(byteCount)
}

def ReadDiscreteInputsRequest = {
    startingAddress : uint16,
    quantityOfInputs : uint16
}

def ReadDiscreteInputsResponse = {
    byteCount : uint8,
    inputStatus : bytes(byteCount)
}

def ReadHoldingRegistersRequest = {
    startingAddress : uint16,
    quantityOfRegisters : uint16
}

def ReadHoldingRegistersResponse = {
    byteCount : uint8,
    registerValues : bytes(byteCount)
}

def ReadInputRegistersRequest = {
    startingAddress : uint16,
    quantityOfRegisters : uint16
}

def ReadInputRegistersResponse = {
    byteCount : uint8,
    registerValues : bytes(byteCount)
}

def WriteSingleCoilRequest = {
    outputAddress : uint16,
    outputValue : uint16
}

def WriteSingleCoilResponse = {
    outputAddress : uint16,
    outputValue : uint16
}

def WriteSingleRegisterRequest = {
    registerAddress : uint16,
    registerValue : uint16
}

def WriteSingleRegisterResponse = {
    registerAddress : uint16,
    registerValue : uint16
}

def WriteMultipleCoilsRequest = {
    startingAddress : uint16,
    quantityOfOutputs : uint16,
    byteCount : uint8,
    outputsValue : bytes(byteCount)
}

def WriteMultipleCoilsResponse = {
    startingAddress : uint16,
    quantityOfOutputs : uint16
}

def WriteMultipleRegistersRequest = {
    startingAddress : uint16,
    quantityOfRegisters : uint16,
    byteCount : uint8,
    registerValues : bytes(byteCount)
}

def WriteMultipleRegistersResponse = {
    startingAddress : uint16,
    quantityOfRegisters : uint16
}

def MaskWriteRegisterRequest = {
    referenceAddress : uint16,
    andMask : uint16,
    orMask : uint16
}

def MaskWriteRegisterResponse = {
    referenceAddress : uint16,
    andMask : uint16,
    orMask : uint16
}

def ReadWriteMultipleRegistersRequest = {
    readStartingAddress : uint16,
    quantityToRead : uint16,
    writeStartingAddress : uint16,
    quantityToWrite : uint16,
    writeByteCount : uint8,
    writeRegistersValue : bytes(writeByteCount)
}

def ReadWriteMultipleRegistersResponse = {
    byteCount : uint8,
    readRegistersValue : bytes(byteCount)
}

def ReadFIFOQueueRequest = {
    fifoPointerAddress : uint16
}

def ReadFIFOQueueResponse = {
    byteCount : uint16,
    fifoCount : uint16,
    fifoValueRegister : bytes(fifoCount * 2)
}

def ExceptionResponse = {
    errorFunctionCode : uint8,
    exceptionCode : uint8
}

def ModbusPacket = {
    header : MBAPHeader,
    function : Function,
    data = switch(function.code) {
        case FUNCTION_READ_COILS: ReadCoilsRequest
        case FUNCTION_READ_DISCRETE_INPUTS: ReadDiscreteInputsRequest
        case FUNCTION_READ_HOLDING_REGISTERS: ReadHoldingRegistersRequest
        case FUNCTION_READ_INPUT_REGISTERS: ReadInputRegistersRequest
        case FUNCTION_WRITE_SINGLE_COIL: WriteSingleCoilRequest
        case FUNCTION_WRITE_SINGLE_REGISTER: WriteSingleRegisterRequest
        case FUNCTION_WRITE_MULTIPLE_COILS: WriteMultipleCoilsRequest
        case FUNCTION_WRITE_MULTIPLE_REGISTERS: WriteMultipleRegistersRequest
        case FUNCTION_MASK_WRITE_REGISTER: MaskWriteRegisterRequest
        case FUNCTION_READ_WRITE_MULTIPLE_REGISTERS: ReadWriteMultipleRegistersRequest
        case FUNCTION_READ_FIFO_QUEUE: ReadFIFOQueueRequest
        default: ExceptionResponse if function.code >= 0x80
    }
}