def ModbusTCP = {
    TransactionId: uint16,
    ProtocolId: uint16,
    Length: uint16,
    UnitId: uint8,
    FunctionCode: uint8,
    Data: ModbusData(FunctionCode)
}

def ModbusRTU = {
    Address: uint8,
    FunctionCode: uint8,
    Data: ModbusData(FunctionCode),
    CRC: uint16
}

def ModbusData(fc: uint8) = select {
    fc == 0x01: ReadCoilsRequest,
    fc == 0x02: ReadDiscreteInputsRequest,
    fc == 0x03: ReadHoldingRegistersRequest,
    fc == 0x04: ReadInputRegistersRequest,
    fc == 0x05: WriteSingleCoilRequest,
    fc == 0x06: WriteSingleRegisterRequest,
    fc == 0x0F: WriteMultipleCoilsRequest,
    fc == 0x10: WriteMultipleRegistersRequest,
    fc == 0x14: ReadFileRecordRequest,
    fc == 0x15: WriteFileRecordRequest,
    fc == 0x16: MaskWriteRegisterRequest,
    fc == 0x17: ReadWriteMultipleRegistersRequest,
    fc == 0x18: ReadFIFOQueueRequest,
    fc >= 0x80: ErrorResponse
}

def ReadCoilsRequest = {
    StartingAddress: uint16,
    QuantityOfCoils: uint16
}

def ReadDiscreteInputsRequest = {
    StartingAddress: uint16,
    QuantityOfInputs: uint16
}

def ReadHoldingRegistersRequest = {
    StartingAddress: uint16,
    QuantityOfRegisters: uint16
}

def ReadInputRegistersRequest = {
    StartingAddress: uint16,
    QuantityOfRegisters: uint16
}

def WriteSingleCoilRequest = {
    OutputAddress: uint16,
    OutputValue: uint16
}

def WriteSingleRegisterRequest = {
    RegisterAddress: uint16,
    RegisterValue: uint16
}

def WriteMultipleCoilsRequest = {
    StartingAddress: uint16,
    QuantityOfOutputs: uint16,
    ByteCount: uint8,
    OutputValues: bytes(ByteCount)
}

def WriteMultipleRegistersRequest = {
    StartingAddress: uint16,
    QuantityOfRegisters: uint16,
    ByteCount: uint8,
    RegisterValues: bytes(ByteCount)
}

def ReadFileRecordRequest = {
    ByteCount: uint8,
    SubRequests: SubRequest[div(ByteCount, 7)]
}

def SubRequest = {
    ReferenceType: uint8,
    FileNumber: uint16,
    RecordNumber: uint16,
    RecordLength: uint16
}

def WriteFileRecordRequest = {
    ByteCount: uint8,
    SubRequests: WriteSubRequest[div(ByteCount, 7)]
}

def WriteSubRequest = {
    ReferenceType: uint8,
    FileNumber: uint16,
    RecordNumber: uint16,
    RecordLength: uint16,
    RegisterData: bytes(mul(RecordLength, 2))
}

def MaskWriteRegisterRequest = {
    ReferenceAddress: uint16,
    AndMask: uint16,
    OrMask: uint16
}

def ReadWriteMultipleRegistersRequest = {
    ReadStartingAddress: uint16,
    QuantityToRead: uint16,
    WriteStartingAddress: uint16,
    QuantityToWrite: uint16,
    WriteByteCount: uint8,
    WriteRegisterValues: bytes(WriteByteCount)
}

def ReadFIFOQueueRequest = {
    FIFOPointerAddress: uint16
}

def ErrorResponse = {
    ExceptionCode: uint8
}

def ReadCoilsResponse = {
    ByteCount: uint8,
    CoilStatus: bytes(ByteCount)
}

def ReadDiscreteInputsResponse = {
    ByteCount: uint8,
    InputStatus: bytes(ByteCount)
}

def ReadHoldingRegistersResponse = {
    ByteCount: uint8,
    RegisterValues: bytes(ByteCount)
}

def ReadInputRegistersResponse = {
    ByteCount: uint8,
    InputRegisters: bytes(ByteCount)
}

def WriteSingleCoilResponse = {
    OutputAddress: uint16,
    OutputValue: uint16
}

def WriteSingleRegisterResponse = {
    RegisterAddress: uint16,
    RegisterValue: uint16
}

def WriteMultipleCoilsResponse = {
    StartingAddress: uint16,
    QuantityOfOutputs: uint16
}

def WriteMultipleRegistersResponse = {
    StartingAddress: uint16,
    QuantityOfRegisters: uint16
}

def ReadFileRecordResponse = {
    ByteCount: uint8,
    Records: bytes(ByteCount)
}

def WriteFileRecordResponse = {
    ByteCount: uint8,
    Records: bytes(ByteCount)
}

def MaskWriteRegisterResponse = {
    ReferenceAddress: uint16,
    AndMask: uint16,
    OrMask: uint16
}

def ReadWriteMultipleRegistersResponse = {
    ByteCount: uint8,
    ReadRegisterValues: bytes(ByteCount)
}

def ReadFIFOQueueResponse = {
    ByteCount: uint16,
    FIFOCount: uint16,
    FIFOValues: bytes(sub(ByteCount, 2))
}