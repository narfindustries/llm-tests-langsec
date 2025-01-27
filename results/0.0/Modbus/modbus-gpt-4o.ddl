module Modbus

type ModbusPDU = 
    | ReadCoilsRequest
    | ReadCoilsResponse
    | WriteSingleCoilRequest
    | WriteSingleCoilResponse
    | WriteMultipleCoilsRequest
    | WriteMultipleCoilsResponse
    | ReadHoldingRegistersRequest
    | ReadHoldingRegistersResponse
    | WriteSingleRegisterRequest
    | WriteSingleRegisterResponse
    | WriteMultipleRegistersRequest
    | WriteMultipleRegistersResponse

type ReadCoilsRequest = struct {
    functionCode: uint8 { assert functionCode == 0x01 }
    startingAddress: uint16
    quantityOfCoils: uint16
}

type ReadCoilsResponse = struct {
    functionCode: uint8 { assert functionCode == 0x01 }
    byteCount: uint8
    coilStatus: bytes(byteCount)
}

type WriteSingleCoilRequest = struct {
    functionCode: uint8 { assert functionCode == 0x05 }
    outputAddress: uint16
    outputValue: uint16
}

type WriteSingleCoilResponse = struct {
    functionCode: uint8 { assert functionCode == 0x05 }
    outputAddress: uint16
    outputValue: uint16
}

type WriteMultipleCoilsRequest = struct {
    functionCode: uint8 { assert functionCode == 0x0F }
    startingAddress: uint16
    quantityOfOutputs: uint16
    byteCount: uint8
    outputValues: bytes(byteCount)
}

type WriteMultipleCoilsResponse = struct {
    functionCode: uint8 { assert functionCode == 0x0F }
    startingAddress: uint16
    quantityOfOutputs: uint16
}

type ReadHoldingRegistersRequest = struct {
    functionCode: uint8 { assert functionCode == 0x03 }
    startingAddress: uint16
    quantityOfRegisters: uint16
}

type ReadHoldingRegistersResponse = struct {
    functionCode: uint8 { assert functionCode == 0x03 }
    byteCount: uint8
    registerValues: bytes(byteCount)
}

type WriteSingleRegisterRequest = struct {
    functionCode: uint8 { assert functionCode == 0x06 }
    registerAddress: uint16
    registerValue: uint16
}

type WriteSingleRegisterResponse = struct {
    functionCode: uint8 { assert functionCode == 0x06 }
    registerAddress: uint16
    registerValue: uint16
}

type WriteMultipleRegistersRequest = struct {
    functionCode: uint8 { assert functionCode == 0x10 }
    startingAddress: uint16
    quantityOfRegisters: uint16
    byteCount: uint8
    registerValues: bytes(byteCount)
}

type WriteMultipleRegistersResponse = struct {
    functionCode: uint8 { assert functionCode == 0x10 }
    startingAddress: uint16
    quantityOfRegisters: uint16
}