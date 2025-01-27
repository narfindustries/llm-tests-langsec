sequence modbus {
    ModbusFrame frames[] = sequence_of(frame, 0..)
}

choice frame {
    request  : request
    response : response
}

sequence request {
    UINT8 transactionId
    UINT16BE protocolId  = 0
    UINT16BE length = $($remaining_bits / 8)$
    UINT8 unitId
    UINT8 functionCode
    Choose<requestBody>(functionCode) body
}

sequence response {
    UINT8 transactionId
    UINT16BE protocolId  = 0
    UINT16BE length = $($remaining_bits / 8)$
    UINT8 unitId
    UINT8 functionCode
    Choose<responseBody>(functionCode) body
}

choice requestBody {
    case 1  : readCoils
    case 2  : readDiscreteInputs  
    case 3  : readHoldingRegisters
    case 4  : readInputRegisters
    case 5  : writeSingleCoil
    case 6  : writeSingleRegister
    case 15 : writeMultipleCoils
    case 16 : writeMultipleRegisters
}

choice responseBody {
    case 1  : readCoilsResponse
    case 2  : readDiscreteInputsResponse
    case 3  : readHoldingRegistersResponse  
    case 4  : readInputRegistersResponse
    case 5  : writeSingleCoilResponse
    case 6  : writeSingleRegisterResponse
    case 15 : writeMultipleCoilsResponse
    case 16 : writeMultipleRegistersResponse
}

sequence readCoils {
    UINT16BE startAddress
    UINT16BE quantity  
}

sequence readDiscreteInputs {
    UINT16BE startAddress
    UINT16BE quantity
}

sequence readHoldingRegisters {
    UINT16BE startAddress
    UINT16BE quantity
}

sequence readInputRegisters {
    UINT16BE startAddress
    UINT16BE quantity
}

sequence writeSingleCoil {
    UINT16BE outputAddress
    UINT16BE value
}

sequence writeSingleRegister {
    UINT16BE address
    UINT16BE value
}

sequence writeMultipleCoils {
    UINT16BE startAddress
    UINT16BE quantity
    UINT8 byteCount
    UINT8 values[byteCount]
}

sequence writeMultipleRegisters {
    UINT16BE startAddress
    UINT16BE quantity
    UINT8 byteCount
    UINT16BE values[quantity]
}

sequence readCoilsResponse {
    UINT8 byteCount
    UINT8 coilStatus[byteCount]
}

sequence readDiscreteInputsResponse {
    UINT8 byteCount
    UINT8 inputStatus[byteCount]
}

sequence readHoldingRegistersResponse {
    UINT8 byteCount
    UINT16BE registerValues[byteCount / 2]
}

sequence readInputRegistersResponse {
    UINT8 byteCount
    UINT16BE inputRegisters[byteCount / 2]
}

sequence writeSingleCoilResponse {
    UINT16BE outputAddress
    UINT16BE value
}

sequence writeSingleRegisterResponse {
    UINT16BE address
    UINT16BE value
}

sequence writeMultipleCoilsResponse {
    UINT16BE startAddress
    UINT16BE quantity
}

sequence writeMultipleRegistersResponse {
    UINT16BE startAddress
    UINT16BE quantity
}