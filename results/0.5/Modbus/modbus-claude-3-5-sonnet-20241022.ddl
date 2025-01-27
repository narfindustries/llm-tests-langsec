format Modbus {
  uint16BE = byte[2]
  uint8 = byte

  TransactionId = uint16BE
  ProtocolId = uint16BE
  Length = uint16BE
  UnitId = uint8
  FunctionCode = uint8
  
  ModbusADU = {
    transactionId : TransactionId
    protocolId : ProtocolId 
    length : Length
    unitId : UnitId
    pdu : ModbusPDU(length - 1)
  }

  ModbusPDU (pduLen: uint16) = {
    functionCode : FunctionCode
    payload : ModbusPayload(functionCode, pduLen - 1)
  }

  ModbusPayload (fc: uint8, payloadLen: uint16) = 
    | FC_READ_COILS                     if fc == 1  => ReadCoilsReq
    | FC_READ_DISCRETE_INPUTS           if fc == 2  => ReadDiscreteInputsReq  
    | FC_READ_HOLDING_REGISTERS         if fc == 3  => ReadHoldingRegistersReq
    | FC_READ_INPUT_REGISTERS          if fc == 4  => ReadInputRegistersReq
    | FC_WRITE_SINGLE_COIL             if fc == 5  => WriteSingleCoilReq
    | FC_WRITE_SINGLE_REGISTER         if fc == 6  => WriteSingleRegisterReq
    | FC_WRITE_MULTIPLE_COILS          if fc == 15 => WriteMultipleCoilsReq
    | FC_WRITE_MULTIPLE_REGISTERS      if fc == 16 => WriteMultipleRegistersReq

  ReadCoilsReq = {
    startingAddr : uint16BE
    quantityOfCoils : uint16BE
  }

  ReadDiscreteInputsReq = {
    startingAddr : uint16BE  
    quantityOfInputs : uint16BE
  }

  ReadHoldingRegistersReq = {
    startingAddr : uint16BE
    quantityOfRegisters : uint16BE
  }

  ReadInputRegistersReq = {
    startingAddr : uint16BE
    quantityOfRegisters : uint16BE
  }

  WriteSingleCoilReq = {
    outputAddr : uint16BE
    outputValue : uint16BE
  }

  WriteSingleRegisterReq = {
    registerAddr : uint16BE
    registerValue : uint16BE
  }

  WriteMultipleCoilsReq = {
    startingAddr : uint16BE
    quantityOfOutputs : uint16BE
    byteCount : uint8
    outputValues : byte[byteCount]
  }

  WriteMultipleRegistersReq = {
    startingAddr : uint16BE
    quantityOfRegisters : uint16BE
    byteCount : uint8
    registerValues : byte[byteCount]
  }

  Main = ModbusADU[]
}

def FC_READ_COILS = 1
def FC_READ_DISCRETE_INPUTS = 2  
def FC_READ_HOLDING_REGISTERS = 3
def FC_READ_INPUT_REGISTERS = 4
def FC_WRITE_SINGLE_COIL = 5
def FC_WRITE_SINGLE_REGISTER = 6
def FC_WRITE_MULTIPLE_COILS = 15
def FC_WRITE_MULTIPLE_REGISTERS = 16