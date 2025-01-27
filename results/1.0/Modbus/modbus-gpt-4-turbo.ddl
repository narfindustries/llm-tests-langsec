module Modbus {
  -- Define basic data types
  type uint8 = Bits(8)
  type uint16 = Bits(16)
  type uint32 = Bits(32)

  -- Define Modbus function codes
  const ReadCoils = 1
  const ReadDiscreteInputs = 2
  const ReadHoldingRegisters = 3
  const ReadInputRegisters = 4
  const WriteSingleCoil = 5
  const WriteSingleRegister = 6
  const WriteMultipleCoils = 15
  const WriteMultipleRegisters = 16

  -- Define the Modbus Request PDU
  type MBRequestPDU = struct {
    functionCode : uint8,
    data : switch (functionCode) {
      case ReadCoils => ReadCoilsRequest,
      case ReadDiscreteInputs => ReadDiscreteInputsRequest,
      case ReadHoldingRegisters => ReadHoldingRegistersRequest,
      case ReadInputRegisters => ReadInputRegistersRequest,
      case WriteSingleCoil => WriteSingleCoilRequest,
      case WriteSingleRegister => WriteSingleRegisterRequest,
      case WriteMultipleCoils => WriteMultipleCoilsRequest,
      case WriteMultipleRegisters => WriteMultipleRegistersRequest,
      _ => Bytes  -- Default or unknown function code case to raw bytes
    }
  }

  -- Define requests for each Modbus function
  type ReadCoilsRequest = struct {
    startingAddress : uint16,
    quantityOfCoils : uint16
  }
  
  type ReadDiscreteInputsRequest = struct {
    startingAddress : uint16,
    quantityOfInputs : uint16
  }
  
  type ReadHoldingRegistersRequest = struct {
    startingAddress : uint16,
    quantityOfRegisters : uint16
  }
  
  type ReadInputRegistersRequest = struct {
    startingAddress : uint16,
    quantityOfRegisters : uint16
  }
  
  type WriteSingleCoilRequest = struct {
    outputAddress : uint16,
    outputValue : uint16  -- Should be either 0xFF00 or 0x0000
  }
  
  type WriteSingleRegisterRequest = struct {
    registerAddress : uint16,
    registerValue : uint16
  }
  
  type WriteMultipleCoilsRequest = struct {
    startingAddress : uint16,
    quantityOfOutputs : uint16,
    byteCount : uint8,
    outputValue : Bytes(byteCount)
  }
  
  type WriteMultipleRegistersRequest = struct {
    startingAddress : uint16,
    quantityOfRegisters : uint16,
    byteCount : uint8,
    registerValues : Bytes(byteCount)
  }

  -- Define Modbus ADU (Application Data Unit)
  type ModbusADU = struct {
    transactionId : uint16,
    protocolId : uint16,
    length : uint16,
    unitId : uint8,
    pdu : MBRequestPDU
  }
}