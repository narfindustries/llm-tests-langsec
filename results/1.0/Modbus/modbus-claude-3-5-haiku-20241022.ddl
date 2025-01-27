// Modbus Protocol Specification in Daedalus
module Modbus

// Define base structures for Modbus frame
type MBAPHeader {
  transactionId: uint16,
  protocolId: uint16,
  length: uint16
}

type ModbusRequest {
  mbapHeader: MBAPHeader,
  unitId: uint8,
  functionCode: uint8,
  payload: bytes
}

type ModbusResponse {
  mbapHeader: MBAPHeader,
  unitId: uint8,
  functionCode: uint8,
  payload: bytes
}

// Supported Function Codes
enum FunctionCode {
  ReadCoils = 0x01,
  ReadDiscreteInputs = 0x02,
  ReadHoldingRegisters = 0x03,
  ReadInputRegisters = 0x04,
  WriteSingleCoil = 0x05,
  WriteSingleRegister = 0x06,
  WriteMultipleCoils = 0x0F,
  WriteMultipleRegisters = 0x10
}

// Error Codes
enum ErrorCode {
  IllegalFunction = 0x01,
  IllegalDataAddress = 0x02,
  IllegalDataValue = 0x03
}

// Parsing and validation of Modbus frames
parser ModbusParser {
  // MBAP Header parsing
  consume mbapHeader: MBAPHeader

  // Validate protocol
  assert(mbapHeader.protocolId == 0x0000)

  // Parse unit ID and function code
  consume unitId: uint8
  consume functionCode: uint8

  // Validate function code
  assert(functionCode in FunctionCode)

  // Dynamic payload parsing based on function code
  match functionCode {
    FunctionCode.ReadCoils => {
      consume startAddress: uint16
      consume quantityCoils: uint16
      assert(quantityCoils <= 2000)
    },
    FunctionCode.ReadHoldingRegisters => {
      consume startAddress: uint16
      consume quantityRegisters: uint16
      assert(quantityRegisters <= 125)
    },
    // Add more specific parsing for other function codes
    default => {
      consume payload: bytes
    }
  }
}

// Serialization rules
serializer ModbusSerializer {
  // Similar structure to parser with serialization logic
  serialize mbapHeader
  serialize unitId
  serialize functionCode

  // Conditional serialization based on function code
  match functionCode {
    FunctionCode.WriteSingleCoil => {
      serialize outputAddress: uint16
      serialize outputValue: uint16
    },
    // Add more serialization cases
    default => {
      serialize payload
    }
  }
}

// Error handling specification
error_handler ModbusErrorHandler {
  on ErrorCode.IllegalFunction {
    log("Illegal Function Error")
    reject
  }
  on ErrorCode.IllegalDataAddress {
    log("Invalid Data Address")
    reject
  }
}