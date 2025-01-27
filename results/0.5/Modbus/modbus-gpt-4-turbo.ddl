module Modbus {
  // Define basic types
  type U1 = UInt8;
  type U2 = UInt16;
  type U4 = UInt32;

  // Define Modbus function codes
  enum FunctionCode : U1 {
    ReadCoils = 0x01,
    ReadDiscreteInputs = 0x02,
    ReadHoldingRegisters = 0x03,
    ReadInputRegisters = 0x04,
    WriteSingleCoil = 0x05,
    WriteSingleRegister = 0x06,
    WriteMultipleCoils = 0x0F,
    WriteMultipleRegisters = 0x10,
    // Additional function codes can be added here
  }

  // Define Modbus exception codes
  enum ExceptionCode : U1 {
    IllegalFunction = 0x01,
    IllegalDataAddress = 0x02,
    IllegalDataValue = 0x03,
    ServerDeviceFailure = 0x04,
    Acknowledge = 0x05,
    ServerDeviceBusy = 0x06,
    MemoryParityError = 0x08,
    GatewayPathUnavailable = 0x0A,
    GatewayTargetDeviceFailedToRespond = 0x0B
  }

  // Define Modbus PDU (Protocol Data Unit)
  struct PDU {
    functionCode : FunctionCode;
    data : match functionCode {
      FunctionCode.ReadCoils | FunctionCode.ReadDiscreteInputs |
      FunctionCode.ReadHoldingRegisters | FunctionCode.ReadInputRegisters => ReadRequest,
      FunctionCode.WriteSingleCoil | FunctionCode.WriteSingleRegister => WriteSingleRequest,
      FunctionCode.WriteMultipleCoils | FunctionCode.WriteMultipleRegisters => WriteMultipleRequest,
      _ => Bytes // default case for unimplemented or custom function codes
    }
  }

  // Define Modbus ADU (Application Data Unit) for TCP
  struct ADUTcp {
    transactionId : U2;
    protocolId : U2;
    length : U2;
    unitId : U1;
    pdu : PDU;
  }

  // Requests for reading multiple registers or coils
  struct ReadRequest {
    startingAddress : U2;
    quantityOfRegisters : U2;
  }

  // Request for writing a single coil or register
  struct WriteSingleRequest {
    outputAddress : U2;
    outputValue : U2; // For coils, 0xFF00 for ON and 0x0000 for OFF
  }

  // Request for writing multiple coils or registers
  struct WriteMultipleRequest {
    startingAddress : U2;
    quantityOfOutputs : U2;
    byteCount : U1;
    outputValues : Bytes; // Values to be written
  }

  // Exception response structure
  struct ExceptionResponse {
    functionCode : U1; // Function code + 0x80 to indicate an exception
    exceptionCode : ExceptionCode;
  }
}