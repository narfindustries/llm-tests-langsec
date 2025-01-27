module Modbus {
  -- Modbus function codes
  enum FunctionCode : uint8 {
    ReadCoils = 0x01,
    ReadDiscreteInputs = 0x02,
    ReadHoldingRegisters = 0x03,
    ReadInputRegisters = 0x04,
    WriteSingleCoil = 0x05,
    WriteSingleRegister = 0x06,
    ReadExceptionStatus = 0x07,
    Diagnostics = 0x08,
    GetComEventCounter = 0x0B,
    GetComEventLog = 0x0C,
    WriteMultipleCoils = 0x0F,
    WriteMultipleRegisters = 0x10,
    ReportServerID = 0x11,
    ReadFileRecord = 0x14,
    WriteFileRecord = 0x15,
    MaskWriteRegister = 0x16,
    ReadWriteMultipleRegisters = 0x17,
    ReadFIFOQueue = 0x18,
    EncapsulatedInterfaceTransport = 0x2B
  }

  type ExceptionCode : uint8 {
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

  type MBAPHeader = struct {
    transactionId : uint16,
    protocolId : uint16,
    length : uint16,
    unitId : uint8
  }

  type Request = struct {
    functionCode : FunctionCode,
    data : uint8[length(this._parent.length) - 2]
  }

  type Response = struct {
    functionCode : FunctionCode,
    data : uint8[length(this._parent.length) - 2]
  }

  type ExceptionResponse = struct {
    functionCode : uint8,  -- Function code + 0x80 to indicate an exception
    exceptionCode : ExceptionCode
  }

  type ADU = struct {
    header : MBAPHeader,
    body : choice(header.length) {
      default = fail("Unsupported function code or length"),
      case (header.length == 5 && body.functionCode == FunctionCode.WriteSingleCoil) = Response,
      case (header.length == 5 && body.functionCode == FunctionCode.WriteSingleRegister) = Response,
      case (body.functionCode == FunctionCode.ReadCoils) = Response,
      case (body.functionCode == FunctionCode.ReadDiscreteInputs) = Response,
      case (body.functionCode == FunctionCode.ReadHoldingRegisters) = Response,
      case (body.functionCode == FunctionCode.ReadInputRegisters) = Response,
      case (body.functionCode == FunctionCode.WriteMultipleCoils) = Response,
      case (body.functionCode == FunctionCode.WriteMultipleRegisters) = Response,
      case (body.functionCode == FunctionCode.ReadFIFOQueue) = Response,
      if (body.functionCode >= 0x80) = ExceptionResponse
    }
  }
}