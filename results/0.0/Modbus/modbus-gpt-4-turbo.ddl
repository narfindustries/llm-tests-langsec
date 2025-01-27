module Modbus {
  type Word = U16 : little;
  type Byte = U8;

  type ExceptionCode = U8 {
    IllegalFunction       = 0x01,
    IllegalDataAddress    = 0x02,
    IllegalDataValue      = 0x03,
    ServerDeviceFailure   = 0x04,
    Acknowledge           = 0x05,
    ServerDeviceBusy      = 0x06,
    MemoryParityError     = 0x08,
    GatewayPathUnavailable = 0x0A,
    GatewayTargetFailedToRespond = 0x0B
  };

  type FunctionCode = U8 {
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
  };

  type MBAPHeader = struct {
    transactionId : Word;
    protocolId    : Word;
    length        : Word;
    unitId        : Byte;
  };

  type RequestPDU = struct {
    functionCode : FunctionCode;
    data         : bytes @length(this._parent.length - 2);
  };

  type ResponsePDU = struct {
    functionCode : FunctionCode;
    data         : bytes @length(this._parent.length - 2);
  };

  type ExceptionPDU = struct {
    functionCode : FunctionCode;
    exceptionCode: ExceptionCode;
  };

  type ADU = struct {
    header : MBAPHeader;
    pdu    : select(this.header.protocolId) {
      0 -> select(this.header.unitId) {
        0 -> ExceptionPDU,
        _ -> select(this.pdu.functionCode) {
          FunctionCode.ReadCoils | FunctionCode.ReadDiscreteInputs | FunctionCode.ReadHoldingRegisters | FunctionCode.ReadInputRegisters -> ResponsePDU,
          FunctionCode.WriteSingleCoil | FunctionCode.WriteSingleRegister | FunctionCode.WriteMultipleCoils | FunctionCode.WriteMultipleRegisters | FunctionCode.ReadExceptionStatus | FunctionCode.Diagnostics | FunctionCode.GetComEventCounter | FunctionCode.GetComEventLog | FunctionCode.ReportServerID | FunctionCode.ReadFileRecord | FunctionCode.WriteFileRecord | FunctionCode.MaskWriteRegister | FunctionCode.ReadWriteMultipleRegisters | FunctionCode.ReadFIFOQueue | FunctionCode.EncapsulatedInterfaceTransport -> RequestPDU,
          _ -> ExceptionPDU
        }
      }
    }
  };
}