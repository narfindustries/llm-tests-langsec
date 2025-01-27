module Modbus {
  type U1 = UInt8;
  type U2 = UInt16;
  type U4 = UInt32;

  type ExceptionCode = U1 {
    IllegalFunction       = 0x01,
    IllegalDataAddress    = 0x02,
    IllegalDataValue      = 0x03,
    ServerDeviceFailure   = 0x04,
    Acknowledge           = 0x05,
    ServerDeviceBusy      = 0x06,
    MemoryParityError     = 0x08,
    GatewayPathUnavailable= 0x0A,
    GatewayTargetFailed   = 0x0B
  };

  type FunctionCode = U1 {
    ReadCoils            = 0x01,
    ReadDiscreteInputs   = 0x02,
    ReadHoldingRegisters = 0x03,
    ReadInputRegisters   = 0x04,
    WriteSingleCoil      = 0x05,
    WriteSingleRegister  = 0x06,
    ReadExceptionStatus  = 0x07,
    Diagnostics          = 0x08,
    GetComEventCounter   = 0x0B,
    GetComEventLog       = 0x0C,
    WriteMultipleCoils   = 0x0F,
    WriteMultipleRegisters = 0x10,
    ReportServerID       = 0x11,
    ReadFileRecord       = 0x14,
    WriteFileRecord      = 0x15,
    MaskWriteRegister    = 0x16,
    ReadWriteMultipleRegisters = 0x17,
    ReadFIFOQueue        = 0x18,
    EncapsulatedInterfaceTransport = 0x2B
  };

  type ADU = struct {
    transactionId   : U2;
    protocolId      : U2;
    length          : U2;
    unitId          : U1;
    pdu             : PDU;
  };

  type PDU = union {
    functionCode    : FunctionCode;
    case functionCode {
      FunctionCode.ReadCoils | FunctionCode.ReadDiscreteInputs |
      FunctionCode.ReadHoldingRegisters | FunctionCode.ReadInputRegisters => struct {
        startingAddress : U2;
        quantityOfX     : U2;
      };

      FunctionCode.WriteSingleCoil | FunctionCode.WriteSingleRegister => struct {
        outputAddress   : U2;
        outputValue     : U2;
      };

      FunctionCode.WriteMultipleCoils => struct {
        startingAddress : U2;
        quantityOfOutputs : U2;
        byteCount       : U1;
        outputValues    : bytes(byteCount);
      };

      FunctionCode.WriteMultipleRegisters => struct {
        startingAddress : U2;
        quantityOfRegisters : U2;
        byteCount       : U1;
        registerValues  : bytes(byteCount);
      };

      FunctionCode.ReadExceptionStatus => struct {};

      FunctionCode.Diagnostics => struct {
        subFunction     : U2;
        data            : U2;
      };

      FunctionCode.GetComEventCounter | FunctionCode.GetComEventLog => struct {};

      FunctionCode.ReportServerID => struct {};

      FunctionCode.ReadFileRecord | FunctionCode.WriteFileRecord => struct {
        byteCount       : U1;
        fileRecordData  : bytes(byteCount);
      };

      FunctionCode.MaskWriteRegister => struct {
        referenceAddress: U2;
        andMask          : U2;
        orMask           : U2;
      };

      FunctionCode.ReadWriteMultipleRegisters => struct {
        readStartingAddress : U2;
        quantityToRead      : U2;
        writeStartingAddress: U2;
        quantityToWrite     : U2;
        writeByteCount      : U1;
        writeRegisterValues : bytes(writeByteCount);
      };

      FunctionCode.ReadFIFOQueue => struct {
        fifoPointerAddress : U2;
      };

      FunctionCode.EncapsulatedInterfaceTransport => struct {
        meiType          : U1;
        meiData          : bytes;
      };

      default => struct {
        exceptionCode   : ExceptionCode;
      };
    }
  };
}