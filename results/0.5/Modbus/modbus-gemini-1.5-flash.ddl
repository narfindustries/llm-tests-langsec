module Modbus {
  type ModbusFrame = {
    header: ModbusHeader,
    pdu: ModbusPDU
  }

  type ModbusHeader = {
    transactionId: UInt16,
    protocolId: UInt16,
    length: UInt16,
    unitId: UInt8
  }

  type ModbusPDU = {
    functionCode: UInt8,
    data: Data
  }

  type Data = Choice {
    ReadCoils: { startAddress: UInt16, quantity: UInt16, byteCount: UInt8, coilStatus: Byte[] },
    ReadDiscreteInputs: { startAddress: UInt16, quantity: UInt16, byteCount: UInt8, inputStatus: Byte[] },
    ReadHoldingRegisters: { startAddress: UInt16, quantity: UInt16, byteCount: UInt8, registers: UInt16[] },
    ReadInputRegisters: { startAddress: UInt16, quantity: UInt16, byteCount: UInt8, registers: UInt16[] },
    WriteSingleCoil: { startAddress: UInt16, value: Boolean },
    WriteSingleRegister: { startAddress: UInt16, value: UInt16 },
    WriteMultipleCoils: { startAddress: UInt16, quantity: UInt16, byteCount: UInt8, coilStatus: Byte[] },
    WriteMultipleRegisters: { startAddress: UInt16, quantity: UInt16, byteCount: UInt8, registers: UInt16[] },
    ReportServerDiagnostics: { subFunctionCode: UInt8, diagnosticData: Byte[] },
    ReadExceptionStatus: { exceptionStatus: UInt16 },
    DiagnosticRequest: { subFunctionCode: UInt8, data: Byte[] },
    GetComEventCounter: { eventCounter: UInt32 },
    GetComEventLog: { eventCounter: UInt32, eventLog: Byte[] },
    ReadDeviceIdentification: { objectId: UInt16, data: Byte[] },
    ExceptionResponse: { exceptionCode: UInt8 }
  }

  type Byte = UInt8
  type UInt8 = UInt8
  type UInt16 = UInt16
  type UInt32 = UInt32
  type Boolean = Boolean
}
