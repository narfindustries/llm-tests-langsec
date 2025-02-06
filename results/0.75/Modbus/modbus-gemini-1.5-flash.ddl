module Modbus {
  type TransactionId = UInt16;
  type ProtocolId = UInt16;
  type Length = UInt16;
  type UnitIdentifier = UInt8;
  type FunctionCode = UInt8;
  type ExceptionCode = UInt8;

  type ReadCoilsData = { coilStatus: [Bool] };
  type ReadDiscreteInputsData = { inputStatus: [Bool] };
  type ReadHoldingRegistersData = { registerValues: [UInt16] };
  type ReadInputRegistersData = { registerValues: [UInt16] };
  type WriteSingleCoilData = { coilAddress: UInt16, coilValue: Bool };
  type WriteSingleRegisterData = { registerAddress: UInt16, registerValue: UInt16 };
  type WriteMultipleCoilsData = { startingAddress: UInt16, quantityOfCoils: UInt16, byteCount: UInt8, coilData: [Bool] };
  type WriteMultipleRegistersData = { startingAddress: UInt16, quantityOfRegisters: UInt16, byteCount: UInt8, registerData: [UInt16] };
  type ReadWriteMultipleRegistersData = { readStartingAddress: UInt16, readQuantityOfRegisters: UInt16, writeStartingAddress: UInt16, writeQuantityOfRegisters: UInt16, byteCount: UInt8, writeRegisterData: [UInt16], readRegisterData: [UInt16] };
  type MaskWriteRegisterData = { registerAddress: UInt16, andMask: UInt16, orMask: UInt16 };
  type ReadFIFOQueueData = { fifoQueue: [UInt16] };
  type ReadExceptionStatusData = { exceptionStatus: [UInt16] };


  type Data = {
      ReadCoils: ReadCoilsData,
      ReadDiscreteInputs: ReadDiscreteInputsData,
      ReadHoldingRegisters: ReadHoldingRegistersData,
      ReadInputRegisters: ReadInputRegistersData,
      WriteSingleCoil: WriteSingleCoilData,
      WriteSingleRegister: WriteSingleRegisterData,
      WriteMultipleCoils: WriteMultipleCoilsData,
      WriteMultipleRegisters: WriteMultipleRegistersData,
      ReadWriteMultipleRegisters: ReadWriteMultipleRegistersData,
      MaskWriteRegister: MaskWriteRegisterData,
      ReadFIFOQueue: ReadFIFOQueueData,
      ReadExceptionStatus: ReadExceptionStatusData,
      Default: {}
  };

  type Request = {
    transactionId: TransactionId,
    protocolId: ProtocolId,
    length: Length,
    unitIdentifier: UnitIdentifier,
    functionCode: FunctionCode,
    data: Data
  };

  type Response = {
    transactionId: TransactionId,
    protocolId: ProtocolId,
    length: Length,
    unitIdentifier: UnitIdentifier,
    functionCode: FunctionCode,
    exceptionCode: Maybe ExceptionCode,
    data: Data
  };
}
