type MBAPHeader = struct {
  transactionId : uint16 : big;
  protocolId    : uint16 : big;
  length        : uint16 : big;
  unitId        : uint8;
};

type FunctionCode = enum uint8 {
  ReadCoils = 1,
  ReadDiscreteInputs,
  ReadHoldingRegisters,
  ReadInputRegisters,
  WriteSingleCoil = 5,
  WriteSingleRegister,
  WriteMultipleCoils = 15,
  WriteMultipleRegisters,
  ErrorResponse = 0x80
};

type RequestData = choice FunctionCode {
  ReadCoils, ReadDiscreteInputs, ReadHoldingRegisters, ReadInputRegisters => struct {
    startingAddress uint16 : big;
    quantity uint16 : big;
  },
  WriteSingleCoil, WriteSingleRegister => struct {
    outputAddress uint16 : big;
    outputValue uint16 : big;
  },
  WriteMultipleCoils, WriteMultipleRegisters => struct {
    startingAddress uint16 : big;
    quantity uint16 : big;
    values bytes;
  }
};

type ResponseData = choice FunctionCode {
  ReadCoils, ReadDiscreteInputs, ReadHoldingRegisters, ReadInputRegisters => bytes,
  WriteSingleCoil, WriteSingleRegister, WriteMultipleCoils, WriteMultipleRegisters => struct {
    startingAddress uint16 : big;
    quantity uint16 : big;
  },
  ErrorResponse => struct {
    originalFunctionCode uint8;
    exceptionCode uint8;
  }
};

type ModbusPDU = struct {
  functionCode FunctionCode;
  data RequestData | ResponseData;
};

type ModbusADU = struct {
  header MBAPHeader;
  pdu ModbusPDU;
};

root ModbusADU;