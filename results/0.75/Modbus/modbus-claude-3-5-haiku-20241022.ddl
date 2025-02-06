protocol Modbus {
  enum FunctionCode : u8 {
    ReadCoils = 0x01,
    ReadDiscreteInputs = 0x02,
    ReadHoldingRegisters = 0x03,
    ReadInputRegisters = 0x04,
    WriteSingleCoil = 0x05,
    WriteSingleRegister = 0x06,
    WriteMultipleCoils = 0x0F,
    WriteMultipleRegisters = 0x10
  }

  enum ExceptionCode : u8 {
    IllegalFunction = 0x01,
    IllegalDataAddress = 0x02,
    IllegalDataValue = 0x03,
    ServerDeviceFailure = 0x04,
    Acknowledge = 0x05,
    ServerDeviceBusy = 0x06,
    NegativeAcknowledge = 0x07,
    MemoryParityError = 0x08,
    GatewayPathUnavailable = 0x0A,
    GatewayTargetDeviceFailed = 0x0B
  }

  type ModbusRTUFrame = {
    slave_address: u8 where 0 <= value && value <= 247,
    function_code: FunctionCode,
    payload: match function_code {
      FunctionCode.ReadCoils => ReadCoilsRequest,
      FunctionCode.ReadDiscreteInputs => ReadDiscreteInputsRequest,
      FunctionCode.ReadHoldingRegisters => ReadHoldingRegistersRequest,
      FunctionCode.ReadInputRegisters => ReadInputRegistersRequest,
      FunctionCode.WriteSingleCoil => WriteSingleCoilRequest,
      FunctionCode.WriteSingleRegister => WriteSingleRegisterRequest,
      FunctionCode.WriteMultipleCoils => WriteMultipleCoilsRequest,
      FunctionCode.WriteMultipleRegisters => WriteMultipleRegistersRequest
    },
    crc: u16
  }

  type ModbusTCPFrame = {
    transaction_id: u16,
    protocol_id: u16 = 0,
    length: u16,
    unit_id: u8,
    function_code: FunctionCode,
    payload: match function_code {
      FunctionCode.ReadCoils => ReadCoilsRequest,
      FunctionCode.ReadDiscreteInputs => ReadDiscreteInputsRequest,
      FunctionCode.ReadHoldingRegisters => ReadHoldingRegistersRequest,
      FunctionCode.ReadInputRegisters => ReadInputRegistersRequest,
      FunctionCode.WriteSingleCoil => WriteSingleCoilRequest,
      FunctionCode.WriteSingleRegister => WriteSingleRegisterRequest,
      FunctionCode.WriteMultipleCoils => WriteMultipleCoilsRequest,
      FunctionCode.WriteMultipleRegisters => WriteMultipleRegistersRequest
    }
  }

  type ReadCoilsRequest = {
    start_address: u16,
    quantity: u16 where 1 <= value && value <= 2000
  }

  type ReadDiscreteInputsRequest = {
    start_address: u16,
    quantity: u16 where 1 <= value && value <= 2000
  }

  type ReadHoldingRegistersRequest = {
    start_address: u16,
    quantity: u16 where 1 <= value && value <= 125
  }

  type ReadInputRegistersRequest = {
    start_address: u16,
    quantity: u16 where 1 <= value && value <= 125
  }

  type WriteSingleCoilRequest = {
    address: u16,
    value: u16 where value == 0xFF00 || value == 0x0000
  }

  type WriteSingleRegisterRequest = {
    address: u16,
    value: u16
  }

  type WriteMultipleCoilsRequest = {
    start_address: u16,
    quantity: u16 where 1 <= value && value <= 2000,
    byte_count: u8,
    coil_values: [u8] where length == byte_count
  }

  type WriteMultipleRegistersRequest = {
    start_address: u16,
    quantity: u16 where 1 <= value && value <= 125,
    byte_count: u8,
    register_values: [u16] where length == quantity
  }

  type ExceptionResponse = {
    exception_code: ExceptionCode
  }
}