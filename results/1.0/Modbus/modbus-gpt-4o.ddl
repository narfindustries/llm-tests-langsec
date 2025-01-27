@format littleendian;

struct ModbusTCPHeader {
  transaction_id: u16;
  protocol_id: u16;
  length: u16;
  unit_id: u8;
}

enum FunctionCode : u8 {
  ReadCoils               = 0x01,
  ReadDiscreteInputs      = 0x02,
  ReadHoldingRegisters    = 0x03,
  ReadInputRegisters      = 0x04,
  WriteSingleCoil         = 0x05,
  WriteSingleRegister     = 0x06,
  Diagnostics             = 0x08,
  GetCommEventCounter     = 0x0B,
  GetCommEventLog         = 0x0C,
  WriteMultipleCoils      = 0x0F,
  WriteMultipleRegisters  = 0x10,
  ReportServerId          = 0x11,
  ReadFileRecord          = 0x14,
  WriteFileRecord         = 0x15,
  MaskWriteRegister       = 0x16,
  ReadWriteMultipleRegs   = 0x17,
  ReadFifoQueue           = 0x18
}

struct ModbusTCPRequest {
  header: ModbusTCPHeader;
  function_code: FunctionCode;
  request_body: u8[];

  @validate() {
    length >= request_body.length + 2; // +2 for function code & unit id
  }
}

struct ModbusTCPResponse {
  header: ModbusTCPHeader;
  function_code: FunctionCode;
  response_body: u8[]; // Response body starts after function code

  @validate() {
    length >= response_body.length + 2; // +2 for function code & unit id
  }
}

@validate() {
  protocol_id == 0x0000;
}

@top
union ModbusTCP {
  request: ModbusTCPRequest [header.function_code == request.function_code];
  response: ModbusTCPResponse [header.function_code == response.function_code];
}
