grammar Modbus;

typedef UInt8 = UInt : 8;
typedef UInt16 = UInt : 16;

struct Header {
  unit identifier : UInt8;
  function code : UInt8;
}

struct Data {
  start address : UInt16;
  quantity registers : UInt16;
}

alias ModbusTCPHeader = struct {
  transaction identifier : UInt16;
  protocol identifier : UInt16;
  length : UInt16;
  Header;
};

union ModbusFrame = switch (Bool isTCP) {
  case true : ModbusTCPFrame;
  case false : ModbusRTUFrame;
};

struct ModbusTCPFrame {
  ModbusTCPHeader header;
  Data data;
};

struct ModbusRTUFrame {
  Header header;
  Data data;
  crc : UInt16;
};

let isTCP : Bool = true;
start = ModbusFrame;