domain Modbus {
  header {
    uint16 transaction_id;
    uint16 protocol_id;
    uint8  unit_id;
    uint8  function_code;
  }

  payload {
    choice {
      case read_coils {
        uint16 starting_address;
        uint16 quantity;
      }

      case read_discrete_inputs {
        uint16 starting_address;
        uint16 quantity;
      }

      case write_single_coil {
        uint16 output_address;
        uint16 output_value;
      }

      case write_single_register {
        uint16 register_address;
        uint16 register_value;
      }

      case read_holding_registers {
        uint16 starting_address;
        uint16 quantity;
      }

      case write_multiple_coils {
        uint16 starting_address;
        uint16 quantity;
        bytes values;
      }

      case write_multiple_registers {
        uint16 starting_address;
        uint16 quantity;
        bytes values;
      }

      case read_input_registers {
        uint16 starting_address;
        uint16 quantity;
      }
    }
  }

  footer {
    uint16 crc;
  }
}

grammar Modbus {
  header: transaction_id: uint16, protocol_id: uint16, unit_id: uint8, function_code: uint8;
  payload: choice {
    read_coils: starting_address: uint16, quantity: uint16;
    read_discrete_inputs: starting_address: uint16, quantity: uint16;
    write_single_coil: output_address: uint16, output_value: uint16;
    write_single_register: register_address: uint16, register_value: uint16;
    read_holding_registers: starting_address: uint16, quantity: uint16;
    write_multiple_coils: starting_address: uint16, quantity: uint16, values: bytes;
    write_multiple_registers: starting_address: uint16, quantity: uint16, values: bytes;
    read_input_registers: starting_address: uint16, quantity: uint16;
  };
  footer: crc: uint16;
}

type TransactionId = uint16;
type ProtocolId = uint16;
type UnitId = uint8;
type FunctionCode = uint8;
type Address = uint16;
type Quantity = uint16;
type Value = uint16;
type Registrations = bytes;

message ReadCoils {
  TransactionId transaction_id;
  ProtocolId protocol_id;
  UnitId unit_id;
  FunctionCode function_code;
  Address starting_address;
  Quantity quantity;
}

message ReadDiscreteInputs {
  TransactionId transaction_id;
  ProtocolId protocol_id;
  UnitId unit_id;
  FunctionCode function_code;
  Address starting_address;
  Quantity quantity;
}

message WriteSingleCoil {
  TransactionId transaction_id;
  ProtocolId protocol_id;
  UnitId unit_id;
  FunctionCode function_code;
  Address output_address;
  Value output_value;
}

message WriteSingleRegister {
  TransactionId transaction_id;
  ProtocolId protocol_id;
  UnitId unit_id;
  FunctionCode function_code;
  Address register_address;
  Value register_value;
}

message ReadHoldingRegisters {
  TransactionId transaction_id;
  ProtocolId protocol_id;
  UnitId unit_id;
  FunctionCode function_code;
  Address starting_address;
  Quantity quantity;
}

message WriteMultipleCoils {
  TransactionId transaction_id;
  ProtocolId protocol_id;
  UnitId unit_id;
  FunctionCode function_code;
  Address starting_address;
  Quantity quantity;
  Registrations values;
}

message WriteMultipleRegisters {
  TransactionId transaction_id;
  ProtocolId protocol_id;
  UnitId unit_id;
  FunctionCode function_code;
  Address starting_address;
  Quantity quantity;
  Registrations values;
}

message ReadInputRegisters {
  TransactionId transaction_id;
  ProtocolId protocol_id;
  UnitId unit_id;
  FunctionCode function_code;
  Address starting_address;
  Quantity quantity;
}