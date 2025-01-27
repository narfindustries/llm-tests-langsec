ModbusPDU {
  transaction_id: UInt16;
  protocol_id: UInt16;
  length: UInt16;
  unit_id: UInt8;
  function_code: UInt8;
  data: Switch(function_code) {
    1: ReadCoils {
      starting_address: UInt16;
      quantity_of_coils: UInt16;
    };
    2: ReadDiscreteInputs {
      starting_address: UInt16;
      quantity_of_inputs: UInt16;
    };
    3: ReadHoldingRegisters {
      starting_address: UInt16;
      quantity_of_registers: UInt16;
    };
    4: ReadInputRegisters {
      starting_address: UInt16;
      quantity_of_registers: UInt16;
    };
    5: WriteSingleCoil {
      output_address: UInt16;
      output_value: UInt16;
    };
    6: WriteSingleRegister {
      register_address: UInt16;
      register_value: UInt16;
    };
    15: WriteMultipleCoils {
      starting_address: UInt16;
      quantity_of_outputs: UInt16;
      byte_count: UInt8;
      outputs: Bytes(byte_count);
    };
    16: WriteMultipleRegisters {
      starting_address: UInt16;
      quantity_of_registers: UInt16;
      byte_count: UInt8;
      registers: Bytes(byte_count);
    };
    default: UnknownFunction {
      data: Bytes(length - 2);
    };
  };
}