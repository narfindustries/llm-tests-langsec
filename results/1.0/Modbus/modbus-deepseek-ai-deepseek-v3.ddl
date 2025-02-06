MODBUS_PDU = struct {
  function_code: uint8;
  data: switch (function_code) {
    case 0x01, 0x02: struct {
      starting_address: uint16;
      quantity: uint16;
    };
    case 0x03, 0x04: struct {
      starting_address: uint16;
      quantity: uint16;
    };
    case 0x05: struct {
      output_address: uint16;
      output_value: uint16;
    };
    case 0x06: struct {
      register_address: uint16;
      register_value: uint16;
    };
    case 0x0F: struct {
      starting_address: uint16;
      quantity_of_coils: uint16;
      byte_count: uint8;
      values: bytes(size = byte_count);
    };
    case 0x10: struct {
      starting_address: uint16;
      quantity_of_registers: uint16;
      byte_count: uint8;
      values: bytes(size = byte_count);
    };
    case 0x16: struct {
      reference_address: uint16;
      and_mask: uint16;
      or_mask: uint16;
    };
    case 0x17: struct {
      read_starting_address: uint16;
      quantity_to_read: uint16;
      write_starting_address: uint16;
      quantity_to_write: uint16;
      write_byte_count: uint8;
      write_values: bytes(size = write_byte_count);
    };
    case 0x18: struct {
      fifo_pointer_address: uint16;
    };
    case 0x2B: struct {
      mei_type: uint8;
      mei_data: bytes;
    };
    case 0x81..0xAB: struct {
      exception_code: uint8;
    };
  };
};

MODBUS_RTU_ADU = struct {
  slave_id: uint8;
  pdu: MODBUS_PDU;
  crc: uint16;
};

MODBUS_TCP_ADU = struct {
  transaction_identifier: uint16;
  protocol_identifier: uint16;
  length: uint16;
  unit_identifier: uint8;
  pdu: MODBUS_PDU;
};

MODBUS = union {
  rtu: MODBUS_RTU_ADU;
  tcp: MODBUS_TCP_ADU;
};