Modbus = {
  transaction_id: UInt16,
  protocol_id: UInt16,
  length: UInt16,
  unit_id: UInt8,
  function_code: UInt8,
  data: case function_code {
    0x01 => ReadCoilsData,
    0x02 => ReadDiscreteInputsData,
    0x03 => ReadHoldingRegistersData,
    0x04 => ReadInputRegistersData,
    0x05 => WriteSingleCoilData,
    0x06 => WriteSingleRegisterData,
    0x0F => WriteMultipleCoilsData,
    0x10 => WriteMultipleRegistersData,
    0x16 => MaskWriteRegisterData,
    0x17 => ReadWriteMultipleRegistersData,
    0x18 => ReadFIFOQueueData,
    0x2B => EncapsulatedInterfaceTransportData,
    _ => ErrorData
  },
  error_code: optional UInt8,
  checksum: optional UInt16
};

ReadCoilsData = {
  starting_address: UInt16,
  quantity_of_coils: UInt16
};

ReadDiscreteInputsData = {
  starting_address: UInt16,
  quantity_of_inputs: UInt16
};

ReadHoldingRegistersData = {
  starting_address: UInt16,
  quantity_of_registers: UInt16
};

ReadInputRegistersData = {
  starting_address: UInt16,
  quantity_of_registers: UInt16
};

WriteSingleCoilData = {
  output_address: UInt16,
  output_value: UInt16
};

WriteSingleRegisterData = {
  register_address: UInt16,
  register_value: UInt16
};

WriteMultipleCoilsData = {
  starting_address: UInt16,
  quantity_of_outputs: UInt16,
  byte_count: UInt8,
  outputs_value: Bytes[byte_count]
};

WriteMultipleRegistersData = {
  starting_address: UInt16,
  quantity_of_registers: UInt16,
  byte_count: UInt8,
  registers_value: Bytes[byte_count]
};

MaskWriteRegisterData = {
  reference_address: UInt16,
  and_mask: UInt16,
  or_mask: UInt16
};

ReadWriteMultipleRegistersData = {
  read_starting_address: UInt16,
  quantity_to_read: UInt16,
  write_starting_address: UInt16,
  quantity_to_write: UInt16,
  write_byte_count: UInt8,
  write_registers_value: Bytes[write_byte_count]
};

ReadFIFOQueueData = {
  fifo_pointer_address: UInt16
};

EncapsulatedInterfaceTransportData = {
  mei_type: UInt8,
  mei_data: Bytes[length - 1]
};

ErrorData = {
  exception_code: UInt8
};