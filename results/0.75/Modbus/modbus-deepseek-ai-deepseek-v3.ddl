Modbus = {
  transaction_id: uint16,
  protocol_id: uint16,
  length: uint16,
  unit_id: uint8,
  pdu: {
    function_code: uint8,
    data: [
      case function_code of
      0x01 => ReadCoils: {
        starting_address: uint16,
        quantity_of_coils: uint16
      },
      0x02 => ReadDiscreteInputs: {
        starting_address: uint16,
        quantity_of_inputs: uint16
      },
      0x03 => ReadHoldingRegisters: {
        starting_address: uint16,
        quantity_of_registers: uint16
      },
      0x04 => ReadInputRegisters: {
        starting_address: uint16,
        quantity_of_registers: uint16
      },
      0x05 => WriteSingleCoil: {
        output_address: uint16,
        output_value: uint16
      },
      0x06 => WriteSingleRegister: {
        register_address: uint16,
        register_value: uint16
      },
      0x07 => ReadExceptionStatus: {
      },
      0x08 => Diagnostics: {
        sub_function_code: uint16,
        data: uint16
      },
      0x0B => GetCommEventCounter: {
      },
      0x0F => WriteMultipleCoils: {
        starting_address: uint16,
        quantity_of_outputs: uint16,
        byte_count: uint8,
        outputs_value: bytes
      },
      0x10 => WriteMultipleRegisters: {
        starting_address: uint16,
        quantity_of_registers: uint16,
        byte_count: uint8,
        registers_value: bytes
      },
      0x14 => ReadFileRecord: {
        byte_count: uint8,
        sub_requests: bytes
      },
      0x15 => WriteFileRecord: {
        byte_count: uint8,
        sub_requests: bytes
      },
      0x16 => MaskWriteRegister: {
        reference_address: uint16,
        and_mask: uint16,
        or_mask: uint16
      },
      0x17 => ReadWriteMultipleRegisters: {
        read_starting_address: uint16,
        quantity_to_read: uint16,
        write_starting_address: uint16,
        quantity_to_write: uint16,
        write_byte_count: uint8,
        write_registers_value: bytes
      },
      0x18 => ReadFIFOQueue: {
        fifo_pointer_address: uint16
      },
      0x2B => EncapsulatedInterfaceTransport: {
        mei_type: uint8,
        mei_data: bytes
      },
      _ => Generic: {
        data: bytes
      }
    ]
  }
}