meta:
  id: modbus
  title: Modbus Protocol
  application: industrial_automation
  license: MIT
  endian: be
seq:
  - id: transaction_id
    type: u2
    doc: "Transaction Identifier (0x0000 to 0xFFFF)"
  - id: protocol_id
    type: u2
    doc: "Protocol Identifier (0x0000 for Modbus)"
  - id: length
    type: u2
    doc: "Length Field (number of bytes following)"
  - id: unit_id
    type: u1
    doc: "Unit Identifier (0x00 to 0xFF)"
  - id: function_code
    type: u1
    doc: "Function Code (0x01 to 0x2B)"
  - id: data
    type:
      switch-on: function_code
      cases:
        0x01: read_coils
        0x02: read_discrete_inputs
        0x03: read_holding_registers
        0x04: read_input_registers
        0x05: write_single_coil
        0x06: write_single_register
        0x07: read_exception_status
        0x08: diagnostics
        0x0B: get_comm_event_counter
        0x0C: get_comm_event_log
        0x0F: write_multiple_coils
        0x10: write_multiple_registers
        0x11: report_server_id
        0x14: read_file_record
        0x15: write_file_record
        0x16: mask_write_register
        0x17: read_write_multiple_registers
        0x18: read_fifo_queue
        0x2B: encapsulated_interface_transport
    doc: "Data Field (variable length, depends on function code)"
  - id: error_code
    type: u1
    if: function_code >= 0x80
    doc: "Error Code (0x01 to 0x0B, only in error responses)"
  - id: exception_code
    type: u1
    if: function_code >= 0x80
    doc: "Exception Code (only in error responses)"
  - id: checksum
    type: u2
    doc: "Checksum (0x0000 to 0xFFFF)"
types:
  read_coils:
    seq:
      - id: starting_address
        type: u2
        doc: "Starting Address"
      - id: quantity
        type: u2
        doc: "Number of Coils"
  read_discrete_inputs:
    seq:
      - id: starting_address
        type: u2
        doc: "Starting Address"
      - id: quantity
        type: u2
        doc: "Number of Discrete Inputs"
  read_holding_registers:
    seq:
      - id: starting_address
        type: u2
        doc: "Starting Address"
      - id: quantity
        type: u2
        doc: "Number of Holding Registers"
  read_input_registers:
    seq:
      - id: starting_address
        type: u2
        doc: "Starting Address"
      - id: quantity
        type: u2
        doc: "Number of Input Registers"
  write_single_coil:
    seq:
      - id: output_address
        type: u2
        doc: "Coil Address"
      - id: output_value
        type: u2
        doc: "Value to Write (0xFF00 = ON, 0x0000 = OFF)"
  write_single_register:
    seq:
      - id: register_address
        type: u2
        doc: "Register Address"
      - id: register_value
        type: u2
        doc: "Value to Write"
  read_exception_status:
    seq: []
  diagnostics:
    seq:
      - id: sub_function
        type: u2
        doc: "Sub-function Code"
      - id: data
        type: u2
        doc: "Diagnostic Data"
  get_comm_event_counter:
    seq: []
  get_comm_event_log:
    seq: []
  write_multiple_coils:
    seq:
      - id: starting_address
        type: u2
        doc: "Starting Address"
      - id: quantity
        type: u2
        doc: "Number of Coils"
      - id: byte_count
        type: u1
        doc: "Byte Count"
      - id: coils
        type: str
        size: byte_count
        encoding: ASCII
        doc: "Coils Data"
  write_multiple_registers:
    seq:
      - id: starting_address
        type: u2
        doc: "Starting Address"
      - id: quantity
        type: u2
        doc: "Number of Registers"
      - id: byte_count
        type: u1
        doc: "Byte Count"
      - id: registers
        type: str
        size: byte_count
        encoding: ASCII
        doc: "Registers Data"
  report_server_id:
    seq: []
  read_file_record:
    seq:
      - id: byte_count
        type: u1
        doc: "Byte Count"
      - id: file_records
        type: str
        size: byte_count
        encoding: ASCII
        doc: "File Records Data"
  write_file_record:
    seq:
      - id: byte_count
        type: u1
        doc: "Byte Count"
      - id: file_records
        type: str
        size: byte_count
        encoding: ASCII
        doc: "File Records Data"
  mask_write_register:
    seq:
      - id: reference_address
        type: u2
        doc: "Reference Address"
      - id: and_mask
        type: u2
        doc: "AND Mask"
      - id: or_mask
        type: u2
        doc: "OR Mask"
  read_write_multiple_registers:
    seq:
      - id: read_starting_address
        type: u2
        doc: "Read Starting Address"
      - id: read_quantity
        type: u2
        doc: "Number of Registers to Read"
      - id: write_starting_address
        type: u2
        doc: "Write Starting Address"
      - id: write_quantity
        type: u2
        doc: "Number of Registers to Write"
      - id: write_byte_count
        type: u1
        doc: "Write Byte Count"
      - id: write_registers
        type: str
        size: write_byte_count
        encoding: ASCII
        doc: "Write Registers Data"
  read_fifo_queue:
    seq:
      - id: fifo_pointer_address
        type: u2
        doc: "FIFO Pointer Address"
  encapsulated_interface_transport:
    seq:
      - id: mei_type
        type: u1
        doc: "MEI Type"
      - id: mei_data
        type: str
        size-eos: true
        encoding: ASCII
        doc: "MEI Data"