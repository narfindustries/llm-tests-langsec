meta:
  id: hl7_v2
  file-extension: hl7
  encoding: ascii
  endian: le

seq:
  - id: message
    type: hl7_message

types:
  hl7_message:
    seq:
      - id: msh_segment
        type: msh_segment
      - id: segments
        type: segment
        repeat: eos

  msh_segment:
    seq:
      - id: field_separator
        type: str
        encoding: ascii
        size: 1
      - id: encoding_characters
        type: str
        encoding: ascii
        size: 4
      - id: sending_application
        type: str
        encoding: ascii
        terminator: 0x7C
      - id: sending_facility
        type: str
        encoding: ascii
        terminator: 0x7C
      - id: receiving_application
        type: str
        encoding: ascii
        terminator: 0x7C
      - id: receiving_facility
        type: str
        encoding: ascii
        terminator: 0x7C
      - id: datetime
        type: str
        encoding: ascii
        terminator: 0x7C
      - id: security
        type: str
        encoding: ascii
        size: 1
        valid:
          any-of: ['D', 'N']

  segment:
    seq:
      - id: pid_segment
        type: pid_segment
        if: _io.pos < _io.size
      - id: pv1_segment
        type: pv1_segment
        if: _io.pos < _io.size
      - id: obr_segment
        type: obr_segment
        if: _io.pos < _io.size
      - id: obx_segment
        type: obx_segment
        if: _io.pos < _io.size

  pid_segment:
    seq:
      - id: set_id
        type: u1
      - id: patient_id
        type: str
        encoding: ascii
        terminator: 0x7C
      - id: patient_name
        type: str
        encoding: ascii
        terminator: 0x7C
      - id: mother_maiden_name
        type: str
        encoding: ascii
        terminator: 0x7C
      - id: date_of_birth
        type: str
        encoding: ascii
        terminator: 0x7C
      - id: administrative_sex
        type: str
        encoding: ascii
        terminator: 0x7C
        valid:
          any-of: ['M', 'F', 'O', 'U', 'A', 'N']

  pv1_segment:
    seq:
      - id: patient_class
        type: str
        encoding: ascii
        terminator: 0x7C
        valid:
          any-of: ['R', 'E', 'O', 'I', 'P']
      - id: assigned_patient_location
        type: str
        encoding: ascii
        terminator: 0x7C
      - id: admission_type
        type: str
        encoding: ascii
        terminator: 0x7C
        valid:
          any-of: ['A', 'E', 'U', 'C']

  obr_segment:
    seq:
      - id: set_id
        type: u1
      - id: placer_order_number
        type: str
        encoding: ascii
        terminator: 0x7C
      - id: filler_order_number
        type: str
        encoding: ascii
        terminator: 0x7C
      - id: universal_service_id
        type: str
        encoding: ascii
        terminator: 0x7C
      - id: observation_datetime
        type: str
        encoding: ascii
        terminator: 0x7C

  obx_segment:
    seq:
      - id: set_id
        type: u1
      - id: value_type
        type: str
        encoding: ascii
        terminator: 0x7C
        valid:
          any-of: ['AD', 'CE', 'CF', 'CK', 'CN', 'CP', 'CX', 'DT', 'ED', 'FT', 'ID', 'IS', 'MA', 'MO', 'NM', 'PN', 'RP', 'SN', 'ST', 'TM', 'TN', 'TS', 'TX', 'XAD', 'XCN', 'XON', 'XPN', 'XTN']
      - id: observation_identifier
        type: str
        encoding: ascii
        terminator: 0x7C
      - id: observation_value
        type: str
        encoding: ascii
        terminator: 0x7C