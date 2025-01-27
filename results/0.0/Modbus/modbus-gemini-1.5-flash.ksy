meta:
  id: modbus-gemini-1
  title: Modbus Gemini 1.5 Flash
  endian: be

types:
  uint16:
    type: u2

  uint32:
    type: u4

  uint64:
    type: u8

  crc16:
    type: u2
    read: crc16_ccitt

seqs:
  header:
    seq:
      - id: magic
        type: u4
        enum: magic_enum
      - id: version
        type: u2
      - id: length
        type: u4
      - id: crc
        type: crc16

  flash_data:
    seq:
      - id: data
        type: u1*

  modbus_gemini_1_5_flash:
    seq:
      - id: header
        type: header
      - id: flash_data
        type: flash_data


enums:
  magic_enum:
    0xABCD1234: magic_abcd1234


