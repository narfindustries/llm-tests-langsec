types:
  - id: modbus_gemini_1_5_flash
    name: Modbus Gemini 1.5 Flash
    fields:
      - id: header
        type: header
      - id: data
        type: seq
        size: (header.data_length)
        read: r
        elements:
          - type: u1

  - id: header
    name: Header
    fields:
      - id: magic
        type: u4
        size: 4
      - id: version
        type: u2
        size: 2
      - id: data_length
        type: u4
        size: 4

