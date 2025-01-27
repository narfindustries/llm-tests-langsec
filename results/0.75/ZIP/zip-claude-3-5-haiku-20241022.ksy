meta:
  id: zip
  file-extension: zip
  endian: le
seq:
  - id: signatures
    type: signatures
  - id: entries
    type: entry
    repeat: eos
types:
  signatures:
    seq:
      - id: local_file_header
        contents: [0x50, 0x4b, 0x03, 0x04]
      - id: central_directory_header
        contents: [0x50, 0x4b, 0x01, 0x02]
      - id: end_of_central_directory_record
        contents: [0x50, 0x4b, 0x05, 0x06]
  entry:
    seq:
      - id: header
        type: local_file_header
      - id: body
        size: header.compressed_size
      - id: data_descriptor
        type: data_descriptor
        if: header.flags.has_data_descriptor
    types:
      local_file_header:
        seq:
          - id: version
            type: u2
          - id: flags
            type: flags
          - id: compression_method
            type: u2
          - id: last_mod_time
            type: u2
          - id: last_mod_date
            type: u2
          - id: crc32
            type: u4
          - id: compressed_size
            type: u4
          - id: uncompressed_size
            type: u4
          - id: filename_length
            type: u2
          - id: extra_field_length
            type: u2
          - id: filename
            type: str
            size: filename_length
            encoding: utf-8
          - id: extra_field
            size: extra_field_length
        types:
          flags:
            seq:
              - id: has_data_descriptor
                type: b1
      data_descriptor:
        seq:
          - id: crc32
            type: u4
          - id: compressed_size
            type: u4
          - id: uncompressed_size
            type: u4