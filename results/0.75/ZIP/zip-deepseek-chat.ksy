meta:
  id: zip
  file-extension: zip
  endian: le
seq:
  - id: signature
    type: u4
    enum: local_file_header
  - id: version_needed
    type: u2
  - id: general_purpose_bit_flag
    type: u2
  - id: compression_method
    type: u2
    enum: compression_methods
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
  - id: file_name_length
    type: u2
  - id: extra_field_length
    type: u2
  - id: file_name
    type: str
    encoding: UTF-8
    size: file_name_length
  - id: extra_field
    type: u1
    repeat: expr
    repeat-expr: extra_field_length
  - id: file_data
    type: u1
    repeat: expr
    repeat-expr: compressed_size
enums:
  local_file_header:
    0x04034b50: local_file_header
  compression_methods:
    0: stored
    1: shrunk
    2: reduced_1
    3: reduced_2
    4: reduced_3
    5: reduced_4
    6: imploded
    7: tokenized
    8: deflated
    9: enhanced_deflated
    10: pkware_dcl_imploded
    12: bzip2
    14: lzma
    18: ibm_terse
    19: ibm_lz77_z
    97: zstd
    98: mp3
    99: xz
    100: jpeg
    101: wavpack
    102: ppmd
    103: aex_encryption_marker