# This is a sample Kaitai Struct definition.  You'll need to replace this with your actual schema.
# The error message suggests a problem with the kaitai-struct-compiler or the input file.
# This example addresses potential issues like incorrect paths and file existence.  Adjust as needed.


$schema: https://kaitai.io/schema/draft/2022-01-03
id: bitcoin_transaction
endian: be

seq:
  - id: magic
    type: u4
  - id: version
    type: u4
  - id: in_count
    type: u4
  - id: ins
    type: ins_array
  - id: out_count
    type: u4
  - id: outs
    type: outs_array
  - id: lock_time
    type: u4

types:
  ins_array:
    seq:
      - type: u4 # size of input
      - type: u4 # sequence
      - type: bytes # script
      - type: u4 # sequence

  outs_array:
    seq:
      - type: u8 # value
      - type: u4 # size of output script
      - type: bytes # script
