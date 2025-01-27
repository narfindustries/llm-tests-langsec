$schema: https://github.com/kaitai-io/kaitai-struct/blob/master/spec/schema.json
id: icmp-gemini-1
endian: be

seq:
  type: u2

len:
  type: u2

type:
  type: u2

code:
  type: u2

checksum:
  type: u2

payload:
  type: u8
  repeat: expr
  expr: len - 8
