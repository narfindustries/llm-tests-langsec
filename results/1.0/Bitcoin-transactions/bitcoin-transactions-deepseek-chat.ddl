BitcoinTransaction = {
  version: UInt32LE,
  input_count: VarInt,
  inputs: Input[input_count],
  output_count: VarInt,
  outputs: Output[output_count],
  lock_time: UInt32LE
};

Input = {
  previous_output: OutPoint,
  script_length: VarInt,
  signature_script: UInt8[script_length],
  sequence: UInt32LE
};

Output = {
  value: UInt64LE,
  script_length: VarInt,
  pk_script: UInt8[script_length]
};

OutPoint = {
  hash: UInt8[32],
  index: UInt32LE
};

VarInt = switch {
  case value < 0xFD: UInt8,
  case value <= 0xFFFF: UInt8 { 0xFD } UInt16LE,
  case value <= 0xFFFFFFFF: UInt8 { 0xFE } UInt32LE,
  default: UInt8 { 0xFF } UInt64LE
};