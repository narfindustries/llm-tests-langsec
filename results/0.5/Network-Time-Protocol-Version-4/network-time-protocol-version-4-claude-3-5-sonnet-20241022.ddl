def Main = {
  LI: bits(2)
  VN: bits(3)
  Mode: bits(3)
  Stratum: u8
  Poll: i8
  Precision: i8
  RootDelay: i32
  RootDispersion: u32
  ReferenceID: bytes(4)
  ReferenceTimestamp: {
    seconds: u32
    fraction: u32
  }
  OriginTimestamp: {
    seconds: u32
    fraction: u32
  }
  ReceiveTimestamp: {
    seconds: u32
    fraction: u32
  }
  TransmitTimestamp: {
    seconds: u32
    fraction: u32
  }
  ExtensionFields: [{
    type: u16
    length: u16
    value: bytes(length - 4)
  }]
  if |ExtensionFields| > 0 {
    KeyID: u32
    MessageDigest: bytes(select(20, 16))
  }
}

def select(a: u32, b: u32) = {
  result: u32
  assert(result == a || result == b)
  result
}