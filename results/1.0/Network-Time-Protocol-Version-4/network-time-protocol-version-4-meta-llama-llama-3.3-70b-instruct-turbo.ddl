DOMAIN NetworkTimeProtocolVersion4 MetaLlamaLLama33

TYPE NtpPacket = STRUCT {
  leap:       UINT(2),
  version:    UINT(3),
  mode:       UINT(3),
  poll:       UINT(8),
  precision:  UINT(8),
  delay:      UINT(32),
  dispersion: UINT(32),
  identifier: UINT(32),
  referenceTs:TIMESTAMP(64),
  originateTs:TIMESTAMP(64),
  receiveTs:  TIMESTAMP(64),
  transmitTs: TIMESTAMP(64)
}

TYPE TIMESTAMP(length: UINT) = BITS(length) {
  WHEN length == 32 {
    int:     UINT(16),
    fract:   UINT(16)
  }
  WHEN length == 64 {
    seconds: UINT(32),
    fract:   UINT(32)
  }
}

FORMATTED NtpPacket {
  leap        @ 0 : 1,
  version     @ 1 : 2,
  mode        @ 2 : 3,
  poll        @ 4 : 5,
  precision   @ 6 : 7,
  delay       @ 8 : 11,
  dispersion  @ 12 : 15,
  identifier  @ 16 : 19,
  referenceTs @ 20 : 27,
  originateTs @ 28 : 35,
  receiveTs   @ 36 : 43,
  transmitTs  @ 44 : 51
}

FORMATTED TIMESTAMP(length: UINT) {
  WHEN length == 32 {
    int     @ 0 : 15,
    fract   @ 16 : 31
  }
  WHEN length == 64 {
    seconds @ 0 : 31,
    fract   @ 32 : 63
  }
}