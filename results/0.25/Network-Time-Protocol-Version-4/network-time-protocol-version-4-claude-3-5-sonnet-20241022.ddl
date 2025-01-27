def Main = NTP_Packet

def NTP_Packet = {
  li: $uint2;
  vn: $uint3;
  mode: $uint3;
  stratum: $uint8;
  poll: $int8;
  precision: $int8;
  root_delay: $uint32;
  root_dispersion: $uint32;
  reference_id: $uint32;
  reference_timestamp: Timestamp;
  originate_timestamp: Timestamp;
  receive_timestamp: Timestamp;
  transmit_timestamp: Timestamp;
  extensions: Extensions
}

def Extensions = {
  @try {
    fields: Many1 Extension_Field
  }
  | {}
}

def Extension_Field = {
  type: $uint16;
  length: $uint16;
  value: Value(length)
}

def Value(len: uint16) = 
  $bytes(len)

def Timestamp = {
  seconds: $uint32;
  fraction: $uint32
}

def Many1(p) = {
  x = p;
  xs = Many(p);
  return FList1 x xs
}

def Many(p) = {
  @try {
    x = p;
    xs = Many(p);
    return FList1 x xs
  }
  | return FNil
}

type FList1 a = a FList
type FList a = FNil | FList1 a