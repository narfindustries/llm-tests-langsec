def Main = NTP4

def NTP4 = {
  li : uint2
  vn : uint3
  mode : uint3
  stratum : uint8
  poll : int8
  precision : int8
  root_delay : uint32
  root_dispersion : uint32
  reference_id : uint32
  reference_timestamp : uint64
  originate_timestamp : uint64
  receive_timestamp : uint64
  transmit_timestamp : uint64
}

def uint2 = uint value ~(value <= 3)
def uint3 = uint value ~(value <= 7)
def uint8 = uint8
def int8 = int8
def uint32 = uint32
def uint64 = uint64