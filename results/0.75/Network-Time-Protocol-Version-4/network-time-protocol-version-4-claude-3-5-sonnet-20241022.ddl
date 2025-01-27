def Main = NTP_Packet

def NTP_Packet = {
  li: LI
  vn: VN
  mode: Mode
  stratum: uint8
  poll: int8
  precision: int8
  root_delay: int32
  root_dispersion: int32
  ref_id: RefID
  ref_timestamp: Timestamp
  origin_timestamp: Timestamp
  receive_timestamp: Timestamp
  transmit_timestamp: Timestamp
  extensions: Extensions?
  mac: MAC?
}

def LI = uint2
def VN = uint3
def Mode = uint3

def RefID = bytes(4)

def Timestamp = {
  seconds: uint32
  fraction: uint32
}

def Extensions = [Extension]

def Extension = {
  field_type: uint16
  length: uint16
  value: bytes(length)
}

def MAC = {
  key_id: uint32
  digest: bytes(16..20)
}

def uint2 = n:uint8 if n < 4
def uint3 = n:uint8 if n < 8