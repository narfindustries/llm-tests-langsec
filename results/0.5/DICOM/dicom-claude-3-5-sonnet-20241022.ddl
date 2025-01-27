#!daedalus
def Main = DICOM

def DICOM = {
  preamble: $byte{128},
  prefix: "DICM",
  elements: DataElement[]
}

def DataElement = {
  tag: Tag,
  vr: VR?,
  length: UInt32,
  value: Value(length)
}

def Tag = {
  group: UInt16,
  element: UInt16
}

def VR = Choose {
  "AE" | "AS" | "AT" | "CS" | "DA" | "DS" | "DT" | "FL" | "FD" | "IS" |
  "LO" | "LT" | "OB" | "OD" | "OF" | "OW" | "PN" | "SH" | "SL" | "SQ" |
  "SS" | "ST" | "TM" | "UI" | "UL" | "UN" | "US" | "UT"
}

def Value (len: uint32) = $byte{len}

def UInt16 = $uint16_le
def UInt32 = $uint32_le