domain dicom {
  version 3.3

  types {
    ui32 : uint32
    ui16 : uint16
    ui8  : uint8
    str  : string
  }

  structure dicom-meta {
    uint16(2) group-number
    uint16(2) element-number
    ui32 length
    bytes(length) value
  }

  structure dicom-data-set {
    seq(dicom-meta) elements
  }

  grammar {
    start -> dicom-data-set
  }

  concrete {
    group-number  : "x" ui16
    element-number: "x" ui16
    length       : "x" ui32
    value        : bytes
  }

  header {
    pattern: "0x44494943 0x4d45" // "DICM"
  }

  footer {
    pattern: "0x434f440a" // "COD\n"
  }
}