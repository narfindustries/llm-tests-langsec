type FILE_HEADER = struct {
  FILEHDRID: string(4) = "NITF",
  FILEVER: string(6) = "02.10",
  COMPLEXITYLV: uint8,
  STYP: string(4),
  ORGNM: string(25),
  ORGPNO: string(20),
  FSC: enum { "U", "C", "S", "T" },
  RELEASING: string(40),
  FTITLE: string(80),
  FSCSCLAS: enum { "USA", "USM", "NATO" }
}

type IMAGE_HEADER = struct {
  IMID: string(25),
  IDATIM: string(14),
  IMAG: enum { "U", "C", "S", "T" },
  IMRELEAS: string(40),
  ITITLE: string(80),
  ICOMP: enum { "NC", "BICUBIC", "JPEG" },
  IPT: enum { "INT8", "UINT8", "INT16", "UINT16" }
}

type IMAGE_DATA = struct {
  IDT: enum { "MONO", "RGB", "MULTI" },
  IPSZ: string(10),
  IBIT: uint8
}

type DATA_EXTENSION_SEGMENT = struct {
  DESID: string(25),
  DESC: enum { "U", "C", "S", "T" },
  DERELEAS: string(40),
  DESDATA: bytes
}

type TEXT_SEGMENT = struct {
  TSID: string(25),
  TSC: enum { "U", "C", "S", "T" },
  TSRELEAS: string(40),
  TSDATA: string
}

type NITF_FILE = struct {
  HS: FILE_HEADER,
  IH: IMAGE_HEADER,
  ID: IMAGE_DATA,
  DES: optional(DATA_EXTENSION_SEGMENT),
  TS: optional(TEXT_SEGMENT)
}

type NITF_FILE_REPEAT = struct {
  HS: FILE_HEADER,
  IH: IMAGE_HEADER,
  ID: IMAGE_DATA,
  DES: optional(DATA_EXTENSION_SEGMENT),
  TS: optional(TEXT_SEGMENT),
  NEXT: optional(NITF_FILE_REPEAT)
}

root type NITF_FILE_REPEAT