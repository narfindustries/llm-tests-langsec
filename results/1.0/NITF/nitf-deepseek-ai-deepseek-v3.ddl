NITF_File = {
  FileHeader: FileHeader;
  ImageSegments: ImageHeader*;
  GraphicSegments: GraphicHeader*;
  TextSegments: TextHeader*;
  DataExtensionSegments: DataExtensionHeader*;
}

FileHeader = {
  FHDR: "NITF";
  FVER: string(4);
  CLEVEL: uint8;
  STYPE: "BF";
  OSTAID: string(10);
  FDT: string(14);
  FTITLE: string(80);
  Security: Security;
  FSCOP: string(5);
  FSCPYS: string(5);
  ONAME: string(24);
  OPHONE: string(18);
}

Security = {
  FSCLAS: string(1);
  FSCLSY: string(2);
  FSCODE: string(11);
  FSCTLH: string(2);
  FSREL: string(20);
  FSDCTP: string(2);
  FSDCDT: string(8);
  FSDCXM: string(4);
  FSDG: string(1);
  FSDGDT: string(8);
  FSCLTX: string(43);
}

ImageHeader = {
  IM: "IM";
  IID: string(10);
  IDATIM: string(14);
  TGTID: string(17);
  IITLE: string(80);
  Security: Security;
  ENCRYP: uint8;
  ISORCE: string(42);
  NROWS: uint32;
  NCOLS: uint32;
}

GraphicHeader = {
  GID: string(10);
  GNAME: string(20);
  Security: Security;
}

TextHeader = {
  TID: string(10);
  TXTALVL: uint8;
  TXTDT: string(14);
  TXTITL: string(80);
  Security: Security;
}

DataExtensionHeader = {
  DESID: string(25);
  DESVER: uint8;
  DESNAME: string(80);
}