type FileHeader {
  fhdr: string(9) = "NITF02.10" | "NITF02.11";
  fver: string(6) = "02.10" | "02.11";
  clevel: string(1) = "U" | "C" | "S" | "T";
  csys: string(2) = "US" | "CA" | "UK" | "AU";
  ccode: string(1) = "U" | "C" | "S" | "T";
  rel: string(20);
  ctlh: string(20);
  nstd: uint8 = 1;
  osta: string(25);
  otitle: string(40);
  ocname: string(20);
  ocaddr: string(40);
  fdt: string(14);
  fhdrsz: uint16;
  om: string(1) = "B" | "E";
  filepart: string(1) = "A" | "B";
  irep: string(1) = "B" | "M" | "R";
  ief: string(1) = "0" | "1";
  onum: string(10);
  odid: string(25);
  otpl: string(3);
  seccls: string(1) = "U" | "C" | "S" | "T";
  secctlh: string(20);
  secrel: string(20);
  seccode: string(1) = "U" | "C" | "S" | "T";
  cntcc: string(2);
  cntrel: string(20);
  encl: string(1) = "0" | "1";
  bcsn: string(13);
  fcsn: string(13);
}

type ImageSegment {
  isid: string(2) = "IS";
  iid1: string(25);
  iid2: string(17);
  idatim: string(14);
  imag: string(3) = "VIS" | "IR" | "UV" | "MM";
  irow: uint16;
  icol: uint16;
  irep: string(1) = "B" | "M" | "R";
  iband: uint8;
  igeol: string(1) = "0" | "1";
  isorce: string(1) = "0" | "1" | "2" | "3";
  iecom: string(1) = "0" | "1";
  icat: string(1) = "0" | "1";
  iac: string(1) = "0" | "1";
  iam: string(1) = "0" | "1";
  iasls: string(1) = "0" | "1";
  iaslc: string(1) = "0" | "1";
  igtf: string(1) = "0" | "1";
  igeod: string(1) = "0" | "1";
  idt: string(1) = "0" | "1";
  idata1: bytes;
  idata2: bytes;
}

type GraphicSegment {
  gsid: string(2) = "GS";
  gid1: string(25);
  gid2: string(17);
  gdatim: string(14);
  gtype: string(1) = "0" | "1" | "2" | "3";
  grow: uint16;
  gcol: uint16;
  gecom: string(1) = "0" | "1";
  gcat: string(1) = "0" | "1";
  gac: string(1) = "0" | "1";
  gam: string(1) = "0" | "1";
  gasls: string(1) = "0" | "1";
  gaslc: string(1) = "0" | "1";
  ggtf: string(1) = "0" | "1";
  ggeod: string(1) = "0" | "1";
  gdt: string(1) = "0" | "1";
  gdata1: bytes;
  gdata2: bytes;
}

type TextSegment {
  tsid: string(2) = "TS";
  tid1: string(25);
  tid2: string(17);
  tdatim: string(14);
  ttype: string(1) = "0" | "1" | "2" | "3";
  tlen: uint16;
  tecom: string(1) = "0" | "1";
  tcat: string(1) = "0" | "1";
  tac: string(1) = "0" | "1";
  tam: string(1) = "0" | "1";
  tasls: string(1) = "0" | "1";
  taslc: string(1) = "0" | "1";
  tgtf: string(1) = "0" | "1";
  tgeod: string(1) = "0" | "1";
  tdt: string(1) = "0" | "1";
  tdata1: bytes;
  tdata2: bytes;
}

type TrailerSegment {
  tsid: string(2) = "TS";
  tlen: uint16;
}

type NITF {
  file_header: FileHeader;
  image_segments: [ImageSegment];
  graphic_segments: [GraphicSegment];
  text_segments: [TextSegment];
  trailer_segment: TrailerSegment;
}