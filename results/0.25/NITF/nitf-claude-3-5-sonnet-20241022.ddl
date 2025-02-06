def NITF = struct {
  fhdr: bytes(4) where fhdr == b"NITF";
  fver: bytes(5) where fver == b"02.10";
  clevel: u8 where clevel in [3, 5, 6, 7];
  stype: bytes(2) where stype == b"BF";
  ostaid: bytes(10);
  fdt: bytes(14);
  ftitle: bytes(80);
  fsclas: bytes(1) where fsclas in [b"T", b"S", b"C", b"R", b"U"];
  fsclsy: bytes(2);
  fscode: bytes(11);
  fsctlh: bytes(2);
  fsrel: bytes(20);
  fsdctp: bytes(2);
  fsdcdt: bytes(8);
  fsdcxm: bytes(4);
  fsdg: bytes(1);
  fsdgdt: bytes(8);
  fscltx: bytes(43);
  fscatp: bytes(1);
  fscaut: bytes(40);
  fscrsn: bytes(1);
  fssrdt: bytes(8);
  fsctln: bytes(15);
  encryp: u8 where encryp in [0, 1];
  fbkgc: bytes(3);
  oname: bytes(24);
  ophone: bytes(18);
  fl: u64;
  hl: u16;
  numi: u16;
  if numi > 0 {
    lish: u16[numi];
    li: u64[numi];
  };
  nums: u16;
  if nums > 0 {
    lssh: u16[nums];
    ls: u64[nums];
  };
  numx: u16;
  if numx > 0 {
    lxsh: u16[numx];
    lx: u64[numx];
  };
  numt: u16;
  if numt > 0 {
    ltsh: u16[numt];
    lt: u64[numt];
  };
  numdes: u16;
  if numdes > 0 {
    ldsh: u16[numdes];
    ld: u64[numdes];
  };
  numres: u16;
  if numres > 0 {
    lresh: u16[numres];
    lre: u64[numres];
  };
  udhdl: u16;
  if udhdl > 0 {
    udhd: bytes(udhdl);
  };
  xhdl: u16;
  if xhdl > 0 {
    xhd: bytes(xhdl);
  };
  if numi > 0 {
    image_segments: ImageSegment[numi];
  };
  if nums > 0 {
    graphic_segments: GraphicSegment[nums];
  };
  if numx > 0 {
    symbol_segments: SymbolSegment[numx];
  };
  if numt > 0 {
    text_segments: TextSegment[numt];
  };
  if numdes > 0 {
    data_extension_segments: DataExtensionSegment[numdes];
  };
  if numres > 0 {
    reserved_extension_segments: ReservedExtensionSegment[numres];
  }
}

def ImageSegment = struct {
  im: bytes(2) where im == b"IM";
  iid1: bytes(10);
  idatim: bytes(14);
  tgtid: bytes(17);
  iid2: bytes(80);
  isclas: bytes(1) where isclas in [b"T", b"S", b"C", b"R", b"U"];
  isclsy: bytes(2);
  iscode: bytes(11);
  isctlh: bytes(2);
  isrel: bytes(20);
  isdctp: bytes(2);
  isdcdt: bytes(8);
  isdcxm: bytes(4);
  isdg: bytes(1);
  isdgdt: bytes(8);
  iscltx: bytes(43);
  iscatp: bytes(1);
  iscaut: bytes(40);
  iscrsn: bytes(1);
  issrdt: bytes(8);
  isctln: bytes(15);
  encryp: u8 where encryp in [0, 1];
  isorce: bytes(42);
  nrows: u32;
  ncols: u32;
  pvtype: bytes(3);
  irep: bytes(8);
  icat: bytes(8);
  abpp: u8;
  pjust: bytes(1);
  icords: bytes(1);
  if icords in [b"U", b"G", b"N", b"S", b"D"] {
    igeolo: bytes(60);
  };
  nicom: u8;
  if nicom > 0 {
    icom: bytes(80)[nicom];
  };
  ic: bytes(2);
  comrat: bytes(4);
  nbands: u8;
  xbands: u16;
  actual_bands: u16 = if nbands == 0 then xbands else nbands;
  bandinfo: BandInfo[actual_bands];
  isync: u8;
  imode: bytes(1);
  nbpr: u16;
  nbpc: u16;
  nppbh: u16;
  nppbv: u16;
  nbpp: u8;
  idlvl: u16;
  ialvl: u16;
  iloc: u16;
  imag: bytes(4);
  udidl: u16;
  if udidl > 0 {
    udid: bytes(udidl);
  };
  ixshdl: u16;
  if ixshdl > 0 {
    ixshd: bytes(ixshdl);
  };
  image_data: bytes(nrows * ncols * ((nbpp + 7) / 8))
}

def BandInfo = struct {
  irepband: bytes(2);
  isubcat: bytes(6);
  ifc: bytes(1);
  imflt: bytes(3);
  nluts: u8;
  if nluts > 0 {
    lutd: u8[nluts][256];
  }
}

def GraphicSegment = struct {
  sy: bytes(2) where sy == b"SY";
  sid: bytes(10);
  stype: bytes(1);
  sxshdl: u16;
  if sxshdl > 0 {
    sxshd: bytes(sxshdl);
  };
  sdlvl: u16;
  salvl: u16;
  sloc: u32;
  sbnd1: u32;
  scolor: bytes(1);
  sbnd2: u32;
  sres2: u16;
  data: bytes(sbnd2)
}

def TextSegment = struct {
  te: bytes(2) where te == b"TE";
  textid: bytes(7);
  txtalvl: u8;
  txtdt: bytes(14);
  txtitl: bytes(80);
  tsclas: bytes(1);
  tsclsy: bytes(2);
  tscode: bytes(11);
  tsctlh: bytes(2);
  tsrel: bytes(20);
  tsdctp: bytes(2);
  tsdcdt: bytes(8);
  tsdcxm: bytes(4);
  tsdg: bytes(1);
  tsdgdt: bytes(8);
  tscltx: bytes(43);
  tscatp: bytes(1);
  tscaut: bytes(40);
  tscrsn: bytes(1);
  tssrdt: bytes(8);
  tsctln: bytes(15);
  encryp: u8;
  txtfmt: bytes(3);
  txshdl: u16;
  if txshdl > 0 {
    txshd: bytes(txshdl);
  };
  text_data: bytes
}

def DataExtensionSegment = struct {
  de: bytes(2) where de == b"DE";
  destag: bytes(25);
  desver: u8;
  desclas: bytes(1);
  desclsy: bytes(2);
  descode: bytes(11);
  desctlh: bytes(2);
  desrel: bytes(20);
  desdctp: bytes(2);
  desdcdt: bytes(8);
  desdcxm: bytes(4);
  desdg: bytes(1);
  desdgdt: bytes(8);
  descltx: bytes(43);
  descatp: bytes(1);
  descaut: bytes(40);
  descrsn: bytes(1);
  dessrdt: bytes(8);
  desctln: bytes(15);
  encryp: u8;
  desoflw: u8;
  desitem: u8;
  desshl: u16;
  if desshl > 0 {
    desshf: bytes(desshl);
  };
  desdata: bytes
}

def ReservedExtensionSegment = struct {
  re: bytes(2) where re == b"RE";
  resid: bytes(25);
  resver: u8;
  resclas: bytes(1);
  resclsy: bytes(2);
  rescode: bytes(11);
  resctlh: bytes(2);
  resrel: bytes(20);
  resdctp: bytes(2);
  resdcdt: bytes(8);
  resdcxm: bytes(4);
  resdg: bytes(1);
  resdgdt: bytes(8);
  rescltx: bytes(43);
  rescatp: bytes(1);
  rescaut: bytes(40);
  rescrsn: bytes(1);
  ressrdt: bytes(8);
  resctln: bytes(15);
  encryp: u8;
  resshl: u16;
  if resshl > 0 {
    resshf: bytes(resshl);
  };
  resdata: bytes
}