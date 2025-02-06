def ID1 = 0x1f;
def ID2 = 0x8b;
def DEFLATE = 8;

def FTEXT_MASK = 0x01;
def FHCRC_MASK = 0x02;
def FEXTRA_MASK = 0x04;
def FNAME_MASK = 0x08;
def FCOMMENT_MASK = 0x10;

def HasFlag = {
  flags: uint8;
  mask: uint8;
  (flags band mask) != 0
};

format ExtraSubField {
  SI1: uint8;
  SI2: uint8;
  LEN: uint16;
  DATA: byte[LEN];
};

format ExtraField {
  XLEN: uint16;
  subfields: ExtraSubField[];
};

def NullTermString = {
  var result: byte[];
  while (!Lookahead(0x00)) {
    result += byte;
  }
  Skip(1);
  result
};

format GzipHeader {
  id1: uint8 where id1 == ID1;
  id2: uint8 where id2 == ID2;
  cm: uint8 where cm == DEFLATE;
  flags: uint8;
  mtime: uint32;
  xfl: uint8;
  os: uint8;
  
  @if(HasFlag(flags, FEXTRA_MASK))
    extra: ExtraField;
  @end

  @if(HasFlag(flags, FNAME_MASK))
    fname: NullTermString;
  @end

  @if(HasFlag(flags, FCOMMENT_MASK))
    fcomment: NullTermString;
  @end

  @if(HasFlag(flags, FHCRC_MASK))
    crc16: uint16;
  @end
};

format GzipFile {
  header: GzipHeader;
  compressed_data: byte[];
  crc32: uint32;
  isize: uint32;
};

def Main = GzipFile;