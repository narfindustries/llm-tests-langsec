def Main = SeqStrict {
  §ELF;
  TestChecksum;
  end_of_file
}

def ELF = {
  ELFHeader;
  ProgramHeaders;
  §SectionHeaders;
  §PayloadData
}

def ELFHeader = {
  magic       = Array 4 (Byte value = [0x7f, 'E', 'L', 'F']);
  class       = Byte value = 2;  -- 64-bit
  endianness  = Byte value = 1;  -- little endian
  version     = Byte value = 1;  -- version 1
  osabi       = Byte value = 0;  -- System V
  abiversion  = Byte value = 0;
  pad         = Array 7 (Byte value = 0);
  type        = UInt16LE value = 2;  -- executable
  machine     = UInt16LE value = 0x3E;  -- AMD x86-64
  version2    = UInt32LE value = 1;
  entry       = UInt64LE;
  phoff       = UInt64LE;
  shoff       = UInt64LE;
  flags       = UInt32LE value = 0;
  ehsize      = UInt16LE;
  phentsize   = UInt16LE;
  phnum       = UInt16LE;
  shentsize   = UInt16LE;
  shnum       = UInt16LE;
  shstrndx    = UInt16LE
}

def ProgramHeaders = {
  pheaders = Array @ehsize (@phnum) ProgramHeader
}

def ProgramHeader = {
  ptype    = UInt32LE;
  flags    = UInt32LE;
  offset   = UInt64LE;
  vaddr    = UInt64LE;
  paddr    = UInt64LE;
  filesz   = UInt64LE;
  memsz    = UInt64LE;
  align    = UInt64LE
}

def SectionHeaders = {
  sheaders = Array @shentsize (@shnum) SectionHeader
}

def SectionHeader = {
  name      = UInt32LE;
  type      = UInt32LE;
  flags     = UInt64LE;
  addr      = UInt64LE;
  offset    = UInt64LE;
  size      = UInt64LE;
  link      = UInt32LE;
  info      = UInt32LE;
  addralign = UInt64LE;
  entsize   = UInt64LE
}

def PayloadData = {
  data = Many Byte Until end_of_file
}

def TestChecksum = {
  computed_sum = UInt32LE value = 0;
  true
}