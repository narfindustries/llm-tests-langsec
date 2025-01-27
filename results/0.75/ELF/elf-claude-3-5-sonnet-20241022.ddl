def Main = {
  ELF_file
}

def ELF_file = {
  magic = Magic;
  elf_class = ELFCLASS;
  data = ELFDATA;
  version = ELFVERSION;
  osabi = OSABI;
  abiversion = ABIVERSION;
  pad = PAD;
  type = TYPE;
  machine = MACHINE;
  e_version = E_VERSION;
  entry = ENTRY;
  phoff = PHOFF;
  shoff = SHOFF;
  flags = FLAGS;
  ehsize = EHSIZE;
  phentsize = PHENTSIZE;
  phnum = PHNUM;
  shentsize = SHENTSIZE;
  shnum = SHNUM;
  shstrndx = SHSTRNDX
}

def Magic = {
  magic = Bytes 4 where magic == [0x7f, 0x45, 0x4c, 0x46]
}

def ELFCLASS = {
  class = UInt8 where class in [1, 2]
}

def ELFDATA = {
  data = UInt8 where data in [1, 2]
}

def ELFVERSION = {
  version = UInt8 where version == 1
}

def OSABI = {
  osabi = UInt8
}

def ABIVERSION = {
  abiversion = UInt8
}

def PAD = {
  pad = Bytes 7
}

def TYPE = {
  type = UInt16
}

def MACHINE = {
  machine = UInt16
}

def E_VERSION = {
  version = UInt32 where version == 1
}

def ENTRY = {
  entry = UInt64
}

def PHOFF = {
  phoff = UInt64
}

def SHOFF = {
  shoff = UInt64
}

def FLAGS = {
  flags = UInt32
}

def EHSIZE = {
  ehsize = UInt16
}

def PHENTSIZE = {
  phentsize = UInt16
}

def PHNUM = {
  phnum = UInt16
}

def SHENTSIZE = {
  shentsize = UInt16
}

def SHNUM = {
  shnum = UInt16
}

def SHSTRNDX = {
  shstrndx = UInt16
}