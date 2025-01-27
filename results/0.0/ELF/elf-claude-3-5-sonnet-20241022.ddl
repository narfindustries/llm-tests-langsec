def Main = {
  header;
  sections
}

def header = {
  magic = Match 0x7f "ELF";
  class = ELFCLASS;
  data = ELFDATA;
  version = ELFVersion;
  osabi = ELFOSABI;
  abiversion = uint 8;
  pad = zeros 7;
  type = ELFType;
  machine = ELFMachine;
  elfversion = uint 32;
  entry = uint 64;
  phoff = uint 64;
  shoff = uint 64;
  flags = uint 32;
  ehsize = uint 16;
  phentsize = uint 16;
  phnum = uint 16;
  shentsize = uint 16;
  shnum = uint 16;
  shstrndx = uint 16
}

def ELFCLASS = uint 8
def ELFDATA = uint 8
def ELFVersion = uint 8
def ELFOSABI = uint 8
def ELFType = uint 16
def ELFMachine = uint 16

def sections = many {
  sh_name = uint 32;
  sh_type = uint 32;
  sh_flags = uint 64;
  sh_addr = uint 64;
  sh_offset = uint 64;
  sh_size = uint 64;
  sh_link = uint 32;
  sh_info = uint 32;
  sh_addralign = uint 64;
  sh_entsize = uint 64
}

def zeros n = {
  xs = many uint 8;
  length xs == n
}