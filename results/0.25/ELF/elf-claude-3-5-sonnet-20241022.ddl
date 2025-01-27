def Main = {
  ELF_header;
  Program_header_table;
  Section_header_table
}

def ELF_header = {
  magic = Match 0x7f'ELF';
  class = uint8;
  data = uint8;
  version = uint8;
  osabi = uint8;
  abiversion = uint8;
  pad = Array 7 (uint8);
  type = uint16;
  machine = uint16;
  version2 = uint32;
  entry = uint64;
  phoff = uint64;
  shoff = uint64;
  flags = uint32;
  ehsize = uint16;
  phentsize = uint16;
  phnum = uint16;
  shentsize = uint16;
  shnum = uint16;
  shstrndx = uint16
}

def Program_header_table = {
  pheaders = Array @(ELF_header.phnum) Program_header
}

def Program_header = {
  p_type = uint32;
  p_flags = uint32;
  p_offset = uint64;
  p_vaddr = uint64;
  p_paddr = uint64;
  p_filesz = uint64;
  p_memsz = uint64;
  p_align = uint64
}

def Section_header_table = {
  sheaders = Array @(ELF_header.shnum) Section_header
}

def Section_header = {
  sh_name = uint32;
  sh_type = uint32;
  sh_flags = uint64;
  sh_addr = uint64;
  sh_offset = uint64;
  sh_size = uint64;
  sh_link = uint32;
  sh_info = uint32;
  sh_addralign = uint64;
  sh_entsize = uint64
}