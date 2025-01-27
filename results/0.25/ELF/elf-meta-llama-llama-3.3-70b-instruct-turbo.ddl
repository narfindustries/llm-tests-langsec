[id: elf-meta-llama-llama-3.3-70b-instruct-turbo]
[name: ELF Meta LLaMA LLaMA 3.3 70B Instruct Turbo]
[license: MIT]
[description: Daedalus specification for ELF Meta LLaMA LLaMA 3.3 70B Instruct Turbo]
[version: 1.0]

LittleEndian {
  magic: uint32 = 0x464C457F;
  elf_class: uint8;
  data_encoding: uint8;
  file_version: uint8;
  os_abi: uint8;
  abi_version: uint8;
  pad: bytes[7];

  e_type: uint16;
  e_machine: uint16;
  e_version: uint32;
  e_entry: uint64;
  e_phoff: uint64;
  e_shoff: uint64;
  e_flags: uint32;
  e_ehsize: uint16;
  e_phentsize: uint16;
  e_phnum: uint16;
  e_shentsize: uint16;
  e_shnum: uint16;
  e_shstrndx: uint16;

  if e_phnum > 0 {
    program_headers: ProgramHeader[e_phnum];
  }

  if e_shnum > 0 {
    section_headers: SectionHeader[e_shnum];
  }
}

ProgramHeader {
  p_type: uint32;
  p_flags: uint32;
  p_offset: uint64;
  p_vaddr: uint64;
  p_paddr: uint64;
  p_filesz: uint64;
  p_memsz: uint64;
  p_align: uint64;
}

SectionHeader {
  sh_name: uint32;
  sh_type: uint32;
  sh_flags: uint64;
  sh_addr: uint64;
  sh_offset: uint64;
  sh_size: uint64;
  sh_link: uint32;
  sh_info: uint32;
  sh_addralign: uint64;
  sh_entsize: uint64;
}

assert magic == 0x464C457F, "Invalid ELF magic";
assert elf_class == 2, "Only 64-bit ELF is supported";
assert data_encoding == 1, "Only little-endian is supported";
assert file_version == 1, "Only EV_CURRENT is supported";
assert os_abi == 0, "Only SYSV is supported";
assert abi_version == 0, "Only ABI version 0 is supported";