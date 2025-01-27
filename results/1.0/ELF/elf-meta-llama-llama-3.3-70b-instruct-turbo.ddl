domain ELF {
  id = "ELF";
  version = "1.0";
}

type Elf_Hdr {
  e_ident: byte[16];
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
}

type Prog_Hdr {
  p_type: uint32;
  p_flags: uint32;
  p_offset: uint64;
  p_vaddr: uint64;
  p_paddr: uint64;
  p_filesz: uint64;
  p_memsz: uint64;
  p_align: uint64;
}

type Sect_Hdr {
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

grammar {
  elf_file: elf_hdr section_headers* program_headers*;
  elf_hdr: Elf_Hdr;
  section_headers: Sect_Hdr;
  program_headers: Prog_Hdr;
}