define type ElfHeader {
  e_ident: byte[16],
  e_type: uint16,
  e_machine: uint16,
  e_version: uint32,
  e_entry: uint32 or uint64,
  e_phoff: uint32 or uint64,
  e_shoff: uint32 or uint64,
  e_flags: uint32,
  e_ehsize: uint16,
  e_phentsize: uint16,
  e_phnum: uint16,
  e_shentsize: uint16,
  e_shnum: uint16,
  e_shstrndx: uint16
}

define type ProgramHeader {
  p_type: uint32,
  p_offset: uint32 or uint64,
  p_vaddr: uint32 or uint64,
  p_paddr: uint32 or uint64,
  p_filesz: uint32 or uint64,
  p_memsz: uint32 or uint64,
  p_flags: uint32,
  p_align: uint32 or uint64
}

define type SectionHeader {
  sh_name: uint32,
  sh_type: uint32,
  sh_flags: uint32,
  sh_addr: uint32 or uint64,
  sh_offset: uint32 or uint64,
  sh_size: uint32 or uint64,
  sh_link: uint32,
  sh_info: uint32,
  sh_addralign: uint32 or uint64,
  sh_entsize: uint32
}

define type Symbol {
  st_name: uint32,
  st_value: uint32 or uint64,
  st_size: uint32 or uint64,
  st_info: byte,
  st_other: byte,
  st_shndx: uint16
}

define type Dynamic {
  d_tag: uint32,
  d_val: uint32 or uint64,
  d_ptr: uint32 or uint64
}

define type ElfFile {
  header: ElfHeader,
  program_headers: array[header.e_phnum] of ProgramHeader,
  section_headers: array[header.e_shnum] of SectionHeader,
  sections: array[header.e_shnum] of (
    { sh_type == 1, bytes[sh_size] },
    { sh_type == 2, bytes[sh_size] },
    { sh_type == 3, array[sh_size / 16] of Symbol },
    { sh_type == 4, bytes[sh_size] }
  ),
  dynamic: array[header.e_shnum] of (
    { d_tag == 1, uint32 },
    { d_tag == 2, bytes[d_val] },
    { d_tag == 3, Dynamic },
    { d_tag == 4, Dynamic }
  )
}

root type ElfFile