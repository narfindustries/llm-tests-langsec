format elf {
  elf_header: ElfHeader,
  program_headers: array of ProgramHeader,
  section_headers: array of SectionHeader,
  symbol_table: array of SymbolTableEntry,
  dynamic_section: array of DynamicSectionEntry,
  relocation_entries: array of RelocationEntry
}

format elf64 {
  elf_header: ElfHeader64,
  program_headers: array of ProgramHeader64,
  section_headers: array of SectionHeader64,
  symbol_table: array of SymbolTableEntry64,
  dynamic_section: array of DynamicSectionEntry64,
  relocation_entries: array of RelocationEntry64
}

type ElfHeader {
  ei_mag0: uint8,
  ei_mag1: uint8,
  ei_mag2: uint8,
  ei_mag3: uint8,
  ei_class: uint8,
  ei_data: uint8,
  ei_version: uint8,
  ei_osabi: uint8,
  ei_abi: uint8,
  ei_pad: array[7] of uint8,
  e_type: uint16,
  e_machine: uint16,
  e_version: uint32,
  e_entry: uint32,
  e_phoff: uint32,
  e_shoff: uint32,
  e_flags: uint32,
  e_ehsize: uint16,
  e_phentsize: uint16,
  e_phnum: uint16,
  e_shentsize: uint16,
  e_shnum: uint16,
  e_shstrndx: uint16
}

type ProgramHeader {
  p_type: uint32,
  p_offset: uint32,
  p_vaddr: uint32,
  p_paddr: uint32,
  p_filesz: uint32,
  p_memsz: uint32,
  p_flags: uint32,
  p_align: uint32
}

type SectionHeader {
  sh_name: uint32,
  sh_type: uint32,
  sh_flags: uint32,
  sh_addr: uint32,
  sh_offset: uint32,
  sh_size: uint32,
  sh_link: uint32,
  sh_info: uint32,
  sh_addralign: uint32,
  sh_entsize: uint32
}

type SymbolTableEntry {
  st_name: uint32,
  st_value: uint32,
  st_size: uint32,
  st_info: uint8,
  st_other: uint8,
  st_shndx: uint16
}

type DynamicSectionEntry {
  d_tag: uint32,
  d_val: uint32
}

type RelocationEntry {
  r_offset: uint32,
  r_info: uint32,
  r_addend: uint32
}

type ElfHeader64 {
  ei_mag0: uint8,
  ei_mag1: uint8,
  ei_mag2: uint8,
  ei_mag3: uint8,
  ei_class: uint8,
  ei_data: uint8,
  ei_version: uint8,
  ei_osabi: uint8,
  ei_abi: uint8,
  ei_pad: array[7] of uint8,
  e_type: uint16,
  e_machine: uint16,
  e_version: uint32,
  e_entry: uint64,
  e_phoff: uint64,
  e_shoff: uint64,
  e_flags: uint32,
  e_ehsize: uint16,
  e_phentsize: uint16,
  e_phnum: uint16,
  e_shentsize: uint16,
  e_shnum: uint16,
  e_shstrndx: uint16
}

type ProgramHeader64 {
  p_type: uint32,
  p_flags: uint32,
  p_offset: uint64,
  p_vaddr: uint64,
  p_paddr: uint64,
  p_filesz: uint64,
  p_memsz: uint64,
  p_align: uint64
}

type SectionHeader64 {
  sh_name: uint32,
  sh_type: uint32,
  sh_flags: uint64,
  sh_addr: uint64,
  sh_offset: uint64,
  sh_size: uint64,
  sh_link: uint32,
  sh_info: uint32,
  sh_addralign: uint64,
  sh_entsize: uint64
}

type SymbolTableEntry64 {
  st_name: uint32,
  st_info: uint8,
  st_other: uint8,
  st_shndx: uint16,
  st_value: uint64,
  st_size: uint64
}

type DynamicSectionEntry64 {
  d_tag: uint64,
  d_val: uint64
}

type RelocationEntry64 {
  r_offset: uint64,
  r_info: uint64,
  r_addend: uint64
}