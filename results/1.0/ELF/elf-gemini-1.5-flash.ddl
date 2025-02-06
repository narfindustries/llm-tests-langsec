data ElfHeader = ElfHeader {
  e_ident :: Array 16 UInt8
  e_type :: UInt16
  e_machine :: UInt16
  e_version :: UInt32
  e_entry :: UInt64
  e_phoff :: UInt64
  e_shoff :: UInt64
  e_flags :: UInt32
  e_ehsize :: UInt16
  e_phentsize :: UInt16
  e_phnum :: UInt16
  e_shentsize :: UInt16
  e_shnum :: UInt16
  e_shstrndx :: UInt16
}

data ElfProgramHeader = ElfProgramHeader {
  p_type :: UInt32
  p_flags :: UInt32
  p_offset :: UInt64
  p_vaddr :: UInt64
  p_paddr :: UInt64
  p_filesz :: UInt64
  p_memsz :: UInt64
  p_align :: UInt64
}

data ElfSectionHeader = ElfSectionHeader {
  sh_name :: UInt32
  sh_type :: UInt32
  sh_flags :: UInt64
  sh_addr :: UInt64
  sh_offset :: UInt64
  sh_size :: UInt64
  sh_link :: UInt32
  sh_info :: UInt32
  sh_addralign :: UInt64
  sh_entsize :: UInt64
}

data ElfSymbol = ElfSymbol {
  st_name :: UInt32
  st_value :: UInt64
  st_size :: UInt64
  st_info :: UInt8
  st_other :: UInt8
  st_shndx :: UInt16
}

data ElfFile = ElfFile {
  header :: ElfHeader
  program_headers :: Array (header.e_phnum) ElfProgramHeader
  section_headers :: Array (header.e_shnum) ElfSectionHeader
  sections :: Array (header.e_shnum) (Array 0 UInt8) -- Placeholder for section data; size is variable and needs dynamic handling
  symbols :: Maybe (Array 0 ElfSymbol) -- Symbol table is optional; placeholder for variable size
  dynamic_section :: Maybe (Array 0 UInt8) -- Highly variable; placeholder
}
