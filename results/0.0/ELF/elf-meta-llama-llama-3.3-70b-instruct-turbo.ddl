elf: 
  magic: bytes(4) = 0x7F, 'E', 'L', 'F'
  e_ident: bytes(16)
  e_type: uint16
  e_machine: uint16
  e_version: uint32
  e_entry: uint32 | uint64
  e_phoff: uint32 | uint64
  e_shoff: uint32 | uint64
  e_flags: uint32
  e_ehsize: uint16
  e_phentsize: uint16
  e_phnum: uint16
  e_shentsize: uint16
  e_shnum: uint16
  e_shstrndx: uint16

program_header: 
  p_type: uint32
  p_offset: uint32 | uint64
  p_vaddr: uint32 | uint64
  p_paddr: uint32 | uint64
  p_filesz: uint32 | uint64
  p_memsz: uint32 | uint64
  p_flags: uint32
  p_align: uint32 | uint64

section_header: 
  sh_name: uint32
  sh_type: uint32
  sh_flags: uint32
  sh_addr: uint32 | uint64
  sh_offset: uint32 | uint64
  sh_size: uint32 | uint64
  sh_link: uint32
  sh_info: uint32
  sh_addralign: uint32 | uint64
  sh_entsize: uint32

symbol_table: 
  st_name: uint32
  st_value: uint32 | uint64
  st_size: uint32 | uint64
  st_info: byte
  st_other: byte
  st_shndx: uint16

relocation: 
  r_offset: uint32 | uint64
  r_info: uint32
  r_addend: int32

dynamic_section: 
  d_tag: uint32
  d_un: uint32 | uint64

elf_file: 
  header: elf
  program_headers: array(program_header) @ header.e_phoff
  section_headers: array(section_header) @ header.e_shoff
  symbol_tables: array(symbol_table)
  relocations: array(relocation)
  dynamic_sections: array(dynamic_section)