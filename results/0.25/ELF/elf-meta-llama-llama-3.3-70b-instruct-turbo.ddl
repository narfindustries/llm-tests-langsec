elf: 
  magic: bytes(4) = "7fELF"
  class: uint8 = 1 | uint8 = 2
  data: uint8 = 1 | uint8 = 2
  osabi: uint8
  abiversion: uint8
  pad: bytes(7)
  type: uint16 = 1 | uint16 = 2 | uint16 = 3 | uint16 = 4
  machine: uint16
  version: uint32 = 1
  entry: uint32 | uint64
  phoff: uint32 | uint64
  shoff: uint32 | uint64
  flags: uint32
  ehsize: uint16 = 52 | uint16 = 64
  phentsize: uint16 = 32 | uint16 = 56
  phnum: uint16
  shentsize: uint16 = 40 | uint16 = 64
  shnum: uint16
  shstrndx: uint16

program_header: 
  type: uint32 = 0 | uint32 = 1 | uint32 = 2 | uint32 = 3 | uint32 = 4 | uint32 = 5 | uint32 = 6
  offset: uint32 | uint64
  vaddr: uint32 | uint64
  paddr: uint32 | uint64
  filesz: uint32 | uint64
  memsz: uint32 | uint64
  flags: uint32
  align: uint32 | uint64

section_header: 
  name: uint32
  type: uint32 = 0 | uint32 = 1 | uint32 = 2 | uint32 = 3 | uint32 = 4 | uint32 = 5 | uint32 = 6 | uint32 = 7 | uint32 = 8 | uint32 = 9 | uint32 = 10 | uint32 = 11 | uint32 = 12 | uint32 = 13 | uint32 = 14 | uint32 = 15 | uint32 = 16 | uint32 = 17 | uint32 = 18 | uint32 = 19 | uint32 = 20 | uint32 = 21
  flags: uint32
  addr: uint32 | uint64
  offset: uint32 | uint64
  size: uint32 | uint64
  link: uint32
  info: uint32
  addralign: uint32 | uint64
  entsize: uint32 | uint64

symbol_table: 
  name: uint32
  value: uint32 | uint64
  size: uint32 | uint64
  info: byte
  other: byte
  shndx: uint16

relocation: 
  offset: uint32 | uint64
  info: uint32
  addend: uint32 | uint64

elf_file: 
  header: elf
  program_headers: array[header.phnum] of program_header @ header.phoff
  section_headers: array[header.shnum] of section_header @ header.shoff
  symbol_tables: array of symbol_table
  relocations: array of relocation