meta:
  id: elf
  file-extension: elf
  endian: le

seq:
  - id: e_ident
    type: str
    size: 16
    doc: ELF identifier (first 16 bytes, starting with 0x7F 'E' 'L' 'F')
  - id: e_type
    type: uint16
    doc: Object file type
  - id: e_machine
    type: uint16
    doc: Machine architecture
  - id: e_version
    type: uint32
    doc: ELF format version
  - id: e_entry
    type: uint64
    doc: Virtual address of entry point
  - id: e_phoff
    type: uint64
    doc: Offset of program headers
  - id: e_shoff
    type: uint64
    doc: Offset of section headers
  - id: e_flags
    type: uint32
    doc: Processor-specific flags
  - id: e_ehsize
    type: uint16
    doc: Size of this ELF header
  - id: e_phentsize
    type: uint16
    doc: Size of a program header
  - id: e_phnum
    type: uint16
    doc: Number of program headers
  - id: e_shentsize
    type: uint16
    doc: Size of a section header
  - id: e_shnum
    type: uint16
    doc: Number of section headers
  - id: e_shstrndx
    type: uint16
    doc: Index of section name string table

types:
  program_header:
    seq:
      - id: p_type
        type: uint32
        doc: Type of segment
      - id: p_flags
        type: uint32
        doc: Segment flags
      - id: p_offset
        type: uint64
        doc: Offset of segment in file
      - id: p_vaddr
        type: uint64
        doc: Virtual address of segment
      - id: p_paddr
        type: uint64
        doc: Physical address of segment (not used)
      - id: p_filesz
        type: uint64
        doc: Size of segment in file
      - id: p_memsz
        type: uint64
        doc: Size of segment in memory
      - id: p_align
        type: uint64
        doc: Alignment of segment

  section_header:
    seq:
      - id: sh_name
        type: uint32
        doc: Index of section name in string table
      - id: sh_type
        type: uint32
        doc: Type of section
      - id: sh_flags
        type: uint64
        doc: Section flags
      - id: sh_addr
        type: uint64
        doc: Virtual address of section
      - id: sh_offset
        type: uint64
        doc: Offset of section in file
      - id: sh_size
        type: uint64
        doc: Size of section
      - id: sh_link
        type: uint32
        doc: Index of related section
      - id: sh_info
        type: uint32
        doc: Additional section information
      - id: sh_addralign
        type: uint64
        doc: Alignment of section
      - id: sh_entsize
        type: uint64
        doc: Size of each entry in section

instances:
  program_headers:
    type: program_header
    repeat: e_phnum
    doc: Array of program headers
  section_headers:
    type: section_header
    repeat: e_shnum
    doc: Array of section headers