meta:
  id: elf
  endian: be
types:
  program_header:
    seq:
      - id: p_type
        type: u4
      - id: p_offset
        type: u4
      - id: p_vaddr
        type: u4
      - id: p_paddr
        type: u4
      - id: p_filesz
        type: u4
      - id: p_memsz
        type: u4
      - id: p_flags
        type: u4
      - id: p_align
        type: u4
  section_header:
    seq:
      - id: sh_name
        type: u4
      - id: sh_type
        type: u4
      - id: sh_flags
        type: u4
      - id: sh_addr
        type: u4
      - id: sh_offset
        type: u4
      - id: sh_size
        type: u4
      - id: sh_link
        type: u4
      - id: sh_info
        type: u4
      - id: sh_addralign
        type: u4
      - id: sh_entsize
        type: u4
seq:
  - id: e_ident
    type: bytes
    size: 16
  - id: e_type
    type: u2
  - id: e_machine
    type: u2
  - id: e_version
    type: u4
  - id: e_entry
    type: u4
  - id: e_phoff
    type: u4
  - id: e_shoff
    type: u4
  - id: e_flags
    type: u4
  - id: e_ehsize
    type: u2
  - id: e_phentsize
    type: u2
  - id: e_phnum
    type: u2
  - id: e_shentsize
    type: u2
  - id: e_shnum
    type: u2
  - id: e_shstrndx
    type: u2
  - id: program_headers
    type: seq
    size: e_phnum
    type: program_header
  - id: section_headers
    type: seq
    size: e_shnum
    type: section_header
  - id: section_string_table
    type: str
    size: section_headers[e_shstrndx].sh_size
    offset: section_headers[e_shstrndx].sh_offset

