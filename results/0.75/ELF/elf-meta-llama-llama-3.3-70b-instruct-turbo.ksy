meta:
  id: elf
  file-extension: elf
  endian: le

seq:
  - id: e_ident
    type: str
    size: 16
    encoding: ASCII
  - id: e_type
    type: uint16
  - id: e_machine
    type: uint16
  - id: e_version
    type: uint32
  - id: e_entry
    type: uint64
  - id: e_phoff
    type: uint64
  - id: e_shoff
    type: uint64
  - id: e_flags
    type: uint32
  - id: e_ehsize
    type: uint16
  - id: e_phentsize
    type: uint16
  - id: e_phnum
    type: uint16
  - id: e_shentsize
    type: uint16
  - id: e_shnum
    type: uint16
  - id: e_shstrndx
    type: uint16

types:
  program_header:
    seq:
      - id: p_type
        type: uint32
      - id: p_flags
        type: uint32
      - id: p_offset
        type: uint64
      - id: p_vaddr
        type: uint64
      - id: p_paddr
        type: uint64
      - id: p_filesz
        type: uint64
      - id: p_memsz
        type: uint64
      - id: p_align
        type: uint64

  section_header:
    seq:
      - id: sh_name
        type: uint32
      - id: sh_type
        type: uint32
      - id: sh_flags
        type: uint64
      - id: sh_addr
        type: uint64
      - id: sh_offset
        type: uint64
      - id: sh_size
        type: uint64
      - id: sh_link
        type: uint32
      - id: sh_info
        type: uint32
      - id: sh_addralign
        type: uint64
      - id: sh_entsize
        type: uint64