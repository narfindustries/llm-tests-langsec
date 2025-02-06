meta:
  id: elf64
  endian: le
types:
  Elf64_Ehdr:
    seq:
      - id: e_ident
        type: u1
        size: 16
      - id: e_type
        type: u2
      - id: e_machine
        type: u2
      - id: e_version
        type: u4
      - id: e_entry
        type: u8
      - id: e_phoff
        type: u8
      - id: e_shoff
        type: u8
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
  Elf64_Phdr:
    seq:
      - id: p_type
        type: u4
      - id: p_flags
        type: u4
      - id: p_offset
        type: u8
      - id: p_vaddr
        type: u8
      - id: p_paddr
        type: u8
      - id: p_filesz
        type: u8
      - id: p_memsz
        type: u8
      - id: p_align
        type: u8
  Elf64_Shdr:
    seq:
      - id: sh_name
        type: u4
      - id: sh_type
        type: u4
      - id: sh_flags
        type: u8
      - id: sh_addr
        type: u8
      - id: sh_offset
        type: u8
      - id: sh_size
        type: u8
      - id: sh_link
        type: u4
      - id: sh_info
        type: u4
      - id: sh_addralign
        type: u8
      - id: sh_entsize
        type: u8

root:
  seq:
    - id: header
      type: Elf64_Ehdr
    - id: program_headers
      type: Elf64_Phdr
      repeat: expr
      repeat-expr: header.e_phnum
    - id: section_headers
      type: Elf64_Shdr
      repeat: expr
      repeat-expr: header.e_shnum

