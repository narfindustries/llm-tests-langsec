meta:
  id: elf
  title: Executable and Linkable Format
  file-extension: elf
  xref:
    wikidata: Q178798
  license: CC0-1.0
  endian: le
  imports:
    - elf_header
    - elf_section_header
    - elf_program_header

seq:
  - id: header
    type: elf_header

  - id: program_headers
    type: elf_program_header
    repeat: expr
    repeat-expr: header.e_phnum

  - id: section_headers
    type: elf_section_header
    repeat: expr
    repeat-expr: header.e_shnum

types:
  elf_header:
    seq:
      - id: e_ident
        type: e_ident
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

  e_ident:
    seq:
      - id: magic
        contents: [0x7f, 0x45, 0x4c, 0x46]
      - id: class
        type: u1
      - id: data
        type: u1
      - id: version
        type: u1
      - id: os_abi
        type: u1
      - id: abi_version
        type: u1
      - id: pad
        size: 7

  elf_program_header:
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

  elf_section_header:
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