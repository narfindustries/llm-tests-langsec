types:
  elf_header:
    seq:
      - id: e_ident
        type: u1
        count: 16
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

  program_header:
    seq:
      - id: p_type
        type: u4
      - id: p_flags
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

  elf_file:
    seq:
      - id: header
        type: elf_header
      - id: program_headers
        type: program_header
        repeat: expr(header.e_phnum)
      - id: section_headers
        type: section_header
        repeat: expr(header.e_shnum)
      - id: section_string_table_data
        type: u1
        size: expr(section_headers[header.e_shstrndx].sh_size)
        if: header.e_shstrndx != -1
      - id: section_data
        type: u1
        repeat: expr(header.e_shnum)

